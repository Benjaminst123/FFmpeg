/*
 * Copyright (c) 2016 Benjamin Steffes <benjaminst123@gmail.com>
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/**
 * @file vdpau filter.
 */

#include <vdpau/vdpau.h>
#include <vdpau/vdpau_x11.h>

#include <X11/Xlib.h>

#include "libavutil/avstring.h"
#include "libavutil/imgutils.h"
#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"
#include "libavutil/buffer.h"
#include "libavutil/hwcontext.h"
#include "libavutil/hwcontext_vdpau.h"
#include "libavutil/parseutils.h"
#include "avfilter.h"
#include "formats.h"
#include "internal.h"
#include "video.h"
#include "framesync.h"

#define MAX_FUTURE_FRAMES 3
#define MAX_PAST_FRAMES 3

#define MAX_SUPPORTED_FEATURES 4
#define MAX_SUPPORTED_ATTRIBUTES 4

enum DeinterlaceMethode {
    NONE,
    BOB,
    TEMPORAL,
    TEMPORAL_SPATIAL
};

typedef struct {
    VdpGetProcAddress *get_proc_address;

    VdpVideoMixerCreate *vdpVideoMixerCreate;
    VdpVideoMixerSetFeatureEnables *vdpVideoMixerSetFeatureEnables;

    VdpVideoMixerQueryFeatureSupport *vdpVideoMixerQueryFeatureSupport;

    VdpVideoMixerSetAttributeValues *vdpVideoMixerSetAttributeValues;

    VdpVideoSurfaceCreate *vdpVideoSurfaceCreate;
    VdpVideoSurfaceDestroy *vdpVideoSurfaceDestroy;
    VdpOutputSurfaceCreate *vdpOutputSurfaceCreate;
    VdpOutputSurfaceDestroy *vdpOutputSurfaceDestroy;

    VdpVideoSurfacePutBitsYCbCr *vdpVideoSurfacePutBitsYCbCr;
    VdpOutputSurfaceGetBitsNative *vdpOutputSurfaceGetBitsNative;
    VdpOutputSurfacePutBitsNative *vdpOutputSurfacePutBitsNative;

    VdpVideoMixerDestroy *vdpVideoMixerDestroy;
    VdpVideoMixerRender *vdpVideoMixerRender;
    VdpDeviceDestroy *vdpDeviceDestroy;

    VdpGetErrorString *vdpGetErrorString;
} VdpauFunctions;

typedef struct {
    const AVClass *class;

    Display *display;
    char *display_name;

    VdpDevice device;
    VdpauFunctions vdpaufuncs;

    AVBufferRef *hwdevice;
    AVBufferRef *hwframe;

    VdpVideoMixer mixer;
    VdpVideoSurface   videosSurface;
    VdpOutputSurface  outputSurface;
    VdpOutputSurface  overlaySurface;

    VdpVideoMixerAttribute attributes[MAX_SUPPORTED_ATTRIBUTES];
    const void *attribute_values[MAX_SUPPORTED_ATTRIBUTES];
    int attribute_cnt;

    VdpVideoMixerFeature features[MAX_SUPPORTED_FEATURES];
    int feature_cnt;

    AVFrame *cur_frame;

    int future_frames_cnt;
    AVFrame *future_frames[MAX_FUTURE_FRAMES];

    int past_frames_cnt;
    AVFrame *past_frames[MAX_PAST_FRAMES];

    int eof;

    float noise_reduction;
    float sharpness;
    int deinterlacer;
    int double_framerate;

    int scaling_quality;
    int w;
    int h;

    char* size_str;

    FFFrameSync fs;
    int use_overlay;
    int overlay_x;
    int overlay_y;
} VdpauContext;

#define OFFSET(x) offsetof(VdpauContext, x)
#define FLAGS AV_OPT_FLAG_FILTERING_PARAM|AV_OPT_FLAG_VIDEO_PARAM

static const AVOption vdpau_options[] = {
    { "noise_reduction", "set interlacing threshold", OFFSET(noise_reduction),   AV_OPT_TYPE_FLOAT, {.dbl = 0}, 0, 1.0, FLAGS },
    { "sharpness", "set progressive threshold", OFFSET(sharpness), AV_OPT_TYPE_FLOAT, {.dbl = 0},  -1, 1, FLAGS },
    { "deinterlacer", "set deinterlacing methode", OFFSET(deinterlacer), AV_OPT_TYPE_INT, {.i64 = 0},  0, TEMPORAL_SPATIAL, FLAGS, "deinterlacer"},
        {"bob", "bob deinterlacer", 0, AV_OPT_TYPE_CONST, {.i64=BOB}, 0, 0, FLAGS, "deinterlacer"},
        {"temporal", "temporal deinterlacer", 0, AV_OPT_TYPE_CONST, {.i64=TEMPORAL}, 0, 0, FLAGS, "deinterlacer"},
        {"temporal_spatial", "temporal spatial deinterlacer", 0, AV_OPT_TYPE_CONST, {.i64=TEMPORAL_SPATIAL}, 0, 0, FLAGS, "deinterlacer"},
    { "double_framerate", "double framerate", OFFSET(double_framerate), AV_OPT_TYPE_INT, {.i64 = 0}, 0, 1, FLAGS },
    { "size","set video size", OFFSET(size_str), AV_OPT_TYPE_STRING, {.str = NULL}, 0, FLAGS },
    { "scaling_quality", "set scaling quality", OFFSET(scaling_quality), AV_OPT_TYPE_INT, {.i64 = 1}, 1, 9, FLAGS },
    { "overlay_x", "set overlay x", OFFSET(overlay_x), AV_OPT_TYPE_INT, {.i64=-1}, -1, 8000, FLAGS },
    { "overlay_y", "set overlay x", OFFSET(overlay_y), AV_OPT_TYPE_INT, {.i64=-1}, -1, 8000, FLAGS },
    { NULL }
};

AVFILTER_DEFINE_CLASS(vdpau);

#define GET_CALLBACK(id, result)                                                \
do {                                                                            \
    void *tmp;                                                                  \
    int err;                                                                    \
    err = vdpauFuncs->get_proc_address(s->device, id, &tmp);                    \
    if (err != VDP_STATUS_OK) {                                                 \
        av_log(ctx, AV_LOG_ERROR, "Error getting the " #id " callback.\n");     \
        return -1;                                                              \
    }                                                                           \
    result = tmp;                                                               \
} while (0)

static int filter_frame_vdpau(AVFilterLink *inlink, AVFrame *frame, AVFrame *overlay);
static int filter_frame(AVFilterLink *inlink, AVFrame *frame);

static int check_support(VdpauContext *s, VdpVideoMixerFeature feature, const char* name) {
    VdpauFunctions *vdpauFuncs = &s->vdpaufuncs;
    VdpBool is_supported;
    VdpStatus ret;

    ret = vdpauFuncs->vdpVideoMixerQueryFeatureSupport(s->device, feature, &is_supported);
    if (ret != VDP_STATUS_OK) {
        av_log(NULL, AV_LOG_ERROR, "VDPAU mixer query feature %s failed: %s\n",
                s->display_name, vdpauFuncs->vdpGetErrorString(ret));
        return AVERROR_UNKNOWN;
    }
    if (is_supported == VDP_FALSE) {
        av_log(NULL, AV_LOG_ERROR, "%s is not supported!\n", name);
        return AVERROR_UNKNOWN;
    }

    return 0;
}

static int init_supported_features(VdpauContext *s) {
    int ret;
    if (s->sharpness != 0) {
        ret = check_support(s, VDP_VIDEO_MIXER_FEATURE_SHARPNESS, "sharpness change");
        if (ret < 0) {
            return ret;
        }
        s->features[s->feature_cnt++] = VDP_VIDEO_MIXER_FEATURE_SHARPNESS;

        s->attributes[s->attribute_cnt] = VDP_VIDEO_MIXER_ATTRIBUTE_SHARPNESS_LEVEL;
        s->attribute_values[s->attribute_cnt++] = &s->sharpness;
    }

    if (s->noise_reduction != 0) {
        ret = check_support(s, VDP_VIDEO_MIXER_FEATURE_NOISE_REDUCTION, "noise reduction");
        if (ret < 0) {
            return ret;
        }
        s->features[s->feature_cnt++] = VDP_VIDEO_MIXER_FEATURE_NOISE_REDUCTION;
        s->attributes[s->attribute_cnt] = VDP_VIDEO_MIXER_ATTRIBUTE_NOISE_REDUCTION_LEVEL;
        s->attribute_values[s->attribute_cnt++] = &s->noise_reduction;
    }

    if (s->deinterlacer == TEMPORAL) {
        ret = check_support(s, VDP_VIDEO_MIXER_FEATURE_DEINTERLACE_TEMPORAL, "temporal deinterlacer");
        if (ret < 0) {
            return ret;
        }
        s->features[s->feature_cnt++] = VDP_VIDEO_MIXER_FEATURE_DEINTERLACE_TEMPORAL;
    }

    if (s->deinterlacer == TEMPORAL_SPATIAL) {
        ret = check_support(s, VDP_VIDEO_MIXER_FEATURE_DEINTERLACE_TEMPORAL, "temporal spatial deinterlacer");
        if (ret < 0) {
            return ret;
        }
        s->features[s->feature_cnt++] = VDP_VIDEO_MIXER_FEATURE_DEINTERLACE_TEMPORAL_SPATIAL;
    }

    if (s->size_str != NULL) {
        int scaling = VDP_VIDEO_MIXER_FEATURE_HIGH_QUALITY_SCALING_L1 + s->scaling_quality - 1;
        ret = check_support(s, scaling, "high quality scaling");
        if (ret < 0) {
            return ret;
        }
        s->features[s->feature_cnt++] = scaling;
    }

    return 0;
}

static int process_frame(FFFrameSync *fs) {
    AVFilterContext *ctx = fs->parent;
    VdpauContext *s = fs->opaque;
    AVFrame *mainpic = NULL, *secondpic = NULL;
    int ret = 0;

    if ((ret = ff_framesync_get_frame(&s->fs, 0, &mainpic,   1)) < 0 ||
        (ret = ff_framesync_get_frame(&s->fs, 1, &secondpic, 0)) < 0) {
        av_frame_free(&mainpic);
        return ret;
    }
    av_assert0(mainpic);
    mainpic->pts = av_rescale_q(s->fs.pts, s->fs.time_base, ctx->outputs[0]->time_base);
    if (secondpic && !ctx->is_disabled) {
        //mainpic = s->process(ctx, mainpic, secondpic);
        return filter_frame_vdpau(ctx->inputs[0], mainpic, secondpic);

    }

    //ret = ff_filter_frame(ctx->outputs[0], mainpic);
    av_assert1(ret != AVERROR(EAGAIN));
    return ret;
}

static int init_framesync(AVFilterContext *ctx, int repeatlast, int shortest)
{
    int ret;
    VdpauContext *s = ctx->priv;
    FFFrameSyncIn *in;

    if ((ret = ff_framesync_init(&s->fs, ctx, 2)) < 0) {
        return ret;
    }

    in = s->fs.in;
    s->fs.opaque = s;
    s->fs.on_event = process_frame;
    in[0].time_base = ctx->inputs[0]->time_base;
    in[1].time_base = ctx->inputs[1]->time_base;
    in[0].sync   = 2;
    in[0].before = EXT_STOP;
    in[0].after  = EXT_INFINITY;
    in[1].sync   = 1;
    in[1].before = EXT_NULL;
    in[1].after  = EXT_INFINITY;

    if (shortest)
        in[0].after = in[1].after = EXT_STOP;
    if (repeatlast) {
        in[1].after = EXT_NULL;
        in[1].sync  = 0;
    }

    return ff_framesync_configure(&s->fs);
}

static int config_overlay_input(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    VdpauContext *s = inlink->dst->priv;
    VdpauFunctions *vdpauFuncs = &s->vdpaufuncs;
    VdpStatus ret;

    ret = vdpauFuncs->vdpOutputSurfaceCreate(s->device, VDP_RGBA_FORMAT_B8G8R8A8, inlink->w, inlink->h, &s->overlaySurface);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU overlay surface create on X11 display %s failed: %s\n",
                s->display_name, vdpauFuncs->vdpGetErrorString(ret));
        return -1;
    }

    return 0;
}

static av_cold int init(AVFilterContext *ctx)
{
    VdpauContext *s = ctx->priv;
    VdpauFunctions *vdpauFuncs =  &s->vdpaufuncs;
    int ret;
    VdpStatus status;
    AVHWDeviceContext *device_ctx;
    AVVDPAUDeviceContext *device_hwctx;
    int i = 0;


    s->display = 0;
    s->device  = 0;
    s->mixer   = 0;
    s->feature_cnt = 0;

    //parse scaling options
    if (s->size_str) {
        char buf[32];
        if ((ret = av_parse_video_size(&s->w, &s->h, s->size_str)) < 0) {
            av_log(ctx, AV_LOG_ERROR, "Invalid size '%s'\n", s->size_str);
            return ret;
        }
        snprintf(buf, sizeof(buf)-1, "%d", s->w);
        av_opt_set(s, "w", buf, 0);
        snprintf(buf, sizeof(buf)-1, "%d", s->h);
        av_opt_set(s, "h", buf, 0);
    }

    //open display
    s->display = XOpenDisplay(0);
    if (!s->display) {
        av_log(ctx, AV_LOG_ERROR, "Cannot open the X11 display %s.\n",
               XDisplayName(0));
        return -1;
    }
    s->display_name = XDisplayString(s->display);

    //setup vdpau device
    status = vdp_device_create_x11(s->display, XDefaultScreen(s->display),
                                &s->device, &vdpauFuncs->get_proc_address);
    if (status != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU device creation on X11 display %s failed.\n",
                XDisplayName(0));
        return AVERROR_UNKNOWN;
    }

    //allocate hardware context
    s->hwdevice = av_hwdevice_ctx_alloc(AV_HWDEVICE_TYPE_VDPAU);
    if (!s->hwdevice) {
        return AVERROR(ENOMEM);
    }

    device_ctx       = (AVHWDeviceContext*)s->hwdevice->data;
    device_ctx->free = NULL;

    device_hwctx = device_ctx->hwctx;
    device_hwctx->device = s->device;
    device_hwctx->get_proc_address = vdpauFuncs->get_proc_address;

    //init hardware device
    ret = av_hwdevice_ctx_init(s->hwdevice);
    if (ret < 0)
        return ret;

    //retrieve needed vdpau function pointer
    GET_CALLBACK(VDP_FUNC_ID_VIDEO_MIXER_CREATE, vdpauFuncs->vdpVideoMixerCreate);
    GET_CALLBACK(VDP_FUNC_ID_VIDEO_MIXER_SET_FEATURE_ENABLES, vdpauFuncs->vdpVideoMixerSetFeatureEnables);
    GET_CALLBACK(VDP_FUNC_ID_VIDEO_MIXER_DESTROY, vdpauFuncs->vdpVideoMixerDestroy);
    GET_CALLBACK(VDP_FUNC_ID_DEVICE_DESTROY, vdpauFuncs->vdpDeviceDestroy);
    GET_CALLBACK(VDP_FUNC_ID_VIDEO_SURFACE_CREATE, vdpauFuncs->vdpVideoSurfaceCreate);
    GET_CALLBACK(VDP_FUNC_ID_VIDEO_SURFACE_PUT_BITS_Y_CB_CR, vdpauFuncs->vdpVideoSurfacePutBitsYCbCr);
    GET_CALLBACK(VDP_FUNC_ID_OUTPUT_SURFACE_GET_BITS_NATIVE, vdpauFuncs->vdpOutputSurfaceGetBitsNative);
    GET_CALLBACK(VDP_FUNC_ID_OUTPUT_SURFACE_PUT_BITS_NATIVE, vdpauFuncs->vdpOutputSurfacePutBitsNative);
    GET_CALLBACK(VDP_FUNC_ID_OUTPUT_SURFACE_CREATE, vdpauFuncs->vdpOutputSurfaceCreate);
    GET_CALLBACK(VDP_FUNC_ID_DEVICE_DESTROY, vdpauFuncs->vdpDeviceDestroy);
    GET_CALLBACK(VDP_FUNC_ID_VIDEO_MIXER_DESTROY, vdpauFuncs->vdpVideoMixerDestroy);
    GET_CALLBACK(VDP_FUNC_ID_VIDEO_MIXER_RENDER, vdpauFuncs->vdpVideoMixerRender);
    GET_CALLBACK(VDP_FUNC_ID_OUTPUT_SURFACE_DESTROY, vdpauFuncs->vdpOutputSurfaceDestroy);
    GET_CALLBACK(VDP_FUNC_ID_VIDEO_SURFACE_DESTROY, vdpauFuncs->vdpVideoSurfaceDestroy);
    GET_CALLBACK(VDP_FUNC_ID_GET_ERROR_STRING, vdpauFuncs->vdpGetErrorString);
    GET_CALLBACK(VDP_FUNC_ID_VIDEO_MIXER_SET_ATTRIBUTE_VALUES, vdpauFuncs->vdpVideoMixerSetAttributeValues);
    GET_CALLBACK(VDP_FUNC_ID_VIDEO_MIXER_QUERY_FEATURE_SUPPORT, vdpauFuncs->vdpVideoMixerQueryFeatureSupport);

    //Check if features selected are supported
    ret = init_supported_features(s);
    if (ret < 0) {
        return ret;
    }

    s->future_frames_cnt = 0;
    s->past_frames_cnt   = 0;

    //TODO: memset
    s->cur_frame = NULL;
    for (i = 0; i < MAX_FUTURE_FRAMES; i++) {
        s->future_frames[i] = NULL;
    }
    for (i = 0; i < MAX_PAST_FRAMES; i++) {
        s->past_frames[i] = NULL;
    }

    //check overlay
    if ((s->overlay_x >= 0 && s->overlay_y < 0) || (s->overlay_x < 0 && s->overlay_y >= 0)) {
        av_log(ctx, AV_LOG_ERROR, "Invalid overlay coordinates!\n");
        return AVERROR(EINVAL);
    }

    s->use_overlay = 0;
    if (s->overlay_x >= 0) {
        //initialize overlay_input pad
        AVFilterPad overlay_pad = {
            .name          = "overlay",
            .type          = AVMEDIA_TYPE_VIDEO,
            .filter_frame  = filter_frame,
            .config_props  = config_overlay_input,
        };

        if ((ret = ff_insert_inpad(ctx, 0, &overlay_pad)) < 0) {
            return ret;
        }

        s->use_overlay = 1;
    }

    return 0;
}

static int query_formats(AVFilterContext *ctx)
{
    VdpauContext *s = ctx->priv;
    AVFilterFormats *in_formats;
    AVFilterFormats *out_formats;
    int ret;
    AVFilterLink *inlink  = ctx->inputs[0];
    AVFilterLink *outlink = ctx->outputs[0];
    static const enum AVPixelFormat pix_fmts_in[] = {
        AV_PIX_FMT_YUV420P,
        AV_PIX_FMT_NONE
    };
    static const enum AVPixelFormat pix_fmts_out[] = {
        AV_PIX_FMT_BGRA,
        AV_PIX_FMT_NONE
    };
    static const enum AVPixelFormat pix_fmt_overlay_in[] = {
        AV_PIX_FMT_BGRA,
        AV_PIX_FMT_NONE
    };

    in_formats = ff_make_format_list(pix_fmts_in);
    out_formats = ff_make_format_list(pix_fmts_out);
    if (!in_formats || !out_formats)
        return AVERROR(ENOMEM);

    ret = ff_formats_ref(in_formats, &inlink->out_formats);
    if (ret < 0)
        return ret;
    ret = ff_formats_ref(out_formats, &outlink->in_formats);
    if (ret < 0)
        return ret;

    if (s->use_overlay) {
        AVFilterLink *inlink_overlay  = ctx->inputs[1];
        if ((in_formats = ff_make_format_list(pix_fmt_overlay_in)) == 0) {
            return AVERROR(ENOMEM);
        }
        if ((ret = ff_formats_ref(in_formats, &inlink_overlay->out_formats)) < 0) {
            return ret;
        }

    }

    return 0;
}

static int config_input(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    VdpauContext *s = inlink->dst->priv;
    VdpauFunctions *vdpauFuncs = &s->vdpaufuncs;
    VdpStatus ret;
    VdpBool enables[MAX_SUPPORTED_FEATURES] = { VDP_TRUE };
    VdpVideoMixerParameter parameters[] = {
        VDP_VIDEO_MIXER_PARAMETER_VIDEO_SURFACE_WIDTH,
        VDP_VIDEO_MIXER_PARAMETER_VIDEO_SURFACE_HEIGHT,
        VDP_VIDEO_MIXER_PARAMETER_LAYERS,
    };
    int layers = (s->use_overlay) ? 1 : 0;
    const void *parameter_values[] = {
        &inlink->w,
        &inlink->h,
        &layers
    };

    ret = vdpauFuncs->vdpVideoMixerCreate(s->device, s->feature_cnt, s->features, 3, parameters, parameter_values, &s->mixer);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU mixer creation on X11 display %s failed: %s\n",
                s->display_name, vdpauFuncs->vdpGetErrorString(ret));
        return -1;
    }

    ret = vdpauFuncs->vdpVideoMixerSetFeatureEnables(s->mixer, s->feature_cnt, s->features, enables);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU mixer set feature on X11 display %s failed: %s\n",
                s->display_name, vdpauFuncs->vdpGetErrorString(ret));
        return -1;
    }

    ret = vdpauFuncs->vdpVideoMixerSetAttributeValues(s->mixer, s->attribute_cnt, s->attributes, s->attribute_values);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU video  mixer set attribute values on X11 display %s failed failed: %s\n",
                s->display_name, vdpauFuncs->vdpGetErrorString(ret));
        return -1;
    }

    ret = vdpauFuncs->vdpVideoSurfaceCreate(s->device, VDP_CHROMA_TYPE_420, inlink->w, inlink->h, &s->videosSurface);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU video surface create on X11 display %s failed failed: %s\n",
                s->display_name, vdpauFuncs->vdpGetErrorString(ret));
        return -1;
    }

    return 0;
}

static int config_output(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    VdpauContext *s = ctx->priv;
    VdpauFunctions *vdpauFuncs = &s->vdpaufuncs;
    const AVFilterLink *inlink = ctx->inputs[0];

    AVHWFramesContext *hwframe_ctx;
    int ret;

    av_buffer_unref(&s->hwframe);
    s->hwframe = av_hwframe_ctx_alloc(s->hwdevice);
    if (!s->hwframe)
        return AVERROR(ENOMEM);

    hwframe_ctx            = (AVHWFramesContext*)s->hwframe->data;
    hwframe_ctx->format    = AV_PIX_FMT_VDPAU;
    hwframe_ctx->sw_format = inlink->format;
    hwframe_ctx->width     = /*FFALIGN(*/inlink->w;//, 16);
    hwframe_ctx->height    = /*FFALIGN(*/inlink->h;//, 16);

    if (s->double_framerate) {
        outlink->time_base.num = ctx->inputs[0]->time_base.num;
        outlink->time_base.den = ctx->inputs[0]->time_base.den * 2;

        outlink->frame_rate = av_mul_q(ctx->inputs[0]->frame_rate, (AVRational){2, 1});
    }

    if (s->size_str == NULL) {
        s->w = inlink->w;
        s->h = inlink->h;
    }

    outlink->w = s->w;
    outlink->h = s->h;


    if (s->use_overlay) {
        if ((ret = init_framesync(ctx, 1, 1)) < 0) {
            return ret;
        }
    }

    ret = vdpauFuncs->vdpOutputSurfaceCreate(s->device, VDP_RGBA_FORMAT_B8G8R8A8, outlink->w, outlink->h, &s->outputSurface);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU output surface create on X11 display %s failed: %s\n",
                s->display_name, vdpauFuncs->vdpGetErrorString(ret));
        vdpauFuncs->vdpVideoSurfaceDestroy(s->videosSurface);
        return -1;
    }

    ret = av_hwframe_ctx_init(s->hwframe);
    if (ret < 0)
        return ret;

    return 0;
}

static int upload_frame_outputsurface(VdpauContext *s, AVFrame *frame, VdpOutputSurface surface) {
    VdpauFunctions *vdpauFuncs = &s->vdpaufuncs;
    VdpStatus ret;

    ret = vdpauFuncs->vdpOutputSurfacePutBitsNative(surface,
            (const void * const*)frame->data, frame->linesize, NULL);
    if (ret != VDP_STATUS_OK) {
        av_log(NULL, AV_LOG_ERROR, "VDPAU vdpOutputSurfacePutBitsNative on X11 display %s failed: %s\n",
               s->display_name, vdpauFuncs->vdpGetErrorString(ret));
        return AVERROR_UNKNOWN;
    }

    return 0;
}

static int upload_frame(AVBufferRef *hwframe, AVFrame *frame, AVFrame* src)
{
    int ret;
    ret = av_hwframe_get_buffer(hwframe, frame, 0);
    if (ret < 0) {
        return ret;
    }

    ret = av_hwframe_transfer_data(frame, src, 0);
    if (ret < 0) {
        return ret;
    }
    av_frame_copy_props(frame, src);

    return 0;
}

static void update_frames(VdpauContext *s, AVFrame* newFrame)
{
    AVFrame *oldFrame;
    int i;

    //free old frames
    oldFrame = s->past_frames[MAX_PAST_FRAMES -1];
    if (oldFrame != NULL) {
        av_frame_free(&oldFrame);
    }

    //update old frames
    for (i = MAX_PAST_FRAMES; i-- > 1;) {
        s->past_frames[i] = s->past_frames[i - 1];
    }
    s->past_frames[0] = s->cur_frame;

    s->cur_frame = s->future_frames[0];

    //update next frames
    for (i = 1; i < MAX_FUTURE_FRAMES; i++) {
        s->future_frames[i - 1] = s->future_frames[i];
    }
    s->future_frames[MAX_FUTURE_FRAMES - 1] = newFrame;
}

static void setup_past_surfaces(VdpVideoSurface pastVideoSurfaces[], AVFrame *past_frames[])
{
    int i;
    for (i = 0; i < MAX_PAST_FRAMES; i++) {
        if (past_frames[i] != NULL) {
            pastVideoSurfaces[i] = (VdpVideoSurface)past_frames[i]->data[3];
        } else {
            pastVideoSurfaces[i] = VDP_INVALID_HANDLE;
        }
    }
}

static void setup_future_surfaces(VdpVideoSurface futureVideoSurfaces[], AVFrame *future_frames[])
{
    int i;
    for (i = 0; i < MAX_FUTURE_FRAMES; i++) {
        if (future_frames[i] != NULL) {
            futureVideoSurfaces[i] = (VdpVideoSurface)future_frames[i]->data[3];
        } else {
            futureVideoSurfaces[i] = VDP_INVALID_HANDLE;
        }
    }
}

static int render_frame(AVFilterContext *ctx,
                         VdpVideoMixer mixer,
                         VdpVideoMixerPictureStructure picture_structure,
                         VdpVideoSurface *past,
                         VdpVideoSurface cur,
                         VdpVideoSurface *future,
                         VdpOutputSurface out,
                         int layer_count,
                         VdpLayer *layer)
{
    VdpauContext *s = ctx->priv;
    VdpauFunctions *vdpauFuncs = &s->vdpaufuncs;
    VdpStatus ret;
    VdpRect dest;

    dest = (VdpRect){0, 0, s->w, s->h};

    ret = vdpauFuncs->vdpVideoMixerRender(s->mixer,
                                          VDP_INVALID_HANDLE,
                                          NULL,
                                          picture_structure,
                                          MAX_PAST_FRAMES,
                                          past,
                                          cur,
                                          MAX_FUTURE_FRAMES,
                                          future,
                                          NULL,
                                          out,
                                          &dest,
                                          NULL,
                                          layer_count,
                                          layer);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU video mixer render on X11 display %s failed: %s\n",
                s->display_name, vdpauFuncs->vdpGetErrorString(ret));
        return AVERROR_UNKNOWN;
    }

    return 0;
}

static int filter_frame(AVFilterLink *inlink, AVFrame *frame) {
    AVFilterContext *ctx = inlink->dst;
    AVFilterLink *outlink = ctx->outputs[0];
    VdpauContext *s = ctx->priv;

    if (s->use_overlay && frame != NULL) {
        return ff_framesync_filter_frame(&s->fs, inlink, frame);
    } else {
        return filter_frame_vdpau(inlink, frame, NULL);
    }
}


static int filter_frame_vdpau(AVFilterLink *inlink, AVFrame *frame, AVFrame *overlay)
{
    AVFilterContext *ctx = inlink->dst;
    AVFilterLink *outlink = ctx->outputs[0];
    VdpauContext *s = ctx->priv;
    VdpauFunctions *vdpauFuncs = &s->vdpaufuncs;
    int ret;
    AVFrame *oFrame, *oFrame2;
    AVFrame *iFrame;
    VdpVideoSurface past_surfaces[MAX_PAST_FRAMES];
    VdpVideoSurface future_surfaces[MAX_FUTURE_FRAMES];
    VdpVideoMixerPictureStructure picture_structure;
    VdpLayer overlay_layer;
    VdpRect overlay_rect;
    VdpLayer *layers = NULL;
    int layer_count = 0;

    if (frame != NULL) {
        iFrame = av_frame_alloc();
        ret = upload_frame(s->hwframe, iFrame, frame);
        av_frame_free(&frame);
        if (ret < 0) {
            av_frame_free(&overlay);
            av_frame_free(&iFrame);
            return ret;
        }
    } else {
        iFrame = NULL;
    }

    if (s->future_frames[0] == NULL && s->cur_frame != 0) {
        s->eof = 1;
    }

    update_frames(s, iFrame);
    if (s->cur_frame == NULL) {
        return 0;
    }

    setup_past_surfaces(past_surfaces, s->past_frames);
    setup_future_surfaces(future_surfaces, s->future_frames);

    if (s->deinterlacer != NONE && s->cur_frame->interlaced_frame) {
        picture_structure = s->cur_frame->top_field_first ? VDP_VIDEO_MIXER_PICTURE_STRUCTURE_TOP_FIELD :
                VDP_VIDEO_MIXER_PICTURE_STRUCTURE_BOTTOM_FIELD;
    } else {
        picture_structure = VDP_VIDEO_MIXER_PICTURE_STRUCTURE_FRAME;
    }

    //handle overlay
    if (overlay != NULL) {
        ret = upload_frame_outputsurface(s, overlay, s->overlaySurface);
        //av_frame_free(&overlay);
        if (ret < 0) {
            return ret;
        }

        overlay_rect = (VdpRect){ 0, 0, s->w / 4, s->h / 4};

        overlay_layer.struct_version = VDP_LAYER_VERSION;
        overlay_layer.source_surface = s->overlaySurface;
        overlay_layer.source_rect    = NULL;
        overlay_layer.destination_rect = &overlay_rect;

        layers = &overlay_layer;
        layer_count = 1;
    }

    ret = render_frame(ctx, s->mixer, picture_structure, past_surfaces,
            (VdpVideoSurface)s->cur_frame->data[3], future_surfaces, s->outputSurface, layer_count, layers);
    if (ret < 0) {
        return ret;
    }

    //download result to frame
    oFrame = ff_get_video_buffer(outlink, outlink->w, outlink->h);
    av_frame_copy_props(oFrame, s->cur_frame);
    vdpauFuncs->vdpOutputSurfaceGetBitsNative(s->outputSurface, NULL, (void * const*)oFrame->data, oFrame->linesize);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU vdpOutputSurfaceGetBitsNative on X11 display %s failed: %s\n",
                s->display_name, vdpauFuncs->vdpGetErrorString(ret));
        return AVERROR_UNKNOWN;
    }


    if (s->double_framerate) {
        oFrame->pts *= 2;

        if (picture_structure == VDP_VIDEO_MIXER_PICTURE_STRUCTURE_TOP_FIELD) {
            picture_structure = VDP_VIDEO_MIXER_PICTURE_STRUCTURE_BOTTOM_FIELD;
        } else {
            picture_structure = VDP_VIDEO_MIXER_PICTURE_STRUCTURE_TOP_FIELD;
        }

        ret = render_frame(ctx, s->mixer, picture_structure, past_surfaces,
                (VdpVideoSurface)s->cur_frame->data[3], future_surfaces, s->outputSurface, layer_count, layers);
        if (ret < 0) {
            return ret;
        }


        //download result to frame
        oFrame2 = ff_get_video_buffer(outlink, outlink->w, outlink->h);
        av_frame_copy_props(oFrame2, s->cur_frame);
        if (s->future_frames[0] != NULL) {
            oFrame2->pts += s->future_frames[0]->pts;
        } else {
            oFrame2->pts *= 2;
        }

        vdpauFuncs->vdpOutputSurfaceGetBitsNative(s->outputSurface, NULL, (void * const*)oFrame2->data, oFrame2->linesize);
        if (ret != VDP_STATUS_OK) {
            av_log(ctx, AV_LOG_ERROR, "VDPAU vdpOutputSurfaceGetBitsNative on X11 display %s failed: %s\n",
                    s->display_name, vdpauFuncs->vdpGetErrorString(ret));
            return AVERROR_UNKNOWN;
        }
    }

    av_frame_free(&frame);
    ret = ff_filter_frame(outlink, oFrame);
    if (ret < 0) {
        return ret;
    }
    if (s->double_framerate) {
        ret = ff_filter_frame(outlink, oFrame2);
    }
    return ret;
}

static int request_frame(AVFilterLink *link)
{
    AVFilterContext *ctx = link->src;
    VdpauContext *s = ctx->priv;
    int ret;

    if (s->eof)
        return AVERROR_EOF;

    if (s->use_overlay) {
        ret = ff_framesync_request_frame(&s->fs, link);
    } else {
        ret = ff_request_frame(link->src->inputs[0]);
    }

    if (ret == AVERROR_EOF && s->cur_frame) {
        ret = filter_frame(link->src->inputs[0], NULL);
    }

    return ret;
}

static av_cold void uninit(AVFilterContext *ctx)
{
    VdpauContext *s = ctx->priv;
    VdpauFunctions *vdpauFuncs = &s->vdpaufuncs;

    if (s->use_overlay) {
        ff_framesync_uninit(&s->fs);
    }

    if (s->mixer) vdpauFuncs->vdpVideoMixerDestroy(s->mixer);
    if (s->device) vdpauFuncs->vdpDeviceDestroy(s->device);
    if (s->display) XCloseDisplay(s->display);
}

static const AVFilterPad vdpau_inputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
        .filter_frame  = filter_frame,
        .config_props  = config_input,
    },
    { NULL }
};

static const AVFilterPad vdpau_outputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
        .request_frame = request_frame,
        .config_props  = config_output,
    },
    { NULL }
};

AVFilter ff_vf_vdpau = {
    .name          = "vdpau",
    .description   = NULL_IF_CONFIG_SMALL("Apply vdpau filtering."),
    .priv_size     = sizeof(VdpauContext),
    .priv_class    = &vdpau_class,
    .init          = init,
    .uninit        = uninit,
    .query_formats = query_formats,
    .inputs        = vdpau_inputs,
    .outputs       = vdpau_outputs,
    .flags          = AVFILTER_FLAG_DYNAMIC_INPUTS
};
