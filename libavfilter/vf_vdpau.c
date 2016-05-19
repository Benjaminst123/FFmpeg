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
#include "avfilter.h"
#include "formats.h"
#include "internal.h"
#include "video.h"

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
    const AVClass *class;

    Display *display;
    char *display_name;

    AVBufferRef *hwdevice;
    AVBufferRef *hwframe;

    VdpDevice         device;
    VdpGetProcAddress *get_proc_address;

    VdpVideoMixer mixer;
    VdpVideoMixerCreate *vdpVideoMixerCreate;
    VdpVideoMixerSetFeatureEnables *vdpVideoMixerSetFeatureEnables;

    VdpVideoMixerQueryFeatureSupport *vdpVideoMixerQueryFeatureSupport;

    VdpVideoMixerSetAttributeValues *vdpVideoMixerSetAttributeValues;

    VdpVideoSurfaceCreate *vdpVideoSurfaceCreate;
    VdpVideoSurfaceDestroy *vdpVideoSurfaceDestroy;
    VdpOutputSurfaceCreate *vdpOutputSurfaceCreate;
    VdpOutputSurfaceDestroy *vdpOutputSurfaceDestroy;
    VdpVideoSurface   videosSurface;
    VdpOutputSurface  outputSurface;

    VdpVideoSurfacePutBitsYCbCr *vdpVideoSurfacePutBitsYCbCr;
    VdpOutputSurfaceGetBitsNative *vdpOutputSurfaceGetBitsNative;

    VdpVideoMixerDestroy *vdpVideoMixerDestroy;
    VdpVideoMixerRender *vdpVideoMixerRender;
    VdpDeviceDestroy *vdpDeviceDestroy;

    VdpGetErrorString *vdpGetErrorString;

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
    { NULL }
};

AVFILTER_DEFINE_CLASS(vdpau);

#define GET_CALLBACK(id, result)                                                \
do {                                                                            \
    void *tmp;                                                                  \
    int err;                                                                    \
    err = s->get_proc_address(s->device, id, &tmp);                             \
    if (err != VDP_STATUS_OK) {                                                 \
        av_log(ctx, AV_LOG_ERROR, "Error getting the " #id " callback.\n");     \
        return -1;                                                              \
    }                                                                           \
    result = tmp;                                                               \
} while (0)

static int check_support(VdpauContext *s, VdpVideoMixerFeature feature, const char* name) {
    VdpBool is_supported;
    VdpStatus ret;

    ret = s->vdpVideoMixerQueryFeatureSupport(s->device, VDP_VIDEO_MIXER_FEATURE_SHARPNESS, &is_supported);
    if (ret != VDP_STATUS_OK) {
        av_log(NULL, AV_LOG_ERROR, "VDPAU mixer query feature %s failed: %s\n",
                s->display_name, s->vdpGetErrorString(ret));
        return AVERROR_UNKNOWN;
    }
    if (is_supported == VDP_FALSE) {
        av_log(NULL, AV_LOG_ERROR, "%s is not supported!", name);
        return AVERROR_UNKNOWN;
    }

    return 0;
}

static av_cold int init(AVFilterContext *ctx)
{
    VdpauContext *s = ctx->priv;
    VdpStatus ret;
    AVHWDeviceContext *device_ctx;
    AVVDPAUDeviceContext *device_hwctx;
    int i = 0;


    s->display = 0;
    s->device  = 0;
    s->mixer   = 0;
    s->feature_cnt = 0;

    s->display = XOpenDisplay(0);
    if (!s->display) {
        av_log(ctx, AV_LOG_ERROR, "Cannot open the X11 display %s.\n",
               XDisplayName(0));
        return -1;
    }

    ret = vdp_device_create_x11(s->display, XDefaultScreen(s->display),
                                &s->device, &s->get_proc_address);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU device creation on X11 display %s failed.\n",
                XDisplayName(0));
        return -1;
    }

    s->display_name = XDisplayString(s->display);

    s->hwdevice = av_hwdevice_ctx_alloc(AV_HWDEVICE_TYPE_VDPAU);
    if (!s->hwdevice) {
        return AVERROR(ENOMEM);
    }

    device_ctx       = (AVHWDeviceContext*)s->hwdevice->data;
    device_ctx->free = NULL;

    device_hwctx = device_ctx->hwctx;
    device_hwctx->device = s->device;
    device_hwctx->get_proc_address = s->get_proc_address;

    ret = av_hwdevice_ctx_init(s->hwdevice);
    if (ret < 0)
        return ret;

    GET_CALLBACK(VDP_FUNC_ID_VIDEO_MIXER_CREATE, s->vdpVideoMixerCreate);
    GET_CALLBACK(VDP_FUNC_ID_VIDEO_MIXER_SET_FEATURE_ENABLES, s->vdpVideoMixerSetFeatureEnables);
    GET_CALLBACK(VDP_FUNC_ID_VIDEO_MIXER_DESTROY, s->vdpVideoMixerDestroy);
    GET_CALLBACK(VDP_FUNC_ID_DEVICE_DESTROY, s->vdpDeviceDestroy);
    GET_CALLBACK(VDP_FUNC_ID_VIDEO_SURFACE_CREATE, s->vdpVideoSurfaceCreate);
    GET_CALLBACK(VDP_FUNC_ID_VIDEO_SURFACE_PUT_BITS_Y_CB_CR, s->vdpVideoSurfacePutBitsYCbCr);
    GET_CALLBACK(VDP_FUNC_ID_OUTPUT_SURFACE_GET_BITS_NATIVE, s->vdpOutputSurfaceGetBitsNative);
    GET_CALLBACK(VDP_FUNC_ID_OUTPUT_SURFACE_CREATE, s->vdpOutputSurfaceCreate);
    GET_CALLBACK(VDP_FUNC_ID_DEVICE_DESTROY, s->vdpDeviceDestroy);
    GET_CALLBACK(VDP_FUNC_ID_VIDEO_MIXER_DESTROY, s->vdpVideoMixerDestroy);
    GET_CALLBACK(VDP_FUNC_ID_VIDEO_MIXER_RENDER, s->vdpVideoMixerRender);
    GET_CALLBACK(VDP_FUNC_ID_OUTPUT_SURFACE_DESTROY, s->vdpOutputSurfaceDestroy);
    GET_CALLBACK(VDP_FUNC_ID_VIDEO_SURFACE_DESTROY, s->vdpVideoSurfaceDestroy);
    GET_CALLBACK(VDP_FUNC_ID_GET_ERROR_STRING, s->vdpGetErrorString);
    GET_CALLBACK(VDP_FUNC_ID_VIDEO_MIXER_SET_ATTRIBUTE_VALUES, s->vdpVideoMixerSetAttributeValues);
    GET_CALLBACK(VDP_FUNC_ID_VIDEO_MIXER_QUERY_FEATURE_SUPPORT, s->vdpVideoMixerQueryFeatureSupport);

    //Check if features selected are supported
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

    return 0;
}

static int query_formats(AVFilterContext *ctx)
{
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

    return 0;
}

static int config_input(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    VdpauContext *s = inlink->dst->priv;
    VdpStatus ret;
    VdpBool enables[MAX_SUPPORTED_FEATURES] = { VDP_TRUE };
    VdpVideoMixerParameter parameters[] = {
        VDP_VIDEO_MIXER_PARAMETER_VIDEO_SURFACE_WIDTH,
        VDP_VIDEO_MIXER_PARAMETER_VIDEO_SURFACE_HEIGHT,
    };
    const void *parameter_values[] = {
        &inlink->w,
        &inlink->h
    };

    ret = s->vdpVideoMixerCreate(s->device, s->feature_cnt, s->features, 2, parameters, parameter_values, &s->mixer);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU mixer creation on X11 display %s failed: %s\n",
                s->display_name, s->vdpGetErrorString(ret));
        return -1;
    }

    ret = s->vdpVideoMixerSetFeatureEnables(s->mixer, s->feature_cnt, s->features, enables);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU mixer set feature on X11 display %s failed: %s\n",
                s->display_name, s->vdpGetErrorString(ret));
        return -1;
    }

    ret = s->vdpVideoMixerSetAttributeValues(s->mixer, s->attribute_cnt, s->attributes, s->attribute_values);

    ret = s->vdpVideoSurfaceCreate(s->device, VDP_CHROMA_TYPE_420, inlink->w, inlink->h, &s->videosSurface);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU video surface create on X11 display %s failed failed: %s\n",
                s->display_name, s->vdpGetErrorString(ret));
        return -1;
    }

    ret = s->vdpOutputSurfaceCreate(s->device, VDP_RGBA_FORMAT_B8G8R8A8, inlink->w, inlink->h, &s->outputSurface);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU output surface create on X11 display %s failed: %s\n",
                s->display_name, s->vdpGetErrorString(ret));
        s->vdpVideoSurfaceDestroy(s->videosSurface);
        return -1;
    }

    return 0;
}

static int config_output(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    VdpauContext *s = ctx->priv;
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

    ret = av_hwframe_ctx_init(s->hwframe);
    if (ret < 0)
        return ret;

//    outlink->hw_frames_ctx = av_buffer_ref(s->hwframe);
//    if (!outlink->hw_frames_ctx)
//        return AVERROR(ENOMEM);

    return 0;
}

static int filter_frame(AVFilterLink *inlink, AVFrame *frame)
{
    AVFilterContext *ctx = inlink->dst;
    AVFilterLink *outlink = ctx->outputs[0];
    VdpauContext *s = ctx->priv;
    VdpStatus ret;
    AVFrame *oFrame;
    AVFrame *iFrame;
    int i;
    VdpVideoSurface pastVideoSurfaces[MAX_PAST_FRAMES];
    VdpVideoSurface futureVideoSurfaces[MAX_FUTURE_FRAMES];
    AVFrame *oldFrame;
    VdpVideoMixerPictureStructure picture_structure;

//    ret = s->vdpVideoSurfacePutBitsYCbCr(s->videosSurface, VDP_YCBCR_FORMAT_YV12, (const void * const*)frame->data, frame->linesize);
//    if (ret != VDP_STATUS_OK) {
//        av_log(ctx, AV_LOG_ERROR, "VDPAU vdpVideoSurfacePutBitsYCbCr on X11 display %s failed: %s\n",
//                s->display_name, s->vdpGetErrorString(ret));
//        return AVERROR_UNKNOWN;
//    }

    if (frame != NULL) {
        iFrame = av_frame_alloc();
        if (!iFrame) {
            return AVERROR(ENOMEM);
            av_frame_free(&frame);
        }

        ret = av_hwframe_get_buffer(s->hwframe, iFrame, 0);
        if (ret < 0) {
            av_frame_free(&frame);
            return ret;
        }

        ret = av_hwframe_transfer_data(iFrame, frame, 0);
        if (ret < 0) {
            av_log(ctx, AV_LOG_ERROR, "Error transferring data to the GPU\n");
            av_frame_free(&frame);
            av_frame_free(&iFrame);
            return ret;
        }
        av_frame_copy_props(iFrame, frame);
    } else {
        iFrame = NULL;
    }

    //free oldes frame
    oldFrame = s->past_frames[MAX_PAST_FRAMES -1];
    if (oldFrame != NULL) {
        av_frame_unref(oldFrame);
    }

    //update old frames
    for (i = MAX_PAST_FRAMES; i-- > 1;) {
        s->past_frames[i] = s->past_frames[i - 1];
    }
    s->past_frames[0] = s->cur_frame;
    //s->old_frames_cnt = (s->old_frames_cnt >= MAX_OLD_FRAMES) ? MAX_OLD_FRAMES : s->old_frames_cnt++;

    s->cur_frame = s->future_frames[0];

    //update next frames
    for (i = 1; i < MAX_FUTURE_FRAMES; i++) {
        s->future_frames[i - 1] = s->future_frames[i];
    }
    s->future_frames[MAX_FUTURE_FRAMES - 1] = iFrame;

    if (s->cur_frame == NULL) {
        return 0;
    }

    if (s->future_frames[0] == NULL && s->cur_frame != 0) {
        s->eof = 1;
    }

    //setup past videoSurfaces
    for (i = 0; i < MAX_PAST_FRAMES; i++) {
        if (s->past_frames[i] != NULL) {
            pastVideoSurfaces[i] = (VdpVideoSurface)s->past_frames[i]->data[3];
        } else {
            pastVideoSurfaces[i] = VDP_INVALID_HANDLE;
        }
    }

    //setup future videoSurfaces
    for (i = 0; i < MAX_FUTURE_FRAMES; i++) {
        if (s->future_frames[i] != NULL) {
            futureVideoSurfaces[i] = (VdpVideoSurface)s->future_frames[i]->data[3];
        } else {
            futureVideoSurfaces[i] = VDP_INVALID_HANDLE;
        }
    }

    if (s->deinterlacer != NONE && s->cur_frame->interlaced_frame) {
        picture_structure = s->cur_frame->top_field_first ? VDP_VIDEO_MIXER_PICTURE_STRUCTURE_TOP_FIELD :
                VDP_VIDEO_MIXER_PICTURE_STRUCTURE_BOTTOM_FIELD;
    } else {
        picture_structure = VDP_VIDEO_MIXER_PICTURE_STRUCTURE_FRAME;
    }


    ret = s->vdpVideoMixerRender(s->mixer,
                                 VDP_INVALID_HANDLE,
                                 NULL,
                                 picture_structure,
                                 MAX_PAST_FRAMES,
                                 pastVideoSurfaces,
                                 (VdpVideoSurface)s->cur_frame->data[3],
                                 MAX_FUTURE_FRAMES,
                                 futureVideoSurfaces,
                                 NULL,
                                 s->outputSurface,
                                 NULL, //???
                                 NULL,
                                 0,
                                 NULL);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU video mixer render on X11 display %s failed: %s\n",
                s->display_name, s->vdpGetErrorString(ret));
        return AVERROR_UNKNOWN;
    }

    oFrame = ff_get_video_buffer(outlink, outlink->w, outlink->h);
    av_frame_copy_props(oFrame, s->cur_frame);


    s->vdpOutputSurfaceGetBitsNative(s->outputSurface, NULL, (void * const*)oFrame->data, oFrame->linesize);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU vdpOutputSurfaceGetBitsNative on X11 display %s failed: %s\n",
                s->display_name, s->vdpGetErrorString(ret));
        return AVERROR_UNKNOWN;
    }

    av_frame_free(&frame);
    return ff_filter_frame(outlink, oFrame);
}

static int request_frame(AVFilterLink *link)
{
    AVFilterContext *ctx = link->src;
    VdpauContext *s = ctx->priv;
    int ret;

    if (s->eof)
        return AVERROR_EOF;

    ret = ff_request_frame(link->src->inputs[0]);

    if (ret == AVERROR_EOF && s->cur_frame) {
        ret = filter_frame(link->src->inputs[0], NULL);
    }

    return ret;
}

static av_cold void uninit(AVFilterContext *ctx)
{
    VdpauContext *s = ctx->priv;

    if (s->mixer) s->vdpVideoMixerDestroy(s->mixer);
    if (s->device) s->vdpDeviceDestroy(s->device);
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
};
