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
#include "libavutil/eval.h"
#include "avfilter.h"
#include "formats.h"
#include "internal.h"
#include "video.h"
#include "framesync.h"

#define MAX_FUTURE_FRAMES 5
#define MAX_PAST_FRAMES 5

#define MAX_SUPPORTED_FEATURES 6
#define MAX_SUPPORTED_ATTRIBUTES 6

enum var_name {
    VAR_IN_W,   VAR_IW,
    VAR_IN_H,   VAR_IH,
    VAR_OUT_W,  VAR_OW,
    VAR_OUT_H,  VAR_OH,
    VAR_A,
    VAR_SAR,
    VAR_DAR,
    VAR_HSUB,
    VAR_VSUB,
    VARS_NB
};

static const char *const var_names[] = {
    "in_w",   "iw",
    "in_h",   "ih",
    "out_w",  "ow",
    "out_h",  "oh",
    "a",
    "sar",
    "dar",
    "hsub",
    "vsub",
    NULL
};

static const char *const var_names_overlay[] = {
    "main_w",    "W", ///< width  of the main    video
    "main_h",    "H", ///< height of the main    video
    "overlay_w", "w", ///< width  of the overlay video
    "overlay_h", "h", ///< height of the overlay video
    "hsub",
    "vsub",
    "x1",
    "y1",
    "x2",
    "y2",
    NULL
};

enum var_name_overlay {
    OVERLAY_VAR_MAIN_W,    OVERLAY_VAR_MW,
    OVERLAY_VAR_MAIN_H,    OVERLAY_VAR_MH,
    OVERLAY_VAR_OVERLAY_W, OVERLAY_VAR_OW,
    OVERLAY_VAR_OVERLAY_H, OVERLAY_VAR_OH,
    OVERLAY_VAR_HSUB,
    OVERLAY_VAR_VSUB,
    OVERLAY_VAR_X1,
    OVERLAY_VAR_Y1,
    OVERLAY_VAR_X2,
    OVERLAY_VAR_Y2,
    OVERLAY_VAR_VARS_NB
};

enum DeinterlaceMethode {
    NONE,
    BOB,
    TEMPORAL,
    TEMPORAL_SPATIAL
};

enum EOFAction {
    EOF_ACTION_REPEAT,
    EOF_ACTION_ENDALL,
    EOF_ACTION_PASS
};

typedef struct VDPAUPRGBFmpMap {
    VdpRGBAFormat vdpau_fmt;
    enum AVPixelFormat pix_fmt;
} VDPAUPRGBFmpMap ;

static const VDPAUPRGBFmpMap  pix_fmts_rgb[] = {
    { VDP_RGBA_FORMAT_R8G8B8A8, AV_PIX_FMT_RGBA},
    { VDP_RGBA_FORMAT_B8G8R8A8, AV_PIX_FMT_BGRA},
    { VDP_INVALID_HANDLE,       AV_PIX_FMT_NONE}
};

static int get_vdpau_rgb_format(enum AVPixelFormat format)
{
    size_t i;

    for (i = 0; i < FF_ARRAY_ELEMS(pix_fmts_rgb); i++) {
        if (pix_fmts_rgb[i].pix_fmt == format) {
            return pix_fmts_rgb[i].vdpau_fmt;
        }
    }

    return VDP_INVALID_HANDLE;
}

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
    VdpOutputSurfaceQueryGetPutBitsNativeCapabilities *VdpOutputSurfaceQueryGetPutBitsNativeCapabilities;
    VdpOutputSurfaceQueryCapabilities *VdpOutputSurfaceQueryCapabilities;

    VdpVideoMixerDestroy *vdpVideoMixerDestroy;
    VdpVideoMixerRender *vdpVideoMixerRender;
    VdpDeviceDestroy *vdpDeviceDestroy;

    VdpGetErrorString *vdpGetErrorString;
} VdpauFunctions;

typedef struct {
    const AVClass *class;

//    Display *display;
//    char *display_name;

    VdpDevice device;
    VdpauFunctions vdpaufuncs;

    AVBufferRef *hwdevice;
    AVBufferRef *hwframe;
    AVBufferRef *output_hwframe;
    AVBufferRef *overlay_hwframe;

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
    AVFrame *cur_overlay_frame;

    int in_format;
    int overlay_format;

    int future_frames_cnt;
    AVFrame *future_frames[MAX_FUTURE_FRAMES];
    AVFrame *future_overlay_frames[MAX_FUTURE_FRAMES];

    int past_frames_cnt;
    AVFrame *past_frames[MAX_PAST_FRAMES];

    int eof;

    float noise_reduction;
    float sharpness;
    int deinterlacer;

    int double_framerate;
    AVRational ts_unit;
    int64_t start_time;

    int w;
    int h;

    char *size_str;
    char *w_expr;
    char *h_expr;
    int force_original_aspect_ratio;
    int scaling_quality;

    //overlay parameters
    FFFrameSync fs;
    int use_overlay;
    int eof_action;
    char *x1_expr;
    char *y1_expr;
    char *x2_expr;
    char *y2_expr;
    int x1;
    int y1;
    int x2;
    int y2;
    int shortest;
    int repeatlast;
} VdpauContext;

#define OFFSET(x) offsetof(VdpauContext, x)
#define FLAGS AV_OPT_FLAG_FILTERING_PARAM|AV_OPT_FLAG_VIDEO_PARAM

static const AVOption vdpau_options[] = {
    { "future_frame_number", "set number of future frames ", OFFSET(future_frames_cnt),   AV_OPT_TYPE_INT, {.i64 = 1}, 0, MAX_FUTURE_FRAMES, FLAGS },
    { "past_frame_number", "set number of future frames ", OFFSET(past_frames_cnt),   AV_OPT_TYPE_INT, {.i64 = 1}, 0, MAX_PAST_FRAMES, FLAGS },
    { "noise_reduction", "set interlacing threshold", OFFSET(noise_reduction),   AV_OPT_TYPE_FLOAT, {.dbl = 0}, 0, 1.0, FLAGS },
    { "sharpness", "set progressive threshold", OFFSET(sharpness), AV_OPT_TYPE_FLOAT, {.dbl = 0},  -1, 1, FLAGS },
    { "deinterlacer", "set deinterlacing methode", OFFSET(deinterlacer), AV_OPT_TYPE_INT, {.i64 = 0},  0, TEMPORAL_SPATIAL, FLAGS, "deinterlacer"},
        {"bob", "bob deinterlacer", 0, AV_OPT_TYPE_CONST, {.i64=BOB}, 0, 0, FLAGS, "deinterlacer"},
        {"temporal", "temporal deinterlacer", 0, AV_OPT_TYPE_CONST, {.i64=TEMPORAL}, 0, 0, FLAGS, "deinterlacer"},
        {"temporal_spatial", "temporal spatial deinterlacer", 0, AV_OPT_TYPE_CONST, {.i64=TEMPORAL_SPATIAL}, 0, 0, FLAGS, "deinterlacer"},
    { "double_framerate", "double framerate", OFFSET(double_framerate), AV_OPT_TYPE_INT, {.i64 = 0}, 0, 1, FLAGS },
    { "w",     "Output video width",          OFFSET(w_expr),    AV_OPT_TYPE_STRING,        .flags = FLAGS },
    { "width", "Output video width",          OFFSET(w_expr),    AV_OPT_TYPE_STRING,        .flags = FLAGS },
    { "h",     "Output video height",         OFFSET(h_expr),    AV_OPT_TYPE_STRING,        .flags = FLAGS },
    { "height","Output video height",         OFFSET(h_expr),    AV_OPT_TYPE_STRING,        .flags = FLAGS },
    { "size",   "set video size",          OFFSET(size_str), AV_OPT_TYPE_STRING, {.str = NULL}, 0, FLAGS },
    { "scaling_quality", "set scaling quality", OFFSET(scaling_quality), AV_OPT_TYPE_INT, {.i64 = 1}, 1, 9, FLAGS },
    { "force_original_aspect_ratio", "decrease or increase w/h if necessary to keep the original AR", OFFSET(force_original_aspect_ratio), AV_OPT_TYPE_INT, { .i64 = 0}, 0, 2, FLAGS, "force_oar" },
    { "x1", "set the x expression", OFFSET(x1_expr), AV_OPT_TYPE_STRING, .flags=FLAGS },
    { "y1", "set the y expression", OFFSET(y1_expr), AV_OPT_TYPE_STRING, .flags=FLAGS },
    { "x2", "set the x expression", OFFSET(x2_expr), AV_OPT_TYPE_STRING, .flags=FLAGS },
    { "y2", "set the y expression", OFFSET(y2_expr), AV_OPT_TYPE_STRING, .flags=FLAGS },
    { "eof_action", "Action to take when encountering EOF from secondary input ",
        OFFSET(eof_action), AV_OPT_TYPE_INT, { .i64 = EOF_ACTION_REPEAT },
        EOF_ACTION_REPEAT, EOF_ACTION_PASS, .flags = FLAGS, "eof_action" },
        { "repeat", "Repeat the previous frame.",   0, AV_OPT_TYPE_CONST, { .i64 = EOF_ACTION_REPEAT }, .flags = FLAGS, "eof_action" },
        { "endall", "End both streams.",            0, AV_OPT_TYPE_CONST, { .i64 = EOF_ACTION_ENDALL }, .flags = FLAGS, "eof_action" },
        { "pass",   "Pass through the main input.", 0, AV_OPT_TYPE_CONST, { .i64 = EOF_ACTION_PASS },   .flags = FLAGS, "eof_action" },
    { "shortest", "force termination when the shortest input terminates", OFFSET(shortest), AV_OPT_TYPE_BOOL, { .i64 = 0 }, 0, 1, FLAGS },
    { "repeatlast", "repeat overlay of the last overlay frame", OFFSET(repeatlast), AV_OPT_TYPE_BOOL, {.i64=1}, 0, 1, FLAGS },
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
        av_log(NULL, AV_LOG_ERROR, "VDPAU mixer query feature failed: %s\n",
               vdpauFuncs->vdpGetErrorString(ret));
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
        (ret = ff_framesync_get_frame(&s->fs, 1, &secondpic, 1)) < 0) {
        av_frame_free(&mainpic);
        av_frame_free(&secondpic);
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
    int ret;
    const AVPixFmtDescriptor *pix_desc = av_pix_fmt_desc_get(inlink->format);
    double var_values[OVERLAY_VAR_VARS_NB];
    double res;

    /* Finish the configuration by evaluating the expressions
       now when both inputs are configured. */
    var_values[OVERLAY_VAR_MAIN_W] = var_values[OVERLAY_VAR_MW] = ctx->inputs[0]->w;
    var_values[OVERLAY_VAR_MAIN_H] = var_values[OVERLAY_VAR_MH] = ctx->inputs[0]->h;
    var_values[OVERLAY_VAR_OVERLAY_W] = var_values[OVERLAY_VAR_OW] = inlink->w;
    var_values[OVERLAY_VAR_OVERLAY_H] = var_values[OVERLAY_VAR_OH] = inlink->h;
    var_values[OVERLAY_VAR_HSUB] = 1<<pix_desc->log2_chroma_w;
    var_values[OVERLAY_VAR_VSUB] = 1<<pix_desc->log2_chroma_h;
    var_values[OVERLAY_VAR_X1] = NAN;
    var_values[OVERLAY_VAR_Y1] = NAN;
    var_values[OVERLAY_VAR_X2] = NAN;
    var_values[OVERLAY_VAR_Y2] = NAN;

    ret = av_expr_parse_and_eval(&res, s->x1_expr,
                           var_names_overlay, var_values,
                           NULL, NULL, NULL, NULL, NULL, 0, ctx);
    if (ret < 0) return ret;
    var_values[OVERLAY_VAR_X1] = res;

    ret = av_expr_parse_and_eval(&res, s->y1_expr,
                           var_names_overlay, var_values,
                           NULL, NULL, NULL, NULL, NULL, 0, ctx);
    if (ret < 0) return ret;
    var_values[OVERLAY_VAR_Y1] = res;

    ret = av_expr_parse_and_eval(&res, s->x2_expr,
                           var_names_overlay, var_values,
                           NULL, NULL, NULL, NULL, NULL, 0, ctx);
    if (ret < 0) return ret;
    var_values[OVERLAY_VAR_X2] = res;

    ret = av_expr_parse_and_eval(&res, s->y2_expr,
                           var_names_overlay, var_values,
                           NULL, NULL, NULL, NULL, NULL, 0, ctx);
    if (ret < 0) return ret;
    var_values[OVERLAY_VAR_Y2] = res;

    s->x1 = (int)var_values[OVERLAY_VAR_X1];
    s->y1 = (int)var_values[OVERLAY_VAR_Y1];
    s->x2 = (int)var_values[OVERLAY_VAR_X2];
    s->y2 = (int)var_values[OVERLAY_VAR_Y2];

    av_log(ctx, AV_LOG_VERBOSE, "Overlay position: x1:%d y1:%d x2:%d y2:%d\n",
            s->x1, s->y1, s->x2, s->y2);

    return 0;
}

static av_cold int init_scaling(AVFilterContext *ctx)
{
    VdpauContext *s = ctx->priv;
    int ret;

    if (s->size_str && (s->w_expr || s->h_expr)) {
        av_log(ctx, AV_LOG_ERROR,
               "Size and width/height expressions cannot be set at the same time.\n");
            return AVERROR(EINVAL);
    }

    if (s->w_expr && !s->h_expr)
        FFSWAP(char *, s->w_expr, s->size_str);

    if (s->size_str) {
        char buf[32];
        if ((ret = av_parse_video_size(&s->w, &s->h, s->size_str)) < 0) {
            av_log(ctx, AV_LOG_ERROR,
                   "Invalid size '%s'\n", s->size_str);
            return ret;
        }
        snprintf(buf, sizeof(buf)-1, "%d", s->w);
        av_opt_set(s, "w", buf, 0);
        snprintf(buf, sizeof(buf)-1, "%d", s->h);
        av_opt_set(s, "h", buf, 0);
    }
    if (!s->w_expr)
        av_opt_set(s, "w", "iw", 0);
    if (!s->h_expr)
        av_opt_set(s, "h", "ih", 0);

    av_log(ctx, AV_LOG_VERBOSE, "w:%s h:%s\n",
           s->w_expr, s->h_expr);

    return 0;
}

static av_cold int init(AVFilterContext *ctx)
{
    VdpauContext *s = ctx->priv;
    VdpauFunctions *vdpauFuncs =  &s->vdpaufuncs;
    int ret;
    int i = 0;


    //s->display = 0;
    s->device  = 0;
    s->mixer   = 0;
    s->feature_cnt = 0;
    s->start_time = AV_NOPTS_VALUE;

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

//    //open display
//    s->display = XOpenDisplay(0);
//    if (!s->display) {
//        av_log(ctx, AV_LOG_ERROR, "Cannot open the X11 display %s.\n",
//               XDisplayName(0));
//        return -1;
//    }
//    s->display_name = XDisplayString(s->display);
//
//    //setup vdpau device
//    status = vdp_device_create_x11(s->display, XDefaultScreen(s->display),
//                                &s->device, &vdpauFuncs->get_proc_address);
//    if (status != VDP_STATUS_OK) {
//        av_log(ctx, AV_LOG_ERROR, "VDPAU device creation on X11 display %s failed.\n",
//                XDisplayName(0));
//        return AVERROR_UNKNOWN;
//    }



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

    //handle scaling if enabled
    if (s->w_expr != NULL || s->h_expr != NULL || s->size_str != NULL) {
        init_scaling(ctx);
    }

    s->use_overlay = 0;
    if (s->x1_expr != NULL || s->y1_expr != NULL || s->x2_expr != NULL || s->y2_expr != NULL) {
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

        if (!s->x2_expr)
            av_opt_set(s, "x2_expr", "x1+overlay_w", 0);
        if (!s->y2_expr)
            av_opt_set(s, "y2_expr", "y1+overlay_h", 0);

        s->use_overlay = 1;
    }

    return 0;
}

//add format to avff if vdpFormat is supported by VdpOutputSurfacePutBitsNative
static int add_outputsurface_format(AVFilterContext *ctx, VdpRGBAFormat vdpFormat,
        int64_t format, AVFilterFormats **avff, uint32_t *w, uint32_t *h)
{
    VdpauContext *s = ctx->priv;
    VdpauFunctions *f = &s->vdpaufuncs;
    VdpBool supported;
    VdpStatus status;
    int ret;

    status = f->VdpOutputSurfaceQueryCapabilities(s->device, vdpFormat, &supported, w, h);
    if (status != VDP_STATUS_OK) {
        return AVERROR_UNKNOWN;
    }
    if (supported) {
        if ((ret = ff_add_format(avff, format)) < 0) {
            return ret;
        }
    }

    return 0;
}

//add format to avff if vdpFormat is supported by VdpOutputSurface
static int add_outputsurface_in_format(AVFilterContext *ctx, VdpRGBAFormat vdpFormat, int64_t format, AVFilterFormats **avff)
{
    VdpauContext *s = ctx->priv;
    VdpauFunctions *f = &s->vdpaufuncs;
    VdpBool supported;
    VdpStatus status;
    int ret;

    status = f->VdpOutputSurfaceQueryGetPutBitsNativeCapabilities(s->device, vdpFormat, &supported);
    if (status != VDP_STATUS_OK) {
        return AVERROR_UNKNOWN;
    }
    if (supported) {
        if ((ret = ff_add_format(avff, format)) < 0) {
            return ret;
        }
    }

    return 0;
}

static int add_supported_overlay_formats(AVFilterContext *ctx, AVFilterFormats **avff)
{
    int ret;

    if ((ret = add_outputsurface_in_format(ctx, VDP_RGBA_FORMAT_R8G8B8A8, AV_PIX_FMT_RGBA, avff) < 0)) {
        return ret;
    }
    if ((ret = add_outputsurface_in_format(ctx, VDP_RGBA_FORMAT_B8G8R8A8, AV_PIX_FMT_BGRA, avff) < 0)) {
        return ret;
    }

    return 0;
}

static int add_supported_output_formats(AVFilterContext *ctx, AVFilterFormats **avff) {
    int ret;
    uint32_t max_w, max_h;

    ret = add_outputsurface_format(ctx, VDP_RGBA_FORMAT_R8G8B8A8, AV_PIX_FMT_RGBA, avff, &max_w, &max_h);
    if (ret < 0) {
        return ret;
    }
    ret = add_outputsurface_format(ctx, VDP_RGBA_FORMAT_B8G8R8A8, AV_PIX_FMT_BGRA, avff, &max_w, &max_h);
    if (ret < 0) {
        return ret;
    }

    return 0;
}

static int query_formats(AVFilterContext *ctx)
{
    VdpauContext *s = ctx->priv;
    AVFilterFormats *in_formats;
    AVFilterFormats *out_formats = NULL;
    int ret;
    AVFilterLink *inlink  = ctx->inputs[0];
    AVFilterLink *outlink = ctx->outputs[0];
    static const enum AVPixelFormat pix_fmts_in[] = {
        AV_PIX_FMT_VDPAU,
        AV_PIX_FMT_YUV420P,
        AV_PIX_FMT_NONE
    };
    static const enum AVPixelFormat pix_fmt_overlay_in[] = {
        //AV_PIX_FMT_VDPAU_OUTPUTSURFACE,
        AV_PIX_FMT_BGRA,
        AV_PIX_FMT_NONE
    };
    static const enum AVPixelFormat pix_fmts_out[] = {
        AV_PIX_FMT_VDPAU_OUTPUTSURFACE,
        AV_PIX_FMT_BGRA,
        AV_PIX_FMT_NONE
    };

    in_formats = ff_make_format_list(pix_fmts_in);
    out_formats = ff_make_format_list(pix_fmts_out);
    if (!in_formats || !out_formats) {
        return AVERROR(ENOMEM);
    }


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
    VdpStatus status;
    AVHWDeviceContext *device_ctx;
    AVVDPAUDeviceContext *device_hwctx;
    VdpStatus ret;
    VdpBool enables[MAX_SUPPORTED_FEATURES] = { VDP_TRUE };
    VdpVideoMixerParameter parameters[] = {
        VDP_VIDEO_MIXER_PARAMETER_VIDEO_SURFACE_WIDTH,
        VDP_VIDEO_MIXER_PARAMETER_VIDEO_SURFACE_HEIGHT,
        VDP_VIDEO_MIXER_PARAMETER_LAYERS
    };
    int layers = (s->use_overlay) ? 1 : 0;
    VdpColor background_color = {0, 0, 0, 1.0};
    const void *parameter_values[] = {
        &inlink->w,
        &inlink->h,
        &layers
    };
    s->attributes[s->attribute_cnt] = VDP_VIDEO_MIXER_ATTRIBUTE_BACKGROUND_COLOR;
    s->attribute_values[s->attribute_cnt++] = &background_color;

    if (ctx->hw_device_ctx == 0) {
        av_log(ctx, AV_LOG_ERROR, "No vdpau device given\n");
        return -1;
    }

    //allocate hardware context
    s->hwdevice = ctx->hw_device_ctx;//av_hwdevice_ctx_alloc(AV_HWDEVICE_TYPE_VDPAU);
//    if (!s->hwdevice) {
//        return AVERROR(ENOMEM);
//    }

    device_ctx       = (AVHWDeviceContext*)s->hwdevice->data;
    //device_ctx->free = NULL;

    device_hwctx = device_ctx->hwctx;
//    device_hwctx->device = s->device;
//    device_hwctx->get_proc_address = vdpauFuncs->get_proc_address;
    vdpauFuncs->get_proc_address = device_hwctx->get_proc_address;
    s->device = device_hwctx->device;

    //init hardware device
//    ret = av_hwdevice_ctx_init(s->hwdevice);
//    if (ret < 0)
//        return ret;

    //av_buffer_unref(&s->hwframe);


    s->in_format = inlink->format;

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
    GET_CALLBACK(VDP_FUNC_ID_OUTPUT_SURFACE_QUERY_GET_PUT_BITS_NATIVE_CAPABILITIES, vdpauFuncs->VdpOutputSurfaceQueryGetPutBitsNativeCapabilities);
    GET_CALLBACK(VDP_FUNC_ID_OUTPUT_SURFACE_QUERY_CAPABILITIES, vdpauFuncs->VdpOutputSurfaceQueryCapabilities);

    //Check if features selected are supported
    ret = init_supported_features(s);
    if (ret < 0) {
        return ret;
    }

    ret = vdpauFuncs->vdpVideoMixerCreate(s->device, s->feature_cnt, s->features, 3, parameters, parameter_values, &s->mixer);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU mixer creation on X11 display failed: %s\n",
               vdpauFuncs->vdpGetErrorString(ret));
        return -1;
    }

    ret = vdpauFuncs->vdpVideoMixerSetFeatureEnables(s->mixer, s->feature_cnt, s->features, enables);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU mixer set feature on X11 display failed: %s\n",
               vdpauFuncs->vdpGetErrorString(ret));
        return -1;
    }

    ret = vdpauFuncs->vdpVideoMixerSetAttributeValues(s->mixer, s->attribute_cnt, s->attributes, s->attribute_values);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU video  mixer set attribute values on X11 displayfailed: %s\n",
               vdpauFuncs->vdpGetErrorString(ret));
        return -1;
    }

    ret = vdpauFuncs->vdpVideoSurfaceCreate(s->device, VDP_CHROMA_TYPE_420, inlink->w, inlink->h, &s->videosSurface);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU video surface create on X11 display failed: %s\n",
               vdpauFuncs->vdpGetErrorString(ret));
        return -1;
    }

    return 0;
}

static int config_scaling(AVFilterLink *outlink, int *width, int *height)
{
    AVFilterContext *ctx = outlink->src;
    VdpauContext *s = ctx->priv;
    const AVFilterLink *inlink = ctx->inputs[0];
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(inlink->format);
    char *expr;
    int ret;
    int factor_w, factor_h;
    double var_values[VARS_NB], res;
    int64_t w, h;

    var_values[VAR_IN_W]  = var_values[VAR_IW] = inlink->w;
    var_values[VAR_IN_H]  = var_values[VAR_IH] = inlink->h;
    var_values[VAR_OUT_W] = var_values[VAR_OW] = NAN;
    var_values[VAR_OUT_H] = var_values[VAR_OH] = NAN;
    var_values[VAR_A]     = (double) inlink->w / inlink->h;
    var_values[VAR_SAR]   = inlink->sample_aspect_ratio.num ?
        (double) inlink->sample_aspect_ratio.num / inlink->sample_aspect_ratio.den : 1;
    var_values[VAR_DAR]   = var_values[VAR_A] * var_values[VAR_SAR];
    var_values[VAR_HSUB]  = 1 << desc->log2_chroma_w;
    var_values[VAR_VSUB]  = 1 << desc->log2_chroma_h;

    /* evaluate width and height */
      av_expr_parse_and_eval(&res, (expr = s->w_expr),
                             var_names, var_values,
                             NULL, NULL, NULL, NULL, NULL, 0, ctx);
      s->w = var_values[VAR_OUT_W] = var_values[VAR_OW] = res;
      if ((ret = av_expr_parse_and_eval(&res, (expr = s->h_expr),
                                        var_names, var_values,
                                        NULL, NULL, NULL, NULL, NULL, 0, ctx)) < 0)
          goto fail;
      s->h = var_values[VAR_OUT_H] = var_values[VAR_OH] = res;
      /* evaluate again the width, as it may depend on the output height */
      if ((ret = av_expr_parse_and_eval(&res, (expr = s->w_expr),
                                        var_names, var_values,
                                        NULL, NULL, NULL, NULL, NULL, 0, ctx)) < 0)
          goto fail;
      s->w = res;

      w = s->w;
      h = s->h;

      /* Check if it is requested that the result has to be divisible by a some
       * factor (w or h = -n with n being the factor). */
      factor_w = 1;
      factor_h = 1;
      if (w < -1) {
          factor_w = -w;
      }
      if (h < -1) {
          factor_h = -h;
      }

      if (w < 0 && h < 0)
          s->w = s->h = 0;

      if (!(w = s->w))
          w = inlink->w;
      if (!(h = s->h))
          h = inlink->h;

      /* Make sure that the result is divisible by the factor we determined
       * earlier. If no factor was set, it is nothing will happen as the default
       * factor is 1 */
      if (w < 0)
          w = av_rescale(h, inlink->w, inlink->h * factor_w) * factor_w;
      if (h < 0)
          h = av_rescale(w, inlink->h, inlink->w * factor_h) * factor_h;

      /* Note that force_original_aspect_ratio may overwrite the previous set
       * dimensions so that it is not divisible by the set factors anymore. */
      if (s->force_original_aspect_ratio) {
          int tmp_w = av_rescale(h, inlink->w, inlink->h);
          int tmp_h = av_rescale(w, inlink->h, inlink->w);

          if (s->force_original_aspect_ratio == 1) {
               w = FFMIN(tmp_w, w);
               h = FFMIN(tmp_h, h);
          } else {
               w = FFMAX(tmp_w, w);
               h = FFMAX(tmp_h, h);
          }
      }

      if (w > INT_MAX || h > INT_MAX ||
          (h * inlink->w) > INT_MAX  ||
          (w * inlink->h) > INT_MAX)
          av_log(ctx, AV_LOG_ERROR, "Rescaled value for width or height is too big.\n");

      *width = (int)w;
      *height = (int)h;

      return 0;

 fail:
      av_log(ctx, AV_LOG_ERROR,
             "Error when evaluating the expression '%s'.\n"
             "Maybe the expression for out_w:'%s' or for out_h:'%s' is self-referencing.\n",
             expr, s->w_expr, s->h_expr);
      return ret;
}

static int config_output(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    VdpauContext *s = ctx->priv;
    VdpauFunctions *vdpauFuncs = &s->vdpaufuncs;
    const AVFilterLink *inlink = ctx->inputs[0];
    VdpRGBAFormat surface_format;

    AVHWFramesContext *hwframe_ctx;
    AVHWFramesContext *output_hwframe_ctx;
    int ret;

    if (inlink->format != AV_PIX_FMT_VDPAU) {
        s->hwframe = av_hwframe_ctx_alloc(s->hwdevice);
        if (!s->hwframe) {
            return AVERROR(ENOMEM);
        }

        hwframe_ctx            = (AVHWFramesContext*)s->hwframe->data;
        hwframe_ctx->format    = AV_PIX_FMT_VDPAU;
        hwframe_ctx->sw_format = inlink->format;
        hwframe_ctx->width     = /*FFALIGN(*/inlink->w;//, 16);
        hwframe_ctx->height    = /*FFALIGN(*/inlink->h;//, 16);
        ret = av_hwframe_ctx_init(s->hwframe);
        if (ret < 0)
            return ret;
    } else {
        av_assert0(inlink->hw_frames_ctx != 0);
        s->hwframe = av_buffer_ref(inlink->hw_frames_ctx);
    }

    //create hardware frame ctx for output surfaces
    s->output_hwframe = av_hwframe_ctx_alloc(s->hwdevice);
    if (!s->output_hwframe) {
        return AVERROR(ENOMEM);
    }

    outlink->time_base = inlink->time_base;

    if (s->double_framerate) {
        AVRational fps = inlink->frame_rate;
        if (!fps.num || !fps.den) {
            av_log(ctx, AV_LOG_ERROR, "The input needs a constant frame rate; "
                   "current rate of %d/%d is invalid\n", fps.num, fps.den);
            return AVERROR(EINVAL);
        }

        fps = av_mul_q(fps, (AVRational){2, 1});
        outlink->frame_rate = fps;

        s->ts_unit = av_inv_q(av_mul_q(fps, outlink->time_base));
    }

    //handle scaling
    if (s->size_str != NULL || s->w_expr != NULL || s->h_expr != NULL) {
        if ((ret = config_scaling(outlink, &s->w, &s->h)) < 0) {
            return ret;
        }
    } else {
        s->w = inlink->w;
        s->h = inlink->h;
    }
    outlink->w = s->w;
    outlink->h = s->h;

    output_hwframe_ctx             = (AVHWFramesContext*)s->output_hwframe->data;
    output_hwframe_ctx->format     = AV_PIX_FMT_VDPAU_OUTPUTSURFACE;
    output_hwframe_ctx->sw_format  = AV_PIX_FMT_BGRA;
    output_hwframe_ctx->width      = outlink->w;
    output_hwframe_ctx->height     = outlink->h;

    ret = av_hwframe_ctx_init(s->output_hwframe);
    if (ret < 0)
        return ret;


    if (s->use_overlay) {
        if ((ret = init_framesync(ctx, s->repeatlast, s->shortest)) < 0) {
            return ret;
        }
    }

    if ((surface_format = get_vdpau_rgb_format(outlink->format) == VDP_INVALID_HANDLE)) {
        return AVERROR_INVALIDDATA;
    }
    ret = vdpauFuncs->vdpOutputSurfaceCreate(s->device, surface_format, outlink->w, outlink->h, &s->outputSurface);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU output surface create on X11 display failed: %s\n",
               vdpauFuncs->vdpGetErrorString(ret));
        vdpauFuncs->vdpVideoSurfaceDestroy(s->videosSurface);
        return -1;
    }

    if (s->use_overlay) {
        if (ctx->inputs[1]->format != AV_PIX_FMT_VDPAU_OUTPUTSURFACE) {
            AVHWFramesContext *overlay_hwframe_ctx;
            s->overlay_hwframe = av_hwframe_ctx_alloc(s->hwdevice);
            if (s->overlay_hwframe == NULL) {
                return AVERROR(ENOMEM);
            }

            overlay_hwframe_ctx             = (AVHWFramesContext*)s->overlay_hwframe->data;
            overlay_hwframe_ctx->format     = AV_PIX_FMT_VDPAU_OUTPUTSURFACE;
            overlay_hwframe_ctx->sw_format  = AV_PIX_FMT_BGRA;
            overlay_hwframe_ctx->width      = ctx->inputs[1]->w;
            overlay_hwframe_ctx->height     = ctx->inputs[1]->h;
            ret = av_hwframe_ctx_init(s->overlay_hwframe);
            if (ret < 0)
                return ret;
        } else {
            if ((s->overlay_hwframe = av_buffer_ref(ctx->inputs[1]->hw_frames_ctx)) == 0) {
                return AVERROR(ENOMEM);
            }
        }
    }

    return 0;
}

static int upload_frame_outputsurface(VdpauContext *s, AVFrame *frame, VdpOutputSurface surface) {
    VdpauFunctions *vdpauFuncs = &s->vdpaufuncs;
    VdpStatus ret;

    ret = vdpauFuncs->vdpOutputSurfacePutBitsNative(surface,
            (const void * const*)frame->data, frame->linesize, NULL);
    if (ret != VDP_STATUS_OK) {
        av_log(NULL, AV_LOG_ERROR, "VDPAU vdpOutputSurfacePutBitsNative on X11 display failed: %s\n",
               vdpauFuncs->vdpGetErrorString(ret));
        return AVERROR_UNKNOWN;
    }

    return 0;
}

static int upload_frame(AVBufferRef *hwframe, AVFrame *frame)
{
    int ret;
    AVFrame *tmp = av_frame_alloc();
    if (tmp == NULL) {
        return AVERROR(ENOMEM);
    }

    if ((ret = av_hwframe_get_buffer(hwframe, tmp, 0)) < 0) {
        return ret;
    }

    if ((ret = av_hwframe_transfer_data(tmp, frame, 0)) < 0) {
        return ret;
    }

    if ((ret = av_frame_copy_props(tmp, frame)) < 0) {
        return ret;
    }

    av_frame_unref(frame);
    av_frame_move_ref(frame, tmp);

    return 0;
}

static int download_frame(AVFilterLink *link, AVFrame *frame)
{
    int ret;
    AVFrame* tmp = ff_get_video_buffer(link, link->w, link->h);
    av_frame_copy_props(tmp, frame);

    if ((ret = av_hwframe_transfer_data(tmp, frame, 0)) < 0) {
        return ret;
    }

    av_frame_unref(frame);
    av_frame_move_ref(frame, tmp);

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

static void update_overlay_frames(VdpauContext *s, AVFrame *newFrame)
{
    int i;

    if (s->cur_overlay_frame != NULL) {
        av_frame_free(&s->cur_overlay_frame);
    }

    s->cur_overlay_frame = s->future_overlay_frames[0];

    //update future frames
    for (i = 1; i < MAX_FUTURE_FRAMES; i++) {
        s->future_overlay_frames[i - 1] = s->future_overlay_frames[i];
    }
    s->future_overlay_frames[MAX_FUTURE_FRAMES - 1] = newFrame;
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
                                          s->past_frames_cnt,
                                          past,
                                          cur,
                                          s->future_frames_cnt,
                                          future,
                                          NULL,
                                          out,
                                          &dest,
                                          NULL,
                                          layer_count,
                                          layer);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU video mixer render on X11 display failed: %s\n",
               vdpauFuncs->vdpGetErrorString(ret));
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
    int ret;
    AVFrame *oFrame1, *oFrame2;
    VdpVideoSurface past_surfaces[MAX_PAST_FRAMES];
    VdpVideoSurface future_surfaces[MAX_FUTURE_FRAMES];
    VdpVideoMixerPictureStructure picture_structure;
    VdpLayer overlay_layer;
    VdpRect overlay_rect;
    VdpLayer *layers = NULL;
    int layer_count = 0;

    if (frame != NULL && frame->format != AV_PIX_FMT_VDPAU) {
        if ((ret = upload_frame(s->hwframe, frame)) < 0) {
            return ret;
        }
    }

    if (s->use_overlay && overlay != NULL && overlay->format != AV_PIX_FMT_VDPAU_OUTPUTSURFACE) {
        if ((ret = upload_frame(s->overlay_hwframe, overlay)) < 0) {
            return ret;
        }
    }

    if (s->future_frames[0] == NULL && s->cur_frame != 0) {
        s->eof = 1;
    }

    update_frames(s, frame);
    if (s->use_overlay) {
        update_overlay_frames(s, overlay);
    }

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
    if (s->use_overlay) {
        overlay_rect = (VdpRect){ s->x1, s->y1, s->x2, s->y2};
        overlay_layer.struct_version = VDP_LAYER_VERSION;
        overlay_layer.source_surface = (VdpOutputSurface)s->cur_overlay_frame->data[3];
        overlay_layer.source_rect    = NULL;
        overlay_layer.destination_rect = &overlay_rect;

        layers = &overlay_layer;
        layer_count = 1;
    }

    //get output surface frame
    oFrame1 = av_frame_alloc();
    if ((ret = av_hwframe_get_buffer(s->output_hwframe, oFrame1, 0)) < 0) {
        return ret;
    }
    if ((ret = av_frame_copy_props(oFrame1, s->cur_frame)) < 0) {
        return ret;
    }

    ret = render_frame(ctx, s->mixer, picture_structure, past_surfaces,
            (VdpVideoSurface)s->cur_frame->data[3], future_surfaces, (VdpOutputSurface)oFrame1->data[3], layer_count, layers);
    if (ret < 0) {
        return ret;
    }

    if (outlink->format != AV_PIX_FMT_VDPAU_OUTPUTSURFACE) {
        if ((ret = download_frame(outlink, oFrame1)) < 0) {
            return ret;
        }
    }

    if (s->double_framerate) {
        oFrame1->pts = ((s->start_time == AV_NOPTS_VALUE) ? 0 : s->start_time) +
                      av_rescale(outlink->frame_count, s->ts_unit.num, s->ts_unit.den);

        if (picture_structure == VDP_VIDEO_MIXER_PICTURE_STRUCTURE_TOP_FIELD) {
            picture_structure = VDP_VIDEO_MIXER_PICTURE_STRUCTURE_BOTTOM_FIELD;
        } else {
            picture_structure = VDP_VIDEO_MIXER_PICTURE_STRUCTURE_TOP_FIELD;
        }

        //get output surface frame
        oFrame2 = av_frame_alloc();
        if ((ret = av_hwframe_get_buffer(s->output_hwframe, oFrame2, 0)) < 0) {
            return ret;
        }
        if ((ret = av_frame_copy_props(oFrame2, s->cur_frame)) < 0) {
            return ret;
        }

        ret = render_frame(ctx, s->mixer, picture_structure, past_surfaces,
                (VdpVideoSurface)s->cur_frame->data[3], future_surfaces, (VdpOutputSurface)oFrame2->data[3], layer_count, layers);
        if (ret < 0) {
            return ret;
        }

        oFrame2->pts = ((s->start_time == AV_NOPTS_VALUE) ? 0 : s->start_time) +
                       av_rescale(outlink->frame_count + 1, s->ts_unit.num, s->ts_unit.den);

        if (outlink->format != AV_PIX_FMT_VDPAU_OUTPUTSURFACE) {
            //download frame
            if ((ret = download_frame(outlink, oFrame2)) < 0) {
                return ret;
            }
        }
    }

    if ((ret = ff_filter_frame(outlink, oFrame1)) < 0) {
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
//    if (s->device) vdpauFuncs->vdpDeviceDestroy(s->device);
//    if (s->display) XCloseDisplay(s->display);
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
