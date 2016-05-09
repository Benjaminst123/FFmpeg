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
#include "avfilter.h"
#include "formats.h"
#include "internal.h"
#include "video.h"

typedef struct {
    const AVClass *class;

    Display *display;
    char *display_name;

    VdpDevice         device;
    VdpGetProcAddress *get_proc_address;

    VdpVideoMixer mixer;
    VdpVideoMixerCreate *vdpVideoMixerCreate;
    VdpVideoMixerSetFeatureEnables *vdpVideoMixerSetFeatureEnables;

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
} VdpauContext;

#define OFFSET(x) offsetof(DetelecineContext, x)
#define FLAGS AV_OPT_FLAG_FILTERING_PARAM|AV_OPT_FLAG_VIDEO_PARAM

static const AVOption vdpau_options[] = {
    {NULL}
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

static av_cold int init(AVFilterContext *ctx)
{
    VdpauContext *s = ctx->priv;
    VdpStatus ret;



    s->display = 0;
    s->device  = 0;
    s->mixer   = 0;

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

    VdpVideoMixerAttribute attributes[] = {
        VDP_VIDEO_MIXER_FEATURE_SHARPNESS
    };
    const float sharpness = 1;
    const void *attribute_values[] = {
       &sharpness
    };

    VdpBool enables[] = {
        VDP_TRUE
    };

    VdpVideoMixerFeature features[] = {
        VDP_VIDEO_MIXER_FEATURE_SHARPNESS
    };
    VdpVideoMixerParameter parameters[] = {
        VDP_VIDEO_MIXER_PARAMETER_VIDEO_SURFACE_WIDTH,
        VDP_VIDEO_MIXER_PARAMETER_VIDEO_SURFACE_HEIGHT,
    };
    const void *parameter_values[] = {
        &inlink->w,
        &inlink->h
    };

    ret = s->vdpVideoMixerCreate(s->device, 1, features, 2, parameters, parameter_values, &s->mixer);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU mixer creation on X11 display %s failed: %s\n",
                s->display_name, s->vdpGetErrorString(ret));
        return -1;
    }

    ret = s->vdpVideoMixerSetFeatureEnables(s->mixer, 1, attributes, enables);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU mixer set feature on X11 display %s failed: %s\n",
                s->display_name, s->vdpGetErrorString(ret));
        return -1;
    }

    ret = s->vdpVideoMixerSetAttributeValues(s->mixer, 1, attributes, attribute_values);

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
//    AVFilterContext *ctx = outlink->src;
//    VdpauContext *s = ctx->priv;
//    const AVFilterLink *inlink = ctx->inputs[0];

    return 0;
}

static int filter_frame(AVFilterLink *inlink, AVFrame *frame)
{
    AVFilterContext *ctx = inlink->dst;
    AVFilterLink *outlink = ctx->outputs[0];
    VdpauContext *s = ctx->priv;
    VdpStatus ret;
    AVFrame *oFrame;

    ret = s->vdpVideoSurfacePutBitsYCbCr(s->videosSurface, VDP_YCBCR_FORMAT_YV12, (const void * const*)frame->data, frame->linesize);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU vdpVideoSurfacePutBitsYCbCr on X11 display %s failed: %s\n",
                s->display_name, s->vdpGetErrorString(ret));
        return AVERROR_UNKNOWN;
    }

    ret = s->vdpVideoMixerRender(s->mixer,
                                 VDP_INVALID_HANDLE,
                                 NULL,
                                 VDP_VIDEO_MIXER_PICTURE_STRUCTURE_FRAME,
                                 0,
                                 NULL,
                                 s->videosSurface,
                                 0,
                                 NULL,
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
    av_frame_copy_props(oFrame, frame);


    s->vdpOutputSurfaceGetBitsNative(s->outputSurface, NULL, (void * const*)oFrame->data, oFrame->linesize);
    if (ret != VDP_STATUS_OK) {
        av_log(ctx, AV_LOG_ERROR, "VDPAU vdpOutputSurfaceGetBitsNative on X11 display %s failed: %s\n",
                s->display_name, s->vdpGetErrorString(ret));
        return AVERROR_UNKNOWN;
    }

    av_frame_free(&frame);
    return ff_filter_frame(outlink, oFrame);
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
