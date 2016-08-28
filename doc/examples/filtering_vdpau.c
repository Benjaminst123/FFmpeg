/*
 * Copyright (c) 2010 Nicolas George
 * Copyright (c) 2011 Stefano Sabatini
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

/**
 * @file
 * API example for decoding and filtering with vdpau
 * @example filtering_vdpau.c
 */



#define _XOPEN_SOURCE 600 /* for usleep */
#include <unistd.h>
#include <stdint.h>

#include <vdpau/vdpau.h>
#include <vdpau/vdpau_x11.h>
#include <X11/Xlib.h>
#include <libavcodec/vdpau.h>

#include <libavutil/avassert.h>
#include <libavutil/buffer.h>
#include <libavutil/frame.h>
#include <libavutil/hwcontext.h>
#include <libavutil/hwcontext_vdpau.h>
#include <libavutil/pixfmt.h>
#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libavfilter/avfiltergraph.h>
#include <libavfilter/buffersink.h>
#include <libavfilter/buffersrc.h>
#include <libavutil/opt.h>

const char *filter_descr = "scale=78:24,transpose=cclock";
/* other way:
   scale=78:24 [scl]; [scl] transpose=cclock // assumes "[in]" and "[out]" to be input output pads respectively
 */

static AVFormatContext *fmt_ctx;
static AVCodecContext *dec_ctx;
AVFilterContext *buffersink_ctx;
AVFilterContext *buffersrc_ctx;
AVFilterContext *vdpau_ctx;
AVFilterGraph *filter_graph;
static int video_stream_index = -1;
static int64_t last_pts = AV_NOPTS_VALUE;

AVBufferRef          *device_ref = NULL;

typedef struct VDPAUContext {
    AVBufferRef *hw_frames_ctx;
} VDPAUContext;

typedef struct VDPAUHWDevicePriv {
    VdpDeviceDestroy *device_destroy;
    Display *dpy;
} VDPAUHWDevicePriv;

static void device_free(AVHWDeviceContext *ctx)
{
    AVVDPAUDeviceContext *hwctx = ctx->hwctx;
    VDPAUHWDevicePriv     *priv = ctx->user_opaque;

    if (priv->device_destroy)
        priv->device_destroy(hwctx->device);
    if (priv->dpy)
        XCloseDisplay(priv->dpy);
    av_freep(&priv);
}

static void vdpau_uninit(AVCodecContext *s)
{
    VDPAUContext *ctx = s->opaque;


    av_buffer_unref(&ctx->hw_frames_ctx);

    //av_freep(&ist->hwaccel_ctx);
    av_freep(&s->hwaccel_context);
}

static int vdpau_get_buffer(AVCodecContext *s, AVFrame *frame, int flags)
{
    VDPAUContext        *ctx = s->opaque;

    return av_hwframe_get_buffer(ctx->hw_frames_ctx, frame, 0);
}

static int vdpau_alloc(AVCodecContext *s)
{
    int loglevel = AV_LOG_ERROR;
    VDPAUContext *ctx;
    const char *display, *vendor;
    VdpStatus err;
    int ret;

    VdpDevice                device;
    VdpGetProcAddress       *get_proc_address;
    VdpGetInformationString *get_information_string;

    VDPAUHWDevicePriv    *device_priv = NULL;
    AVHWDeviceContext    *device_ctx;
    AVVDPAUDeviceContext *device_hwctx;
    AVHWFramesContext    *frames_ctx;

    ctx = av_mallocz(sizeof(*ctx));
    if (!ctx)
        return AVERROR(ENOMEM);

    device_priv = av_mallocz(sizeof(*device_priv));
    if (!device_priv) {
        av_freep(&ctx);
        goto fail;
    }

    device_priv->dpy = XOpenDisplay(":0");
    if (!device_priv->dpy) {
        av_log(NULL, loglevel, "Cannot open the X11 display %s.\n",
               XDisplayName(":0"));
        goto fail;
    }
    display = XDisplayString(device_priv->dpy);

    err = vdp_device_create_x11(device_priv->dpy, XDefaultScreen(device_priv->dpy),
                                &device, &get_proc_address);
    if (err != VDP_STATUS_OK) {
        av_log(NULL, loglevel, "VDPAU device creation on X11 display %s failed.\n",
               display);
        goto fail;
    }

#define GET_CALLBACK(id, result)                                                \
do {                                                                            \
    void *tmp;                                                                  \
    err = get_proc_address(device, id, &tmp);                                   \
    if (err != VDP_STATUS_OK) {                                                 \
        av_log(NULL, loglevel, "Error getting the " #id " callback.\n");        \
        goto fail;                                                              \
    }                                                                           \
    result = tmp;                                                               \
} while (0)

    GET_CALLBACK(VDP_FUNC_ID_GET_INFORMATION_STRING, get_information_string);
    GET_CALLBACK(VDP_FUNC_ID_DEVICE_DESTROY,         device_priv->device_destroy);

    device_ref = av_hwdevice_ctx_alloc(AV_HWDEVICE_TYPE_VDPAU);
    if (!device_ref)
        goto fail;
    device_ctx                     = (AVHWDeviceContext*)device_ref->data;
    device_hwctx                   = device_ctx->hwctx;
    device_ctx->user_opaque        = device_priv;
    device_ctx->free               = device_free;
    device_hwctx->device           = device;
    device_hwctx->get_proc_address = get_proc_address;

    device_priv = NULL;

    ret = av_hwdevice_ctx_init(device_ref);
    if (ret < 0)
        goto fail;

    ctx->hw_frames_ctx = av_hwframe_ctx_alloc(device_ref);
    if (!ctx->hw_frames_ctx)
        goto fail;
    //av_buffer_unref(&device_ref);

    frames_ctx            = (AVHWFramesContext*)ctx->hw_frames_ctx->data;
    frames_ctx->format    = AV_PIX_FMT_VDPAU;
    frames_ctx->sw_format = s->sw_pix_fmt;
    frames_ctx->width     = 1920;
    frames_ctx->height    = 1080;

    ret = av_hwframe_ctx_init(ctx->hw_frames_ctx);
    if (ret < 0)
        goto fail;

    if (av_vdpau_bind_context(s, device, get_proc_address, 0))
        goto fail;

    s->opaque = ctx;

    return 0;

fail:
    if (device_priv) {
        if (device_priv->device_destroy)
            device_priv->device_destroy(device);
        if (device_priv->dpy)
            XCloseDisplay(device_priv->dpy);
    }
    av_freep(&device_priv);
    av_buffer_unref(&device_ref);
    vdpau_uninit(s);
    return AVERROR(EINVAL);
}

static enum AVPixelFormat get_format(AVCodecContext *s, const enum AVPixelFormat *pix_fmts)
{
    const enum AVPixelFormat *p;
    for (p = pix_fmts; *p != -1; p++) {
        if (*p == AV_PIX_FMT_VDPAU) {
            return *p;
        }
    }

    return AV_PIX_FMT_NONE;
}


static int open_input_file(const char *filename)
{
    int ret;
    AVCodec *dec;

    if ((ret = avformat_open_input(&fmt_ctx, filename, NULL, NULL)) < 0) {
        av_log(NULL, AV_LOG_ERROR, "Cannot open input file\n");
        return ret;
    }

    if ((ret = avformat_find_stream_info(fmt_ctx, NULL)) < 0) {
        av_log(NULL, AV_LOG_ERROR, "Cannot find stream information\n");
        return ret;
    }

    /* select the video stream */
    ret = av_find_best_stream(fmt_ctx, AVMEDIA_TYPE_VIDEO, -1, -1, &dec, 0);
    if (ret < 0) {
        av_log(NULL, AV_LOG_ERROR, "Cannot find a video stream in the input file\n");
        return ret;
    }
    video_stream_index = ret;
    dec_ctx = fmt_ctx->streams[video_stream_index]->codec;
    av_opt_set_int(dec_ctx, "refcounted_frames", 1, 0);

    if ((ret = vdpau_alloc(dec_ctx)) < 0) {
        return ret;
    }

    dec_ctx->get_format            = get_format;
    dec_ctx->get_buffer2           = vdpau_get_buffer;
    dec_ctx->thread_safe_callbacks = 1;

    /* init the video decoder */
    if ((ret = avcodec_open2(dec_ctx, dec, NULL)) < 0) {
        av_log(NULL, AV_LOG_ERROR, "Cannot open video decoder\n");
        return ret;
    }

    return 0;
}

static int init_filters(const char *filters_descr)
{
    char args[512];
    int ret = 0;
    AVFilter *buffersrc  = avfilter_get_by_name("buffer");
    AVFilter *buffersink = avfilter_get_by_name("buffersink");
    AVFilterInOut *outputs = avfilter_inout_alloc();
    AVFilterInOut *inputs  = avfilter_inout_alloc();
    AVRational time_base = fmt_ctx->streams[video_stream_index]->time_base;
    enum AVPixelFormat pix_fmts[] = { AV_PIX_FMT_GRAY8, AV_PIX_FMT_NONE };
    AVFilter *vdpau = avfilter_get_by_name("vdpau");

    filter_graph = avfilter_graph_alloc();
    if (!outputs || !inputs || !filter_graph) {
        ret = AVERROR(ENOMEM);
        goto end;
    }

    /* buffer video source: the decoded frames from the decoder will be inserted here. */
    snprintf(args, sizeof(args),
            "video_size=%dx%d:pix_fmt=%d:time_base=%d/%d:pixel_aspect=%d/%d",
            dec_ctx->width, dec_ctx->height, dec_ctx->pix_fmt,
            time_base.num, time_base.den,
            dec_ctx->sample_aspect_ratio.num, dec_ctx->sample_aspect_ratio.den);

    ret = avfilter_graph_create_filter(&buffersrc_ctx, buffersrc, "in",
                                       args, NULL, filter_graph);
    if (ret < 0) {
        av_log(NULL, AV_LOG_ERROR, "Cannot create buffer source\n");
        goto end;
    }

    /* buffer video sink: to terminate the filter chain. */
    ret = avfilter_graph_create_filter(&buffersink_ctx, buffersink, "out",
                                       NULL, NULL, filter_graph);
    if (ret < 0) {
        av_log(NULL, AV_LOG_ERROR, "Cannot create buffer sink\n");
        goto end;
    }

    ret = av_opt_set_int_list(buffersink_ctx, "pix_fmts", pix_fmts,
                              AV_PIX_FMT_NONE, AV_OPT_SEARCH_CHILDREN);
    if (ret < 0) {
        av_log(NULL, AV_LOG_ERROR, "Cannot set output pixel format\n");
        goto end;
    }


    ret = avfilter_graph_create_filter(&vdpau_ctx, vdpau, "vdpau",
            NULL, NULL, filter_graph);

    vdpau_ctx->hw_device_ctx = device_ref;

    if (ret < 0) {
        av_log(NULL, AV_LOG_ERROR, "Cannot create vdpau filter.\n");
        goto end;
    }


    /* Connect the filters;
     * in this simple case the filters just form a linear chain. */
    ret = avfilter_link(buffersrc_ctx, 0, vdpau_ctx, 0);
    if (ret >= 0)
        ret = avfilter_link(vdpau_ctx, 0, buffersink_ctx, 0);
    if (ret < 0) {
        fprintf(stderr, "Error connecting filters\n");
        return ret;
    }

    if ((ret = avfilter_graph_config(filter_graph, NULL)) < 0)
        goto end;

end:
    avfilter_inout_free(&inputs);
    avfilter_inout_free(&outputs);

    return ret;
}

static void display_frame(const AVFrame *frame, AVRational time_base)
{
    int x, y;
    uint8_t *p0, *p;
    int64_t delay;

    if (frame->pts != AV_NOPTS_VALUE) {
        if (last_pts != AV_NOPTS_VALUE) {
            /* sleep roughly the right amount of time;
             * usleep is in microseconds, just like AV_TIME_BASE. */
            delay = av_rescale_q(frame->pts - last_pts,
                                 time_base, AV_TIME_BASE_Q);
            if (delay > 0 && delay < 1000000)
                usleep(delay);
        }
        last_pts = frame->pts;
    }

    /* Trivial ASCII grayscale display. */
    p0 = frame->data[0];
    puts("\033c");
    for (y = 0; y < frame->height; y++) {
        p = p0;
        for (x = 0; x < frame->width; x++)
            putchar(" .-+#"[*(p++) / 52]);
        putchar('\n');
        p0 += frame->linesize[0];
    }
    fflush(stdout);
}

int main(int argc, char **argv)
{
    int ret;
    AVPacket packet;
    AVFrame *frame = av_frame_alloc();
    AVFrame *filt_frame = av_frame_alloc();
    int got_frame;

    if (!frame || !filt_frame) {
        perror("Could not allocate frame");
        exit(1);
    }
    if (argc != 2) {
        fprintf(stderr, "Usage: %s file\n", argv[0]);
        exit(1);
    }

    av_register_all();
    avfilter_register_all();

    if ((ret = open_input_file(argv[1])) < 0)
        goto end;
    if ((ret = init_filters(filter_descr)) < 0)
        goto end;

    /* read all packets */
    while (1) {
        if ((ret = av_read_frame(fmt_ctx, &packet)) < 0)
            break;

        if (packet.stream_index == video_stream_index) {
            got_frame = 0;
            ret = avcodec_decode_video2(dec_ctx, frame, &got_frame, &packet);
            if (ret < 0) {
                av_log(NULL, AV_LOG_ERROR, "Error decoding video\n");
                break;
            }

            if (got_frame) {
                frame->pts = av_frame_get_best_effort_timestamp(frame);

                /* push the decoded frame into the filtergraph */
                if (av_buffersrc_add_frame_flags(buffersrc_ctx, frame, AV_BUFFERSRC_FLAG_KEEP_REF) < 0) {
                    av_log(NULL, AV_LOG_ERROR, "Error while feeding the filtergraph\n");
                    break;
                }

                /* pull filtered frames from the filtergraph */
                while (1) {
                    ret = av_buffersink_get_frame(buffersink_ctx, filt_frame);
                    if (ret == AVERROR(EAGAIN) || ret == AVERROR_EOF)
                        break;
                    if (ret < 0)
                        goto end;
                    display_frame(filt_frame, buffersink_ctx->inputs[0]->time_base);
                    av_frame_unref(filt_frame);
                }
                av_frame_unref(frame);
            }
        }
        av_packet_unref(&packet);
    }
end:
    avfilter_graph_free(&filter_graph);
    avcodec_close(dec_ctx);
    avformat_close_input(&fmt_ctx);
    av_frame_free(&frame);
    av_frame_free(&filt_frame);

    if (ret < 0 && ret != AVERROR_EOF) {
        fprintf(stderr, "Error occurred: %s\n", av_err2str(ret));
        exit(1);
    }

    exit(0);
}

