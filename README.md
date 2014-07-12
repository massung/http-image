# CAPI HTTP Image Pane for LispWorks

The `http-image` package is a process-driven image downloader and CAPI `output-pane` that can render downloaded images.

It uses of my [`http`](http://github.com/massung/http) package to download the image, which is also freely available.

## Quickstart

The `http-image` system is divided into two parts:

* Functions for downloading images in background processes.
* An `output-pane` subclass for rendering images.

To download an image, simply call `http-image-download` with a URL and a callback function. The signature of the callback function should be:

	(external-image &optional error)

If *external-image* is `nil` then *error* will have the condition that was caught. Otherwise, *external-image* can be used per the [LispWorks documentation](http://www.lispworks.com/documentation/lw61/CAPRM/html/capiref-537.htm#pgfId-1169341).

	(http-image-download url callback &key reload (timeout 30)) => process

If *reload* is `T` then the image will be forced to download. Otherwise, if the image has been downloaded once already, the previous (cached) version will be returned.

The entire download cache can be cleared at any time with `http-image-cache-clear`. This can be quite useful for debugging, but would rarely be called in production use.

## The `http-image-pane` CAPI Pane

Wrapping the `http-image-download` function for you is the `http-image-pane`. Simply create one with a URL:

	CL-USER > (capi:contain (make-instance 'http-image-pane :url "http://www.lisperati.com/lisplogo_fancy_256.png"))

At this point a window should appear and a background process is spun up to download the image in the background. Once loaded, it should appear. The pane should be resizeable and the image should resize to fit, maintaining its original aspect ratio.

![Made with secret alien technology](http://raw.github.com/massung/http-image-pane/master/screenshot.png)

Since `http-image-pane` is just a subclass of `output-pane`, all the other `:default-initargs` like `:background` and `:input-model` can be used to manipulate the pane.

*Note: The `:create-callback`, `:destroy-callback`, and `:display-callback` options are used by the `http-image-pane`. Take care if you wish to do something different!*

If you want to change the URL being pointed to, simply use `#'(setf http-image-pane-url)` with a new URL. And if you want to download the current image again, use `http-image-pane-refresh`.

	CL-USER > (setf (http-image-pane-url *) "http://www.lisperati.com/lisplogo_flag2_256.png")