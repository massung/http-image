# CAPI HTTP Image Output Pane for LispWorks

The `http-image-pane` package is a trivial subclass of `output-pane` that handles downloading and image from the web in a background process and displaying it in an output pane (fitting and maintaining aspect ratio).

It uses of my [`http`](http://github.com/massung/http) package to download the image, which is also freely available.

## Quickstart

After loading the package, using the pane is very simple:

	CL-USER > (capi:contain (make-instance 'http-image-pane :url "http://www.lisperati.com/lisplogo_fancy_256.png"))
	
At this point a window should appear and a background process is spun up to download the image in the background. Once loaded, it should appear. The pane should be resizeable and the image should resize to fit, maintaining its original aspect ratio.

![Made with secret alien technology](http://github.com/massung/http-image-pane/screenshot.png)

Since `http-image-pane` is just a subclass of `output-pane`, all the other `:default-initargs` like `:background` and `:input-model` can be used to manipulate the pane.

If you want to change the URL being pointed to, simply use `#'(setf http-image-pane-url)` with a new URL. And if you want to download the current image again, use `http-image-pane-refresh`.

	CL-USER > (setf (http-image-pane-url *) "http://www.lisperati.com/lisplogo_flag2_256.png")