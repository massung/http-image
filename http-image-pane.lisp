;;;; CAPI HTTP Image Output Pane for LispWorks
;;;;
;;;; Copyright (c) 2013 by Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(in-package :http-image)

(defclass http-image-pane (capi:output-pane)
  ((process  :initform nil :reader http-image-pane-process)

   ;; the url of the image to download
   (url      :initform nil :reader http-image-pane-url      :initarg :url)

   ;; the current image loaded or an error if failed to download
   (image    :initform nil :reader http-image-pane-image    :initarg :image)
   (error    :initform nil :reader http-image-pane-error    :initarg :error)

   ;; how the image should be drawn
   (best-fit :initform t   :reader http-image-pane-fit-pane :initarg :fit-pane))
  (:default-initargs
   :background :white
   :draw-with-buffer t
   :create-callback 'create-http-image-pane
   :destroy-callback 'destroy-http-image-pane
   :display-callback 'display-http-image-pane
   :resize-callback 'gp:invalidate-rectangle))

(defmethod create-http-image-pane ((pane http-image-pane))
  "If the URL has been set, then download it in the background."
  (when (http-image-pane-url pane)
    (http-image-pane-refresh pane)))

(defmethod destroy-http-image-pane ((pane http-image-pane))
  "Stop downloading the image URL if it's still trying."
  (http-image-pane-clear pane nil))

(defmethod display-http-image-pane ((pane http-image-pane) &rest bounds)
  "Render the pane for an image being downloaded."
  (declare (ignore bounds))
  (with-slots (image best-fit)
      pane
    (let ((w (gp:port-width pane))
          (h (gp:port-height pane)))
      (if image
          (let* ((iw (gp:image-width image))
                 (ih (gp:image-height image))

                 ;; calculate the aspect ratio to render at
                 (aspect (funcall (if best-fit #'min #'max)
                                  (min (/ w iw) 1.0)
                                  (min (/ h ih) 1.0)))

                 ;; figure out the final width and height
                 (to-w (* iw aspect))
                 (to-h (* ih aspect))

                 ;; and the target location to render to, if fixed, draw in the middle
                 (x (- (/ w 2) (/ to-w 2)))
                 (y (- (/ h 2) (/ to-h 2))))

            ;; blit the image
            (gp:draw-image pane image x y :from-width iw :from-height ih :to-width to-w :to-height to-h))
        (gp:draw-rectangle pane 0 0 w h :filled t)))))

(defmethod http-image-pane-refresh ((pane http-image-pane))
  "Downloads a URL as an image into an image-pane."
  (with-slots (process url image error)
      pane
    (labels ((apply-image (external-image)
               (setf image (handler-case
                               (gp:convert-external-image pane external-image)
                             (condition (c)
                               (prog1 nil (setf error c)))))

               ;; redraw with the new image
               (gp:invalidate-rectangle pane))

             ;; called on an error or when the download completes
             (download-complete (external-image &optional condition)
               (if condition
                   (setf error condition)
                 (capi:apply-in-pane-process pane #'apply-image external-image))))

      ;; wipe current data
      (http-image-pane-clear pane nil)

      ;; start downloading the new image
      (when url
        (setf process (http-image-download url #'download-complete)))
      
      ;; redraw the pane
      (gp:invalidate-rectangle pane))))

(defmethod http-image-pane-clear ((pane http-image-pane) &optional (redraw t))
  "Kills any pending download, frees the image."
  (with-slots (process image error)
      pane
    (when process
      (mp:process-kill process))
    (when image
      (gp:free-image pane image))

    ;; clear data
    (setf process nil image nil error nil)

    ;; refresh if desired
    (when redraw
      (gp:invalidate-rectangle pane))))

(defmethod (setf http-image-pane-url) (image-url (pane http-image-pane))
  "Change the URL and refresh the pane."
  (http-image-pane-clear pane)

  ;; set the new url
  (setf (slot-value pane 'url) image-url)

  ;; refresh
  (http-image-pane-refresh pane))

(defmethod (setf http-image-pane-fit-pane) (fit (pane http-image-pane))
  "Change the draw style."
  (setf (slot-value pane 'best-fit) fit)

  ;; redraw
  (gp:invalidate-rectangle pane))

