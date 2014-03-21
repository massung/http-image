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

(defpackage :http-image-pane
  (:use :cl :capi :http)
  (:export
   #:http-image-pane

   ;; pane methods
   #:http-image-pane-url
   #:http-image-pane-image
   #:http-image-pane-error
   #:http-image-pane-download-process
   #:http-image-pane-refresh))
 
(in-package :http-image-pane)

(defconstant +image-types+
  '(("ico"  :ico)
    ("png"  :png)
    ("jpg"  :jpg)
    ("jpeg" :jpg)
    ("gif"  :gif)
    ("bmp"  :bmp))
  "Known extensions for images.")

(defclass http-image-pane (output-pane)
  ((url     :initarg :url :reader http-image-pane-url)
   (image                 :reader http-image-pane-image            :initform nil)
   (error                 :reader http-image-pane-error            :initform nil)
   (process               :reader http-image-pane-download-process :initform nil))
  (:default-initargs
   :background :white
   :create-callback 'create-http-image-pane
   :destroy-callback 'destroy-http-image-pane
   :display-callback 'display-http-image-pane))

(defmethod create-http-image-pane ((pane http-image-pane))
  "If the URL has been set, then download it in the background."
  (when (http-image-pane-url pane)
    (http-image-pane-refresh pane)))

(defmethod destroy-http-image-pane ((pane http-image-pane))
  "Stop downloading the image URL if it's still trying."
  (with-slots (process image)
      pane
    (when process
      (mp:process-kill process))
    (when image
      (gp:free-image pane image))))

(defmethod display-http-image-pane ((pane http-image-pane) &rest bounds)
  "Render the pane for an image being downloaded."
  (declare (ignore bounds))
  (with-slots (image error)
      pane
    (let ((w (gp:port-width pane))
          (h (gp:port-height pane)))
      (if image
          (let* ((iw (gp:image-width image))
                 (ih (gp:image-height image))

                 ;; calculate the aspect ratio to render at
                 (aspect (min (min (/ w iw) 1.0)
                              (min (/ h ih) 1.0)))

                 ;; figure out the final width and height
                 (to-w (* iw aspect))
                 (to-h (* ih aspect))

                 ;; and the target location to render to
                 (x (- (/ w 2) (/ to-w 2)))
                 (y (- (/ h 2) (/ to-h 2))))

            ;; blit the image
            (gp:draw-image pane image x y :from-width iw :from-height ih :to-width to-w :to-height to-h))
        (gp:draw-rectangle pane 0 0 w h :filled t :foreground (when error :red))))))

(defmethod (setf http-image-pane-url) (image-url (pane http-image-pane))
  "Change the URL and refresh the pane."
  (with-slots (url error process)
      pane
    (destroy-http-image-pane pane)

    ;; set the new url, clear any error, and the image
    (setf url image-url
          error nil
          process nil)

    ;; refresh
    (http-image-pane-refresh pane)))

(defmethod http-image-pane-refresh ((pane http-image-pane))
  "Downloads a URL as an image into an image-pane."
  (with-slots (image process url)
      pane
    (flet ((download ()
             (with-url (url url)
               (with-response (resp (http-get url))
                 (let* ((bytes (map '(vector (unsigned-byte 8)) #'char-code (response-body resp)))
                        (ext (pathname-type (pathname (url-path url))))
                        (type (second (assoc ext +image-types+ :test #'string-equal))))
                   (apply-in-pane-process pane #'apply-image-data pane bytes type))))))
      (setf process (mp:process-run-function "HTTP Image Download" '() #'download))
      (setf image nil))

    ;; redraw with no image
    (prog1
        url
      (gp:invalidate-rectangle pane))))

(defmethod http-image-pane-stop ((pane http-image-pane))
  "Stop trying to download the image."
  (with-slots (process)
      pane
    (when process
      (mp:process-kill pane)

      ;; reset
      (setf process nil))))

(defmethod apply-image-data ((pane http-image-pane) bytes &optional type)
  "Given an array of bytes, load it into the image."
  (with-slots (image error)
      pane
    (let ((external-image (make-instance 'gp:external-image :data bytes :type type)))
      (handler-case
          (progn
            (sys:atomic-exchange image (gp:convert-external-image pane external-image))
    
            ;; refresh the pane
            (gp:invalidate-rectangle pane))
        (condition (c)
          (sys:atomic-exchange error (cons external-image c)))))))
