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
  (:use :cl :lw :hcl :capi :re :http)
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
  '(("x-icon" :ico)
    ("ico"    :ico)
    ("png"    :png)
    ("jpg"    :jpeg)
    ("jpe"    :jpeg)
    ("jpeg"   :jpeg)
    ("gif"    :gif)
    ("bmp"    :bmp))
  "Known extensions for images.")

(defclass http-image-pane (output-pane)
  ((url         :initarg :url         :reader http-image-pane-url              :initform nil)
   (error-p     :initarg :error       :reader http-image-pane-error-p          :initform nil)
   (cache-p     :initarg :cache       :reader http-image-pane-cache-p          :initform nil)
   (wait-image  :initarg :wait-image  :reader http-image-pane-wait-image       :initform nil)
   (error-image :initarg :error-image :reader http-image-pane-error-image      :initform nil)
   (image                             :reader http-image-pane-image            :initform nil)
   (process                           :reader http-image-pane-download-process :initform nil)

   ;; cached images from various URLs
   (cache       :initform nil :allocation :class))
  (:default-initargs
   :background :white
   :draw-with-buffer t
   :create-callback 'create-http-image-pane
   :destroy-callback 'destroy-http-image-pane
   :display-callback 'display-http-image-pane))

(defstruct cached-image bytes type)

(defmethod create-http-image-pane ((pane http-image-pane))
  "If the URL has been set, then download it in the background."
  (with-slots (cache url wait-image error-image)
      pane
    (unless cache
      (setf cache (make-hash-table :test #'string=)))

    ;; create the wait-image and error-image if provided
    (when wait-image
      (setf wait-image (gp:load-image pane wait-image :cache t)))
    (when error-image
      (setf error-image (gp:load-image pane error-image :cache t)))

    ;; try and download
    (when url
      (http-image-pane-refresh pane))))

(defmethod destroy-http-image-pane ((pane http-image-pane))
  "Stop downloading the image URL if it's still trying."
  (with-slots (process image wait-image error-image)
      pane
    (when process
      (mp:process-kill process))

    ;; the image might be the wait or error (don't free twice)
    (when (and image (not (or (eq image wait-image) (eq image error-image))))
      (gp:free-image pane image))

    ;; free the wait and error images
    (when wait-image
      (gp:free-image pane wait-image))
    (when error-image
      (gp:free-image pane error-image))))

(defmethod display-http-image-pane ((pane http-image-pane) &rest bounds)
  "Render the pane for an image being downloaded."
  (declare (ignore bounds))
  (with-slots (image)
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
        (gp:draw-rectangle pane 0 0 w h :filled t)))))

(defmethod (setf http-image-pane-url) (image-url (pane http-image-pane))
  "Change the URL and refresh the pane."
  (with-slots (image url process)
      pane
    (destroy-http-image-pane pane)

    ;; set the new url, clear any error, and the image
    (setf process nil
          image nil
          url image-url)

    ;; refresh
    (http-image-pane-refresh pane)))

(defmethod http-image-pane-refresh ((pane http-image-pane))
  "Downloads a URL as an image into an image-pane."
  (with-slots (cache cache-p error-p process url image wait-image)
      pane
    (when url
      (with-url (url url)
        (labels ((image-type (resp)
                   (let ((type (let ((content-type (http-header resp "Content-Type")))
                                 (when (and content-type (eql (search "image/" content-type) 0))
                                   (subseq content-type 6))))
                         (ext (pathname-type (pathname (url-path (request-url (response-request resp)))))))
                     
                     ;; use the Content-Type, if that fails, try the file extension
                     (second (or (assoc type +image-types+ :test #'string-equal)
                                 (assoc ext +image-types+ :test #'string-equal)))))
                 
                 ;; lookup cached image or download it
                 (download ()
                   (let ((cached-image (and cache-p (with-hash-table-locked cache
                                                      (gethash (format-url url) cache)))))
                     (unless cached-image
                       (handler-case
                           (with-response (resp (http-follow (http-get url) :redirect-limit 2) :timeout 10)
                             (when-let (type (image-type resp))
                               (let ((bytes (map '(vector (unsigned-byte 8)) #'char-code (response-body resp))))
                                 (setf cached-image (make-cached-image :bytes bytes :type type))
                                 (when cache-p
                                   (with-hash-table-locked cache
                                     (setf (gethash (format-url url) cache) cached-image))))))
                         (error (c)
                           (when error-p (error c)))))
                     
                     ;; create the image only if a cached-image exists or the file downloaded successfully
                     (apply-in-pane-process pane #'apply-image-data pane cached-image))))
          (setf process (mp:process-run-function "HTTP Image Download" '() #'download)))
        
        ;; set the current image to the wait image
        (setf image wait-image)))
        
    ;; redraw the pane
    (gp:invalidate-rectangle pane)))

(defmethod http-image-pane-stop ((pane http-image-pane))
  "Stop trying to download the image."
  (with-slots (process)
      pane
    (when process
      (mp:process-kill process))

    ;; reset
    (setf process nil)))

(defmethod apply-image-data ((pane http-image-pane) cached-image)
  "Given an array of bytes, load it into the image."
  (with-slots (image error-image)
      pane
    (setf image (if (null cached-image)
                    error-image
                  (with-slots (bytes type)
                      cached-image
                    (let ((ex-image (make-instance 'gp:external-image :data bytes :type type)))
                      (handler-case
                          (gp:convert-external-image pane ex-image)
                        (condition (c) error-image))))))

    ;; redraw with the new image
    (gp:invalidate-rectangle pane)))
