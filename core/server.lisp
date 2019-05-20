(defpackage cl-csr-2d-game/core/server
  (:use :cl
        :cl-markup)
  (:export :start
           :stop
           :start-csr-game-loop
           :stop-csr-game-loop)
  (:import-from :cl-ps-ecs
                :ecs-main)
  (:import-from :cl-csr-2d-game/core/game-state
                :process-game-state)
  (:import-from :cl-csr-2d-game/core/initializer
                :init-default-systems)
  (:import-from :proto-cl-client-side-rendering
                :start-game-loop
                :stop-game-loop
                :ensure-js-files
                :make-src-list-for-script-tag
                :make-client-side-rendering-middleware))
(in-package :cl-csr-2d-game/core/server)

(defvar *server* nil)

(defun start (&key
                (port 5000)
                root-dir
                (init-func (lambda ()))
                (update-func (lambda ())))
  (assert root-dir)
  (stop)
  (init-default-systems)
  (start-csr-game-loop :init-func init-func
                       :update-func update-func)
  (let ((resource-dir (merge-pathnames "resource/" root-dir)))
    (init-ningle-app :resource-dir resource-dir)
    (setf *server*
          (clack:clackup
           (lack:builder
            (make-client-side-rendering-middleware
             :resource-root resource-dir)
            *ningle-app*)
           :port port))))

(defun stop ()
  (when *server*
    (clack:stop *server*)
    (setf *server* nil)
    (stop-csr-game-loop)))

;; --- static --- ;;

(defvar *ningle-app* (make-instance 'ningle:<app>))

(defun init-ningle-app (&key resource-dir)
  (setf (ningle:route *ningle-app* "/" :method :GET)
        (lambda (params)
          (declare (ignorable params))
          (ensure-js-files
           (merge-pathnames "js/" resource-dir))
          (with-output-to-string (str)
            (let ((cl-markup:*output-stream* str))
              (html5 (:head
                      (:title "A sample of client side rendering on Common Lisp")
                      (dolist (src (make-src-list-for-script-tag "js/"))
                        (markup (:script :src src nil))))
                     (:body
                      (:div (:textarea :id "js-code"
                                       :cols 80 :rows 10 :readonly t :disabled t nil))
                      (:div :id "renderer" nil)
                      (:script :src "js/client.js" nil))))))))

;; --- game loop --- ;;

(defun start-csr-game-loop (&key init-func update-func)
  (funcall init-func)
  (start-game-loop :update-func (lambda ()
                                  (process-game-state)
                                  (ecs-main)
                                  (funcall update-func))))

(defun stop-csr-game-loop ()
  (stop-game-loop))
