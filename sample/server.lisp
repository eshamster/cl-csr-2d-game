(defpackage sample-cl-csr-2d-game/server
  (:use :cl
        :cl-markup)
  (:export :start-sample
           :stop-sample)
  (:import-from :cl-csr-2d-game
                :start
                :stop)
  (:import-from :sample-cl-csr-2d-game/process
                :update-sample)
  (:import-from :proto-cl-client-side-rendering
                :ensure-js-files
                :make-src-list-for-script-tag
                :make-client-side-rendering-middleware))
(in-package :sample-cl-csr-2d-game/server)

(defun start-sample (&key (port 5000))
  (start :port port
         :root-dir (asdf:component-pathname
                    (asdf:find-system :sample-cl-csr-2d-game))
         :update-func (lambda () (update-sample))))

(defun stop-sample ()
  (stop))

