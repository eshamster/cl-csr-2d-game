(defpackage sample-cl-csr-2d-game/server
  (:use :cl
        :cl-markup)
  (:export :start-sample
           :stop-sample)
  (:import-from :cl-csr-2d-game
                :start
                :stop)
  (:import-from :sample-cl-csr-2d-game/process
                :init-sample
                :update-sample)
  (:import-from :proto-cl-client-side-rendering
                :ensure-js-files
                :make-src-list-for-script-tag
                :make-client-side-rendering-middleware)
  (:import-from :cl-ps-ecs
                :make-ecs-entity
                :delete-ecs-entity
                :stack-default-ecs-entity-parent
                :pop-default-ecs-entity-parent
                :register-next-frame-func))
(in-package :sample-cl-csr-2d-game/server)

(defvar *parent-entity* nil)

(defun start-sample (&key (port 5000))
  (stop-sample)
  (setf *parent-entity* (make-ecs-entity))
  (stack-default-ecs-entity-parent *parent-entity*)
  (start :port port
         :root-dir (asdf:component-pathname
                    (asdf:find-system :sample-cl-csr-2d-game))
         :init-func (lambda () (init-sample))
         :update-func (lambda () (update-sample))))

(defun stop-sample ()
  (when *parent-entity*
    (let ((parent (pop-default-ecs-entity-parent)))
      (assert (eq *parent-entity* parent))
      (register-next-frame-func
       (lambda () (print
                   (let ((count 0))
                     (cl-ps-ecs::do-ecs-entities var
                       (incf count))
                     count))
               (delete-ecs-entity parent))))
    (setf *parent-entity* nil))
  (stop))

