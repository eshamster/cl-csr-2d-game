(defpackage sample-cl-csr-2d-game/server
  (:use :cl
        :cl-markup)
  (:export :start-sample
           :stop-sample
           :get-sample-list
           :get-current-sample)
  (:import-from :cl-csr-2d-game
                :start
                :stop
                :get-ningle-app)
  (:import-from :sample-cl-csr-2d-game/sample-anime
                :init-sample-anime
                :update-sample-anime)
  (:import-from :sample-cl-csr-2d-game/sample-basic
                :init-sample-basic
                :update-sample-basic)
  (:import-from :sample-cl-csr-2d-game/sample-debug-draw
                :init-sample-debug-draw
                :update-sample-debug-draw)
  (:import-from :cl-csr
                :ensure-js-files
                :make-src-list-for-script-tag
                :make-client-side-rendering-middleware)
  (:import-from :cl-ps-ecs
                :make-ecs-entity
                :delete-ecs-entity
                :stack-default-ecs-entity-parent
                :pop-default-ecs-entity-parent
                :register-next-frame-func)
  (:import-from :alexandria
                :hash-table-keys
                :plist-hash-table))
(in-package :sample-cl-csr-2d-game/server)

(defvar *parent-entity* nil)

(defparameter *sample-func-table*
  (plist-hash-table '(:basic (init-sample-basic update-sample-basic)
                      :anime (init-sample-anime update-sample-anime)
                      :debug-draw (init-sample-debug-draw update-sample-debug-draw))))

(defvar *current-sample* nil)

(defvar *current-port* 5000)

(defun get-sample-list ()
  (hash-table-keys *sample-func-table*))

(defun get-current-sample ()
  *current-sample*)

(defun get-sample-funcs (type)
  (let ((result (gethash type *sample-func-table*)))
    (unless result
      (error "\"~S\" is not recognized sample type. ~S are allowed."
             type (get-sample-list)))
    result))

(defun start-sample (&key (port *current-port*) (type :basic))
  (stop-sample)
  (setf *parent-entity* (make-ecs-entity))
  (stack-default-ecs-entity-parent *parent-entity*)
  (let ((init-func (car (get-sample-funcs type)))
        (update-func (cadr (get-sample-funcs type))))
    (start :port port
           :root-dir (asdf:component-pathname
                      (asdf:find-system :sample-cl-csr-2d-game))
           :init-func init-func
           :update-func update-func))
  (setf *current-sample* type
        *current-port* port))

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
  (setf *current-sample* nil)
  (stop))

