(in-package :cl-user)
(defpackage cl-csr-2d-game/graphics/draw-model-system
  (:use :cl
        :cl-ps-ecs
        :cl-csr-2d-game/utils/calc)
  (:export :make-model-2d
           :model-2d
           :model-2d-p
           :model-2d-mesh
           :model-2d-depth
           :model-2d-offset
           :model-2d-geometry
           :model-2d-label
           :model-2d-target-client-id-list
           :enable-model-2d
           :disable-model-2d
           :init-draw-model-system
           :update-model-2d
           :find-model-2d-by-label)
  (:import-from :cl-csr-2d-game/core/basic-components
                :point-2d
                :point-2d-x
                :point-2d-y
                :point-2d-angle
                :make-point-2d)
  (:import-from :proto-cl-client-side-rendering
                :*target-client-id-list*))
(in-package :cl-csr-2d-game/graphics/draw-model-system)

;; --- component --- ;;

(defvar *max-draw-object-id* 0)

;; The mesh is represented as a function to draw model.
(defstruct (model-2d (:include ecs-component))
  (id (incf *max-draw-object-id*))
  mesh
  (depth 0)
  (offset (make-point-2d))
  (enable-p t)
  (target-client-id-list :all)
  label)

;; --- system --- ;;

(defstruct
    (draw-model-system
     (:include ecs-system
               (target-component-types '(point-2d model-2d))
               (process (lambda (entity)
                          (process-in-draw-model-system entity))))))

(defun process-in-draw-model-system (entity)
  (do-ecs-components-of-entity
      (modelc entity :component-type 'model-2d)
    (when (model-2d-enable-p modelc)
      (with-slots (id mesh depth offset target-client-id-list) modelc
        (let ((*target-client-id-list* target-client-id-list)
              (pnt (calc-global-point entity offset)))
          (funcall mesh
                   :id id
                   :x (point-2d-x pnt)
                   :y (point-2d-y pnt)
                   :rotate (point-2d-angle pnt)
                   :depth depth))))))

(defun modify-model-enable (entity enable-p target-model-2d)
  ;; TODO: Check the entity has the target-model-2d if not nil
  (declare (ignore entity))
  (setf (model-2d-enable-p target-model-2d)
        enable-p))

(defun enable-model-2d (entity &key target-model-2d)
  (modify-model-enable entity t target-model-2d))

(defun disable-model-2d (entity &key target-model-2d)
  (modify-model-enable entity nil target-model-2d))

(defun update-model-2d (entity old-model new-model)
  "Update old-model by new-model."
  (delete-ecs-component old-model entity)
  (add-ecs-component new-model entity))

(defun find-model-2d-by-label (entity label)
  (do-ecs-components-of-entity
      (model entity :component-type 'model-2d)
    (when (eq (model-2d-label model) label)
      (return-from find-model-2d-by-label model)))
  (error "Can't find the label: ~A" label))

(defun init-draw-model-system ()
  (make-draw-model-system))
