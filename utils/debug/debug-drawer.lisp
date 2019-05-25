(in-package :cl-user)
(defpackage cl-csr-2d-game/utils/debug/debug-drawer
  (:use :cl
        :cl-ps-ecs
        :cl-csr-2d-game/core/basic-components
        :cl-csr-2d-game/graphics/2d-geometry
        :cl-csr-2d-game/graphics/draw-model-system)
  (:export :draw-debug-point
           :draw-debug-point-by-time
           :draw-debug-line
           :draw-debug-line-by-time
           :*standard-debug-point-r*
           :*standard-debug-color*
           :*standard-debug-depth*))
(in-package :cl-csr-2d-game/utils/debug/debug-drawer)

;; --- Draw debug models --- ;;

;; - Basic - ;;

(defun draw-debug-model (&key model point
                           (tag-list '())
                           (parent nil)
                           fn-delete-condition)
  (check-type model model-2d)
  (check-type point vector-2d)
  (when parent
    (check-type parent ecs-entity))
  (let ((entity (make-ecs-entity))
        (true-point (if (typep point 'point-2d)
                        point
                        (make-point-2d :x (vector-2d-x point)
                                       :y (vector-2d-y point)))))
    (dolist (tag tag-list)
      (add-entity-tag entity tag))
    (add-ecs-component-list
     entity true-point model
     (make-script-2d
      :func (lambda (entity)
              (when (funcall fn-delete-condition entity)
                (register-next-frame-func
                 (lambda () (delete-ecs-entity entity)))))))
    (register-next-frame-func
     (lambda ()
       (if parent
           (add-ecs-entity entity parent)
           (add-ecs-entity entity))))))

(defvar *standard-debug-color* #xff0000)
(defvar *standard-debug-depth* 100)

;; -- Point -- ;;

(defvar *standard-debug-point-r* 4)

(defun draw-debug-point (&key point
                           (tag-list '())
                           (parent nil)
                           (r *standard-debug-point-r*)
                           fn-delete-condition)
  (draw-debug-model
   :model (make-model-2d :mesh (make-circle-mesh
                                :r r :color *standard-debug-color*)
                         :depth *standard-debug-depth*)
   :point point
   :tag-list tag-list
   :parent parent
   :fn-delete-condition fn-delete-condition))

(defun draw-debug-point-by-time (&key point
                                   (tag-list '())
                                   (parent nil)
                                   (r *standard-debug-point-r*)
                                   (time 60))
  (draw-debug-point
   :point point
   :tag-list tag-list
   :parent parent
   :r r
   :fn-delete-condition (make-fn-timer-condition time)))

;; - Line - ;;

(defun draw-debug-line (&key point1 point2
                          (tag-list '())
                          (parent nil)
                          fn-delete-condition)
  (draw-debug-model
   :model (make-model-2d :mesh (make-line-mesh-by-vector
                                :vec1 point1 :vec2 point2
                                :color *standard-debug-color*)
                         :depth *standard-debug-depth*)
   :point (make-point-2d)
   :tag-list tag-list
   :parent parent
   :fn-delete-condition fn-delete-condition))

(defun draw-debug-line-by-time (&key point1 point2
                                  (tag-list '())
                                  (parent nil)
                                  (time 60))
  (draw-debug-line
   :point1 point1
   :point2 point2
   :tag-list tag-list
   :parent parent
   :fn-delete-condition (make-fn-timer-condition time)))

;; --- tools --- ;;

(defun make-fn-timer-condition (time)
  (lambda (entity)
    (declare (ignore entity))
    (decf time)
    (<= time 0)))
