(defpackage sample-cl-csr-2d-game/sample-debug-draw
  (:use :cl
        :cl-ps-ecs
        :cl-csr-2d-game
        :cl-csr)
  (:export :init-sample-debug-draw
           :update-sample-debug-draw))
(in-package :sample-cl-csr-2d-game/sample-debug-draw)

;; --- interface --- ;;

(defun init-sample-debug-draw ())

(defun update-sample-debug-draw ()
  (dolist (id (get-client-id-list))
    (when (mouse-down-now-p id :left)
      (multiple-value-bind (x y) (get-mouse-pos id)
        (draw-debug-point-by-time
         :point (make-point-2d :x x :y y)
         :r 20
         :time 10)
        (setf (vector-2d-x *down-point*) x
              (vector-2d-y *down-point*) y)))
    (when (mouse-up-now-p id :left)
      (multiple-value-bind (x y) (get-mouse-pos id)
        (draw-debug-line-by-time
         :point1 (clone-point-2d *down-point*)
         :point2 (make-point-2d :x x :y y)
         :time 10)))))

;; --- internal --- ;;

(defvar *down-point* (make-point-2d))
