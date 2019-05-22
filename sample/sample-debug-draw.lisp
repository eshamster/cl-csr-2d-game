(defpackage sample-cl-csr-2d-game/sample-debug-draw
  (:use :cl
        :cl-ps-ecs
        :cl-csr-2d-game
        :proto-cl-client-side-rendering)
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
         :time 10)))))
