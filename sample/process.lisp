(defpackage sample-cl-csr-2d-game/process
  (:use :cl)
  (:export :update-sample)
  (:import-from :proto-cl-client-side-rendering
                :draw-rect
                :draw-circle
                :log-console

                :get-client-id-list
                :*target-client-id-list*
                :key-down-p
                :mouse-down-p
                :get-mouse-pos))
(in-package :sample-cl-csr-2d-game/process)

(defvar *temp-counter* 0)

(defvar *temp-x* 100)
(defvar *temp-y* 300)
(defparameter *temp-speed* 10)

(defun update-sample ()
  (incf *temp-counter*)
  (let ((id 0))
    (draw-circle :id (incf id)
                 :x 200 :y (+ 300 (* 100 (sin (/ *temp-counter* 2))))
                 :depth 0
                 :r 40 :color #xff0000)
    (draw-circle :id (incf id)
                 :x 300 :y (+ 300 (* 100 (sin (/ *temp-counter* 3))))
                 :depth 0 :fill-p t
                 :r 40 :color #x00ffff)
    (draw-rect :id (incf id)
               :x 400 :y 300
               :depth 0 :fill-p nil
               :width 20 :height 40
               :rotate (* 1/5 *temp-counter*)
               :color #x00ff00)
    (draw-rect :id (incf id)
               :x 500 :y 300
               :depth 0 :fill-p t
               :width 20 :height 40
               :rotate (* -1/5 *temp-counter*)
               :color #xff00ff)
    (draw-rect :id (incf id)
               :x 600 :y 300
               :depth 0 :fill-p nil
               :width 20 :height (+ 40 (* 20 (sin (/ *temp-counter* 2))))
               :rotate 0
               :color #xff00ff)
    ;; (log-console :message "test") ; try logging
    (try-keyboard)
    (try-mouse)
    (draw-circle :id (incf id)
                 :x *temp-x* :y *temp-y*
                 :depth 10 :fill-p t
                 :r 50 :color #xffffff)
    ;; try sending to each client
    (flet ((try-send (y color)
             (draw-circle :id (incf id)
                 :x 700 :y y
                 :depth 10 :fill-p t
                 :r 25 :color color)))
      (let ((id-list (get-client-id-list)))
        (let ((*target-client-id-list*
               (remove-if (lambda (id) (= (mod id 2) 0)) id-list)))
          (try-send 500 #xff0000))
        (let ((*target-client-id-list*
               (remove-if (lambda (id) (= (mod id 2) 1)) id-list)))
          (try-send 450 #x0000ff))))))

(defun try-keyboard ()
  (dolist (client-id (get-client-id-list))
    (when (key-down-p client-id :up)
      (incf *temp-y* *temp-speed*))
    (when (key-down-p client-id :down)
      (decf *temp-y* *temp-speed*))
    (when (key-down-p client-id :right)
      (incf *temp-x* *temp-speed*))
    (when (key-down-p client-id :left)
      (decf *temp-x* *temp-speed*))))

(defun try-mouse ()
  (dolist (client-id (get-client-id-list))
    (when (mouse-down-p client-id :left)
      (multiple-value-bind (x y) (get-mouse-pos client-id)
        (setf *temp-x* x
              *temp-y* y)))))
