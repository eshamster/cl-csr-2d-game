(defpackage cl-csr-2d-game/core/update-frequency
  (:use :cl)
  (:export :process-update-frequency
           :set-update-frequency
           :draw-in-this-frame-p)
  (:import-from :proto-cl-client-side-rendering
                :set-fps))
(in-package :cl-csr-2d-game/core/update-frequency)

;; --- data --- ;;

(defvar *draw-interval-frame* 1)
(defvar *frame-count* 1)

;; --- interface --- ;;

(defun process-update-frequency ()
  (incf *frame-count*))

(defun set-update-frequency (fps draw-interval-frame)
  "Set update frequecy of game logic and drawning.
The fps is a frame per second of game logic.
The draw-interval-frame shows frequency of drawing.
Ex. If it is 1, drawing process will be invoked in every frame.
    If it is 2, drawing process will be invoked in every 2 frame."
  (assert (and fps draw-interval-frame))
  (check-type fps fixnum)
  (check-type draw-interval-frame fixnum)
  (assert (and (> fps 0) (> draw-interval-frame 0)))
  (set-fps fps)
  (setf *draw-interval-frame* draw-interval-frame))

(defun draw-in-this-frame-p ()
  (= (mod *frame-count* *draw-interval-frame*) 0))
