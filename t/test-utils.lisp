(in-package :cl-user)
(defpackage cl-csr-2d-game/t/test-utils
  (:use :cl
        :prove
        :cl-csr-2d-game/core/basic-components)
  (:export :within
           :within-angle
           :within-length
           :is-point
           :is-vector
           :*angle-error*
           :*length-error*))
(in-package :cl-csr-2d-game/t/test-utils)

;; --- not clasified --- ;;

(defun within (got expected tolerance)
  (< (- expected tolerance)
     got
     (+ expected tolerance)))

(defvar *angle-error* (/ PI 10000))
(defvar *length-error* (/ 1 10000))

(defun within-angle (got expected &optional (tolerance *angle-error*))
  (within got expected tolerance))

(defun within-length (got expected &optional (tolerance *length-error*))
  (within got expected tolerance))

(defun is-vector (target x y)
  (and (within-length (vector-2d-x target) x)
       (within-length (vector-2d-y target) y)))

(defun is-point (target x y angle)
  (and (within-length (point-2d-x target) x)
       (within-length (point-2d-y target) y)
       (within-angle (point-2d-angle target) angle)))
