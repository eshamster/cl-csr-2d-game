(defpackage cl-csr-2d-game/t/basic-generator
  (:use :cl
        :rove
        :cl-csr-2d-game/utils/basic-generator
        :cl-csr-2d-game/utils/stage-generator)
  (:import-from :cl-csr-2d-game/core/basic-components
                :make-point-2d
                :make-speed-2d)
  (:import-from :cl-csr-2d-game/t/test-utils
                :is-point
                :is-vector))
(in-package :cl-csr-2d-game/t/basic-generator)

(defparameter *buf* nil)

(defmacro process-interpreter (def)
  `(let ((test-stage (generate-stage ,def)))
     (process-stage test-stage)
     *buf*))

;; --- test --- ;;

(def-stage-element-interpreter
    (:point-tester (:include :point)) ()
  (setf *buf* point))

(deftest for-point-component
  (ok (is-point (process-interpreter
                 (:point-tester :time 0 (:point :x 1 :y 2 :angle PI)))
                1 2 PI)))

(def-stage-element-interpreter
    (:speed-tester (:include :speed)) ()
  (setf *buf* speed))

(deftest for-speed-component
  (ok (is-vector (process-interpreter
                  (:speed-tester :time 0 (:speed :x 1 :y 2)))
                 1 2)))
