(defpackage cl-csr-2d-game/utils/basic-generator
  (:use :cl
        :ps-experiment
        :parenscript)
  ;; Note: The dummy exporting is to avoid ps-experiment issue
  ;; where a package without exporting symbol can be failed to load.
  (:export :dummy-cl-csr-2d-game/utils/basic-generator)
  (:import-from :cl-csr-2d-game/core/basic-components
                :make-point-2d
                :make-speed-2d)
  (:import-from :cl-csr-2d-game/utils/stage-generator
                :def-stage-element-interpreter))
(in-package :cl-csr-2d-game/utils/basic-generator)

(def-stage-element-interpreter :point (x y angle)
  (make-point-2d :x x :y y :angle angle))

(def-stage-element-interpreter :speed (x y)
  (make-speed-2d :x x :y y))
