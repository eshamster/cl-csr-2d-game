(defpackage cl-csr-2d-game/core/basic-systems
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-csr-2d-game/core/basic-components
        :cl-csr-2d-game/physics/collision
        :cl-csr-2d-game/utils/debug/performance)
  (:import-from :cl-csr-2d-game/utils/calc
                :incf-vector-2d)
  (:export :script-system
           :make-script-system
           :make-simple-move-system))
(in-package :cl-csr-2d-game/core/basic-systems)

(enable-ps-experiment-syntax)

(defstruct.ps+
    (script-system
     (:include ecs-system
               (target-component-types '(script-2d))
               (process (lambda (entity)
                          (do-ecs-components-of-entity (script entity
                                                               :component-type 'script-2d)
                            (funcall (script-2d-func script) entity)))))))

(defstruct.ps+
    (simple-move-system
     (:include ecs-system
               (target-component-types '(point-2d speed-2d))
               (process (lambda (entity)
                          (with-ecs-components (point-2d speed-2d) entity
                            (incf-vector-2d point-2d speed-2d)))))))
