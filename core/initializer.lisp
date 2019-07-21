(in-package :cl-user)
(defpackage cl-csr-2d-game/core/initializer
  (:use :cl
        :cl-csr-2d-game/core/basic-systems
        :cl-csr-2d-game/graphics/anime-system
        :cl-csr-2d-game/graphics/draw-model-system
        :cl-csr-2d-game/physics/collision-system)
  (:export :init-default-systems)
  (:import-from :cl-ps-ecs
                :register-ecs-system))
(in-package :cl-csr-2d-game/core/initializer)

(defun init-default-systems (&key (script-system t)
                               (draw-system t)
                               (anime-system t)
                               (collision-system t)
                               (simple-move-system t)
                               ; (ui-system t)
                               )
  (when script-system
    (register-ecs-system "script2d" (make-script-system)))
  (when collision-system
    (register-ecs-system "collision" (make-collision-system)))
  (when simple-move-system
    (register-ecs-system "simple-move" (make-simple-move-system)))
  (when draw-system
    (register-ecs-system "draw2d" (init-draw-model-system)))
  (when anime-system
    (register-ecs-system "anime" (make-anime-system)))
  #|
  (when ui-system
    (register-ecs-system "ui" (init-ui-system)))
  |#
  )


