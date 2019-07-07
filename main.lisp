(in-package :cl-user)

;; This package is depend on the following js libraries.
;; - three.js
;; - dat.gui.js
;; - threex.keyboardstate.js

;; Note: ":use" not only re-exporting is also requred. Because
;; ps-experiment:with-use-ps-pack solves dependencies using package-use-list.

(uiop/package:define-package :cl-csr-2d-game/main
  (:nicknames :cl-csr-2d-game)
  (:use-reexport :cl-csr-2d-game/core/basic-components
                 :cl-csr-2d-game/core/basic-systems
                 :cl-csr-2d-game/core/initializer
                 :cl-csr-2d-game/core/game-state
                 :cl-csr-2d-game/core/update-frequency
                 ;; Note: The camera is maybe not "core".
                 :cl-csr-2d-game/core/camera
                 :cl-csr-2d-game/core/server

                 :cl-csr-2d-game/graphics/2d-geometry
                 :cl-csr-2d-game/graphics/draw-model-system
                 :cl-csr-2d-game/graphics/font

                 :cl-csr-2d-game/inputs/gui
                 ;; :cl-csr-2d-game/inputs/ui

                 :cl-csr-2d-game/physics/collision
                 :cl-csr-2d-game/physics/collision-system

                 :cl-csr-2d-game/utils/utils
                 :cl-csr-2d-game/utils/calc 
                 :cl-csr-2d-game/utils/debug/performance
                 :cl-csr-2d-game/utils/debug/logger
                 :cl-csr-2d-game/utils/debug/debug-drawer
                 :cl-csr-2d-game/utils/basic-generator
                 :cl-csr-2d-game/utils/stage-generator
                 :cl-csr-2d-game/utils/storage))
(in-package :cl-csr-2d-game/main)
