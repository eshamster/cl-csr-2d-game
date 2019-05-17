(in-package :cl-user)
(defpackage cl-csr-2d-game/graphics/2d-geometry
  (:use :cl
        :cl-ps-ecs
        :cl-csr-2d-game/core/basic-components)
  (:export ; :make-line
           ; :make-lines
           ; :make-solid-regular-polygon
           ; :make-wired-regular-polygon
           ; :make-wired-polygon
           ; :make-solid-polygon
           ; :make-texture-model
           ; :make-texture-model-promise
           ; :make-text-model-promise
           ; :change-model-color
           ; :change-geometry-uvs

           ; :get-mesh-width
           ; :get-mesh-height
           ; :get-mesh-size
           :make-rect-mesh
           :make-circle-mesh)
  (:import-from :proto-cl-client-side-rendering
                :draw-rect
                :draw-circle))
(in-package :cl-csr-2d-game/graphics/2d-geometry)

;; --- rectangle --- ;;

(defun make-rect-mesh (&key
                         (color #xffffff)
                         width height fill-p)
  (lambda (&key id x y depth rotate)
    (draw-rect :id id :depth depth :color color
               :width width :height height
               :x x
               :y y
               :rotate rotate
               :fill-p fill-p)))

;; --- circle --- ;;

(defun make-circle-mesh (&key
                           (color #xffffff)
                           r fill-p)
  (lambda (&key id x y depth rotate)
    ;; TODO: rotate considering offset
    (declare (ignore rotate))
    (draw-circle :id id :depth depth :color color
                 :r r
                 :x x
                 :y y
                 :fill-p fill-p)))
