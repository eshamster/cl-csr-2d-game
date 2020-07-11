(in-package :cl-user)
(defpackage cl-csr-2d-game/graphics/2d-geometry
  (:use :cl
        :cl-ps-ecs
        :cl-csr-2d-game/core/basic-components)
  (:export :make-line-mesh
           :make-line-mesh-by-vector
           ; :make-lines
           ; :make-solid-regular-polygon
           ; :make-wired-regular-polygon
           ; :make-wired-polygon
           ; :make-solid-polygon
           :make-image-mesh
           ; :make-texture-model-promise
           :make-text-mesh
           ; :change-model-color
           ; :change-geometry-uvs

           ; :get-mesh-width
           ; :get-mesh-height
           ; :get-mesh-size
           :make-rect-mesh
           :make-arc-mesh
           :make-circle-mesh)
  (:import-from :cl-csr
                :draw-rect
                :draw-arc
                :draw-circle
                :draw-line
                :draw-image
                :draw-text))
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

;; --- arc --- ;;

(defun make-arc-mesh (&key
                        (color #xffffff)
                        r start-angle sweep-angle)
  (lambda (&key id x y depth rotate)
    ;; TODO: rotate considering offset
    (draw-arc :id id :depth depth :color color
              :start-angle (+ start-angle rotate)
              :sweep-angle sweep-angle
              :r r
              :x x :y y)))

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

;; --- line --- ;;

(defun make-line-mesh (&key x1 y1 x2 y2 color)
  (lambda (&key id x y depth rotate)
    ;; TODO: consider rotate
    (declare (ignore rotate))
    (draw-line :id id
               :x1 (+ x1 x) :y1 (+ y1 y)
               :x2 (+ x2 x) :y2 (+ y2 y)
               :depth depth
               :color color)))

(defun make-line-mesh-by-vector (&key vec1 vec2 color)
  (make-line-mesh :x1 (vector-2d-x vec1)
                  :y1 (vector-2d-y vec1)
                  :x2 (vector-2d-x vec2)
                  :y2 (vector-2d-y vec2)
                  :color color))

;; --- image --- ;;

(defun make-image-mesh (&key
                          image-name
                          (color #xffffff) width height)
  "Make a mesh textured by a image.
The image-name should be registered by cl-csr:load-image in advance."
  (lambda (&key id x y depth rotate)
    (draw-image :id id
                :image-name image-name
                :x x
                :y y
                :depth depth
                :color color
                :width width
                :height height
                :rotate rotate)))

;; --- text --- ;;

(defun make-text-mesh (&key
                         font-name
                         text
                         (color #xffffff)
                         font-size
                         (align-horiz :left)
                         (align-vert :top))
    "Make a mesh textured by a text.
The font-name should be registered by cl-csr:load-font in advance.
If width is nil, it will be calculated by height to keep aspect ration of each character."
    (assert font-size)
    (lambda (&key id x y depth rotate)
      ;; TODO: consider rotate
      (declare (ignore rotate))
      (draw-text :id id
                 :font-name font-name
                 :text text
                 :x x
                 :y y
                 :depth depth
                 :color color
                 :font-size font-size
                 :align-horiz align-horiz
                 :align-vert align-vert)))
