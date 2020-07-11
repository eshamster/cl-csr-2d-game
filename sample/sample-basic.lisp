(defpackage sample-cl-csr-2d-game/sample-basic
  (:use :cl)
  (:export :update-sample-basic
           :init-sample-basic)
  (:import-from :cl-csr-2d-game
                :make-point-2d
                :point-2d
                :x
                :y
                :point-2d-x
                :point-2d-y
                :point-2d-angle
                :make-script-2d
                :make-model-2d
                :model-2d
                :model-2d-offset
                :model-2d-target-client-id-list
                :make-rect-mesh
                :make-arc-mesh
                :make-circle-mesh
                :make-image-mesh
                :make-text-mesh
                :update-model-2d
                :find-model-2d-by-label)
  (:import-from :cl-ps-ecs
                :make-ecs-entity
                :add-ecs-entity
                :add-ecs-component-list
                :with-ecs-components)
  (:import-from :cl-csr
                :draw-rect
                :draw-circle
                :log-console

                :get-client-id-list
                :*target-client-id-list*
                :key-down-p
                :mouse-down-p
                :get-mouse-pos

                :load-texture
                :load-image
                :make-image-uv
                :load-font))
(in-package :sample-cl-csr-2d-game/sample-basic)

(defvar *temp-counter* 0)

(defparameter *temp-speed* 10)

(defun init-sample-basic ()
  (init-circle :x 200 :speed 1/2
               :r 40
               :fill-p nil
               :color #xff0000)
  (init-circle :x 300 :speed 1/3
               :r 40
               :fill-p t
               :color #x00ffff)
  (init-rect :x 400 :y 300
             :rotate-speed 1/5
             :fill-p nil
             :color #x00ff00)
  (init-rect :x 500 :y 300
             :rotate-speed -1/5
             :fill-p t
             :color #xff00ff)
  (init-arc :x 100 :y 400
            :start-angle (* PI 1/6)
            :sweep-angle (* PI 1/3)
            :r 40
            :color #x00fffff)
  (init-circle-moved-by-input)
  (init-rect-modifying-model)
  (init-circle-sending-to-each-client
   :x 700 :y 450 :color #xff0000 :odd-client-p t)
  (init-circle-sending-to-each-client
   :x 700 :y 500 :color #x0000ff :odd-client-p nil)

  (load-images)
  (init-image :image-name :sample
              :x 400 :y 100
              :rotate-speed 1/6
              :color #xff88ff)
  (init-image :image-name :sample-with-alpha
              :x 500 :y 100
              :rotate-speed -1/6)
  (init-image :image-name :a
              :x 600 :y 100
              :rotate-speed 1/6)
  (init-image :image-name :b
              :x 620 :y 100
              :rotate-speed -1/6)

  (load-sample-font)
  (init-text))

(defun load-images ()
  (load-texture :name :sample
                :path "sample.png")
  (load-image :image-name :sample
              :texture-name :sample)

  (load-texture :name :sample-with-alpha
                :path "sample.png")
  (load-image :image-name :sample-with-alpha
              :texture-name :sample-with-alpha)

  (load-texture :name :multiple-image
                :path "multiple_image.png")
  (load-image :image-name :a
              :texture-name :multiple-image
              :uv (make-image-uv :width 0.5))
  (load-image :image-name :b
              :texture-name :multiple-image
              :uv (make-image-uv :x 0.5 :width 0.5)))

(defun load-sample-font ()
  (load-font :name :sample-font
             :font-name "Arial"))

;; --- rect --- ;;

(defun init-rect (&key x y rotate-speed fill-p color)
  (let ((entity (make-ecs-entity)))
    (add-ecs-component-list
     entity
     (make-point-2d :x x :y y)
     (make-model-2d :mesh (make-rect-mesh :width 20 :height 40
                                          :color color
                                          :fill-p fill-p)
                    :offset (make-point-2d :x -10 :y -20)
                    :depth 0)
     (make-script-2d :func (lambda (entity)
                             (update-rect entity rotate-speed))))
    (add-ecs-entity entity)))

(defun update-rect (entity rotate-speed)
  (with-ecs-components (point-2d) entity
    (incf (point-2d-angle point-2d) rotate-speed)))

;; --- arc --- ;;

(defun init-arc (&key x y start-angle sweep-angle r color)
  (let ((entity (make-ecs-entity)))
    (add-ecs-component-list
     entity
     (make-point-2d :x x :y y)
     (make-model-2d :mesh (make-arc-mesh :r r
                                         :start-angle start-angle
                                         :sweep-angle sweep-angle
                                         :color color)
                    :depth 0))
    (add-ecs-entity entity)))

;; --- circle --- ;;

(defun init-circle (&key x speed r fill-p color)
  (let ((entity (make-ecs-entity)))
    (add-ecs-component-list
     entity
     (make-point-2d :x x :y 300)
     (make-model-2d :mesh (make-circle-mesh :r r
                                            :color color
                                            :fill-p fill-p)
                    :depth 0)
     (make-script-2d :func (lambda (entity)
                             (update-circle entity speed))))
    (add-ecs-entity entity)))

(defun update-circle (entity speed)
  (with-ecs-components (point-2d) entity
    (setf (point-2d-y point-2d)
          (+ 300 (* 100 (sin (* *temp-counter* speed)))))))

;; --- test input --- ;;

(defun init-circle-moved-by-input ()
  (let ((entity (make-ecs-entity)))
    (add-ecs-component-list
     entity
     (make-point-2d :x 100 :y 300)
     (make-model-2d :mesh (make-circle-mesh :r 50
                                            :color #xffffff
                                            :fill-p t)
                    :depth 10)
     (make-script-2d :func (lambda (entity)
                             (move-by-keyboard entity)
                             (move-by-mouse entity))))
    (add-ecs-entity entity)))

(defun move-by-keyboard (entity)
  (with-ecs-components (point-2d) entity
    (with-slots (x y) point-2d
      (dolist (client-id (get-client-id-list))
        (when (key-down-p client-id :up)
          (incf y *temp-speed*))
        (when (key-down-p client-id :down)
          (decf y *temp-speed*))
        (when (key-down-p client-id :right)
          (incf x *temp-speed*))
        (when (key-down-p client-id :left)
          (decf x *temp-speed*))))))

(defun move-by-mouse (entity)
  (with-ecs-components (point-2d) entity
    (with-slots (x y) point-2d
      (dolist (client-id (get-client-id-list))
        (when (mouse-down-p client-id :left)
          (multiple-value-bind (mouse-x mouse-y)
              (get-mouse-pos client-id)
            (setf x mouse-x
                  y mouse-y)))))))

;; --- modify model --- ;;

(defun init-rect-modifying-model ()
  (let ((entity (make-ecs-entity)))
    (add-ecs-component-list
     entity
     (make-point-2d :x 600 :y 300)
     (make-model-2d :mesh (make-rect-mesh :width 20 :height 40
                                          :color #xff00ff
                                          :fill-p nil)
                    :offset (make-point-2d :x -10 :y -20)
                    :depth 0
                    :label :test-model)
     (make-script-2d :func (lambda (entity)
                             (update-rect-model entity))))
    (add-ecs-entity entity)))

(defun update-rect-model (entity)
  (update-model-2d
   entity
   (find-model-2d-by-label entity :test-model)
   (make-model-2d :mesh (make-rect-mesh :width 20
                                        :height (+ 40 (* 20 (sin (/ *temp-counter* 2))))
                                        :color #xff00ff
                                        :fill-p nil)
                  :depth 0
                  :label :test-model)))

;; --- send to each client --- ;;

(defun init-circle-sending-to-each-client (&key x y color odd-client-p)
  (let ((entity (make-ecs-entity)))
    (add-ecs-component-list
     entity
     (make-point-2d :x x :y y)
     (make-model-2d :mesh (make-circle-mesh :r 25
                                            :color color
                                            :fill-p t)
                    :depth 0
                    :target-client-id-list nil)
     (make-script-2d :func (lambda (entity)
                             (update-target-client entity odd-client-p))))
    (add-ecs-entity entity)))

(defun update-target-client (entity odd-client-p)
  (with-ecs-components (model-2d) entity
    (setf (model-2d-target-client-id-list model-2d)
          (remove-if (lambda (id) (= (mod id 2)
                                     (if odd-client-p 0 1)))
                     (get-client-id-list)))))

;; --- image --- ;;

(defun init-image (&key image-name x y rotate-speed color)
  (let ((entity (make-ecs-entity))
        (width 60)
        (height 60))
    (add-ecs-component-list
     entity
     (make-point-2d :x x :y y)
     (make-model-2d :mesh (make-image-mesh :image-name image-name
                                           :width width :height height
                                           :color color)
                    :offset (make-point-2d :x (* width -1/2) :y (* height -1/2))
                    :depth 0)
     (make-script-2d :func (lambda (entity)
                             (update-rect entity rotate-speed))))
    (add-ecs-entity entity)))

;; --- text --- ;;

(defun init-text ()
  (let ((entity (make-ecs-entity))
        (x 10)
        (y 500))
    (add-ecs-component-list
     entity
     (make-point-2d :x x :y y)
     (make-model-2d :mesh (make-text-mesh :font-name :sample-font
                                          :text "Sample text-aaa"
                                          :font-size 18
                                          :color #xffffff)
                    :depth 0))
    (add-ecs-entity entity)))

;; --- ;;

(defun update-sample-basic ()
  (incf *temp-counter*))
