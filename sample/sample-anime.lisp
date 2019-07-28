(defpackage sample-cl-csr-2d-game/sample-anime
  (:use :cl)
  (:export :update-sample-anime
           :init-sample-anime)
  (:import-from :cl-csr-2d-game
                :make-point-2d
                :point-2d
                :point-2d-x
                :point-2d-y
                :point-2d-angle
                :make-script-2d
                :make-model-2d
                :update-model-2d
                :find-model-2d-by-label
                :init-entity-params
                :get-entity-param
                :set-entity-param

                :load-anime
                :add-anime-2d
                :reset-anime
                :reverse-anime
                :resume-anime

                :init-anime-switcher
                :switch-anime
                :get-current-anime-2d)
  (:import-from :cl-ps-ecs
                :make-ecs-entity
                :add-ecs-entity
                :delete-ecs-entity
                :add-ecs-component-list
                :find-the-entity
                :with-ecs-components
                :register-next-frame-func)
  (:import-from :proto-cl-client-side-rendering
                :log-console

                :get-client-id-list
                :key-down-now-p
                :key-up-now-p
                :mouse-down-p
                :get-mouse-pos

                :load-texture))
(in-package :sample-cl-csr-2d-game/sample-anime)

(defun init-sample-anime ()
  (init-anime))

(defun update-sample-anime ())

(defun init-anime ()
  (load-texture :name :sample-explosion
                :path "sample_explosion.png"
                :alpha-path "sample_explosion_alpha.png")
  (load-anime :anime-name :explosion
              :texture-name :sample-explosion
              :x-count 5 :y-count 2
              :uv-height 2/3)
  (init-anime-entity-creator)

  (load-texture :name :sample-moving
                :path "sample_moving.png"
                :alpha-path "sample_moving_alpha.png")
  (load-anime :anime-name :move-right
              :texture-name :sample-moving
              :x-count 4 :y-count 1
              :uv-height 0.5)
  (load-anime :anime-name :move-left
              :texture-name :sample-moving
              :x-count 4 :y-count 1
              :uv-height 0.5 :offset-uv-y 0.5)
  (init-anime-with-switcher))

(defun init-anime-entity-creator ()
  (let ((entity (make-ecs-entity)))
    (add-ecs-component-list
     entity
     (make-script-2d :func (lambda (entity)
                             (declare (ignore entity))
                             (add-anime-entity-by-input))))
    (add-ecs-entity entity)))

(defun add-anime-entity-by-input ()
  (dolist (client-id (get-client-id-list))
    (multiple-value-bind (x y)
        (get-mouse-pos client-id)
      (when (key-down-now-p client-id :z)
        (add-anime-entity x y))
      (when (key-down-now-p client-id :x)
        (add-anime-entity x y :kind :repeat))
      (when (key-down-now-p client-id :c)
        (add-anime-entity x y :kind :reverse)))))

(defun add-anime-entity (x y &key (kind :once))
  (let ((entity (make-ecs-entity))
        (width 100)
        (height 100))
    (add-ecs-component-list
     entity
     (make-point-2d :x x :y y))
    (add-anime-2d :entity entity
                  :anime-name :explosion
                  :interval 2
                  :width width
                  :height height
                  :model (make-model-2d
                          :depth 10
                          :offset (make-point-2d :x (* width -1/2)
                                                 :y (* height -1/2)))
                  :anime-end-callback
                  (lambda (anime-2d)
                    (when (find-the-entity entity)
                      (ecase kind
                        (:once
                         (register-next-frame-func
                          (lambda ()
                            (delete-ecs-entity entity))))
                        (:repeat
                         (setf kind :once)
                         (reset-anime anime-2d :stop-p nil))
                        (:reverse
                         (setf kind :once)
                         (reverse-anime anime-2d))))))
    (add-ecs-entity entity)))

;; --- anime switcher --- ;;

(defun init-anime-with-switcher ()
  (let ((entity (make-ecs-entity))
        (width 100)
        (height 100)
        (x 400)
        (y 300)
        (anime-interval 3))
    (flet ((my-add-anime-2d (anime-name)
             (add-anime-2d :entity entity
                           :anime-name anime-name
                           :interval anime-interval
                           :width width
                           :height height
                           :model (make-model-2d
                                   :depth 20
                                   :offset (make-point-2d :x (* width -1/2)
                                                          :y (* height -1/2))))))
      (let ((switcher (init-anime-switcher
                       entity
                       (list :right (my-add-anime-2d :move-right))
                       (list :left (my-add-anime-2d :move-left))))
            (first-direction :right))
        (switch-anime switcher first-direction :stop-p t)
        (add-ecs-component-list
         entity
         (make-point-2d :x x :y y)
         (make-script-2d :func (lambda (entity)
                                 (process-switch-input entity switcher)))
         (init-entity-params :direction first-direction))))
    (add-ecs-entity entity)))

(defun process-switch-input (entity switcher)
  (dolist (client-id (get-client-id-list))
    (dolist (key '(:right :left))
      (when (key-down-now-p client-id key)
        (start-move-to entity switcher key))
      (when (key-up-now-p client-id key)
        (stop-move-to entity switcher key)))))

(defun start-move-to (entity switcher direction)
  (assert (or (eq direction :left)
              (eq direction :right)))
  (format t "start to ~D~%" direction)
  (let ((prev-direction (get-entity-param entity :direction)))
    (if (eq direction prev-direction)
        (resume-anime (get-current-anime-2d switcher) :forward-p t)
        (switch-anime switcher direction :stop-p nil :forward-p t))
    (set-entity-param entity :direction direction)))

(defun stop-move-to (entity switcher direction)
  (assert (or (eq direction :left)
              (eq direction :right)))
  (when (eq direction (get-entity-param entity :direction))
    (format t "stop to ~D~%" direction)
    (resume-anime (get-current-anime-2d switcher) :forward-p nil)))
