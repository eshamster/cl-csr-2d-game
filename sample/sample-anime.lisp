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

                :load-anime
                :add-anime-2d
                :reset-anime
                :reverse-anime)
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
              :x-count 5 :y-count 3)
  (init-anime-entity-creator))

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
