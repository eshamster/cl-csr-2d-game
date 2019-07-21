(defpackage cl-csr-2d-game/graphics/anime
  (:use :cl
        :cl-ps-ecs)
  (:export :load-anime
           :add-anime-2d
           ;; for internal (another package)
           :process-anime)
  (:import-from :cl-csr-2d-game/graphics/2d-geometry
                :make-image-mesh)
  (:import-from :cl-csr-2d-game/graphics/draw-model-system
                :make-model-2d
                :model-2d-mesh
                :find-model-2d-by-label)
  (:import-from :alexandria
                :make-keyword)
  (:import-from :proto-cl-client-side-rendering
                :load-image
                :make-image-uv))
(in-package :cl-csr-2d-game/graphics/anime)

;; --- data --- ;;

(defstruct anime-info
  (x-count 1)
  (y-count 1)
  image-array)

(defstruct (anime-2d (:include ecs-component))
  ;; input parameter
  anime-name
  interval
  anime-end-callback
  model
  width height color
  ;; state parameter
  (go-to-forward-p t)
  (run-anime-p t)
  (interval-counter 0)
  (image-caret 0))

(defvar *anime-table* (make-hash-table)
  "Key: Name as a keyword, Value: anime-info")

;; --- interface --- ;;

(defun load-anime (&key
                     texture-name anime-name
                     (x-count 1)
                     (y-count 1)
                     (offset-uv-x 0.0)
                     (offset-uv-y 0.0)
                     (uv-width 1.0)
                     (uv-height 1.0))
  (check-type texture-name keyword)
  (check-type anime-name keyword)
  (assert (> x-count 1))
  (assert (> y-count 1))
  (let ((one-uv-width (/ uv-width x-count))
        (one-uv-height (/ uv-height y-count))
        (image-array (make-array (* x-count y-count))))
    (dotimes (y y-count)
      (dotimes (x x-count)
        (let* ((uv-x (+ offset-uv-x
                        (* x one-uv-width)))
               (uv-y (+ offset-uv-y
                        (* y one-uv-height)))
               (index (+ x (* y x-count)))
               ;; XXX: Avoid name confliction of image-name
               (image-name (make-keyword
                            (format nil "~A-~D" anime-name index))))
          (load-image :texture-name texture-name
                      :image-name image-name
                      :uv (make-image-uv :x uv-x
                                         :y uv-y
                                         :width one-uv-width
                                         :height one-uv-height))
          (setf (aref image-array index) image-name))))
    (setf (gethash anime-name *anime-table*)
          (make-anime-info :x-count x-count
                           :y-count y-count
                           :image-array image-array)))
  anime-name)

(defun add-anime-2d (&key anime-name (interval 1) entity model
                       width height (color #xffffff)
                       (anime-end-callback (lambda (anime-2d) (declare (ignore anime-2d)))))
  "Initialize anime-2d structure.
The specified anime by the anime-name should have been loaded by load-anime.
The model is a model-2d with an empty \":mesh\" parameter."
  (let ((anime-2d (make-anime-2d :anime-name anime-name
                                 :interval interval
                                 :model model
                                 :width width
                                 :height height
                                 :color color
                                 :anime-end-callback anime-end-callback)))
    (add-ecs-component anime-2d entity)
    (add-ecs-component model entity anime-2d)
    (switch-anime-image anime-2d 0)
    anime-2d))

(defun process-anime (anime-2d)
  (when (anime-2d-run-anime-p anime-2d)
    (with-slots (interval-counter interval) anime-2d
      (if (< (1+ interval-counter) interval)
          (incf interval-counter)
          (let ((image-caret (anime-2d-image-caret anime-2d))
                (go-to-forward-p (anime-2d-go-to-forward-p anime-2d))
                (anime-info (get-anime-info (anime-2d-anime-name anime-2d))))
            (if (or (and go-to-forward-p (< (1+ image-caret)
                                            (get-anime-cell-count anime-info)))
                    (and (not go-to-forward-p) (> image-caret 0)))
                (progn (setf interval-counter 0)
                       (switch-anime-image anime-2d
                                           (if go-to-forward-p
                                               (1+ image-caret)
                                               (1- image-caret))))
                (progn (setf (anime-2d-run-anime-p anime-2d) nil)
                       (funcall (anime-2d-anime-end-callback anime-2d) anime-2d))))))))

;; --- internal --- ;;

(defun get-anime-info (name)
  (let ((info (gethash name *anime-table*)))
    (unless info
      (error "The anime named \"~A\" has not been loaded." name))
    info))

(defun get-anime-cell-count (anime-info)
  (* (anime-info-x-count anime-info)
     (anime-info-y-count anime-info)))

;; (defun enable-anime (entity anime-2d)
;;   "Enable drawing the model"
;;   (check-type entity ecs-entity)
;;   (check-type anime-2d anime-2d)
;;   (enable-model-2d entity
;;                    :target-model-2d (anime-2d-model anime-2d)))

;; (defun disable-anime (entity anime-2d)
;;   "Stop the anime and disable drawing the model"
;;   (check-type entity ecs-entity)
;;   (check-type anime-2d anime-2d)
;;   (stop-anime anime-2d)
;;   (disable-model-2d entity
;;                     :target-model-2d (anime-2d-model anime-2d)))

;; (defun start-anime (anime)
;;   (with-slots (goes-to-forward interval-counter interval runs-anime) anime
;;     (unless goes-to-forward
;;       (setf interval-counter
;;             (- interval interval-counter 1)))
;;     (setf runs-anime t)
;;     (setf goes-to-forward t)))

;; (defun start-reversed-anime (anime)
;;   (with-slots (goes-to-forward interval-counter interval runs-anime) anime
;;     (when goes-to-forward
;;       (setf interval-counter
;;             (- interval interval-counter 1)))
;;     (setf runs-anime t)
;;     (setf goes-to-forward nil)))

;; (defun reverse-anime (anime)
;;   (if (anime-2d-goes-to-forward anime)
;;       (start-reversed-anime anime)
;;       (start-anime anime)))

;; (defun reset-anime (anime &key (stop-p t) (forward-p :asis))
;;   (with-slots (goes-to-forward interval-counter runs-anime
;;                                horiz-count vert-count) anime
;;     (when (eq forward-p :asis)
;;       (setf forward-p goes-to-forward))
;;     (setf interval-counter 0)
;;     (switch-anime-image anime
;;                             (if forward-p
;;                                 0
;;                                 (1- (* horiz-count vert-count))))
;;     (setf runs-anime (not stop-p))
;;     (setf goes-to-forward forward-p)))

;; (defun stop-anime (anime)
;;   (setf (anime-2d-runs-anime anime) nil))

(defun switch-anime-image (anime-2d next-counter)
  (let ((model (anime-2d-model anime-2d))
        (anime-info (get-anime-info (anime-2d-anime-name anime-2d))))
    (let ((max-count (* (anime-info-x-count anime-info)
                        (anime-info-y-count anime-info))))
      (when (or (< next-counter 0)
                (>= next-counter max-count))
        (error "The target anime counter is invalid (Max: ~D, Got: ~D)"
               max-count next-counter)))
    (setf (anime-2d-image-caret anime-2d) next-counter)
    (let ((image-array (anime-info-image-array anime-info)))
      (setf (model-2d-mesh model)
            (make-image-mesh :image-name (aref image-array next-counter)
                             :color (anime-2d-color anime-2d)
                             :width (anime-2d-width anime-2d)
                             :height (anime-2d-height anime-2d))))))

