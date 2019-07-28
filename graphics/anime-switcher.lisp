(defpackage cl-csr-2d-game/graphics/anime-switcher
  (:use :cl
        :cl-ps-ecs)
  (:export :anime-switcher
           :init-anime-switcher
           :switch-anime
           :get-current-anime-name
           :get-current-anime-2d)
  (:import-from :cl-csr-2d-game/graphics/anime
                :anime-2d
                :reset-anime
                :enable-anime
                :disable-anime))
(in-package :cl-csr-2d-game/graphics/anime-switcher)

;; --- data --- ;;

(defstruct anime-switcher
  entity
  current-name
  (name-to-anime-2d (make-hash-table)))

;; --- interface --- ;;

(defun init-anime-switcher (entity &rest name-to-anime-2d-pairs)
  "Initialize animation switcher and return it.
name-to-anime-2d-pairs := ((name anime-2d)...)
name := keyword"
  (check-type entity ecs-entity)
  (let ((switcher (make-anime-switcher :entity entity)))
    (dolist (pair name-to-anime-2d-pairs)
      (let ((name (car pair))
            (anime (cadr pair)))
        (check-type name keyword)
        (check-type anime anime-2d)
        (setf (gethash name (anime-switcher-name-to-anime-2d switcher))
              anime)))
    switcher))

(defun switch-anime (switcher name &key (stop-p nil) (forward-p t))
  (let ((entity (anime-switcher-entity switcher))
        (table (anime-switcher-name-to-anime-2d switcher)))
    (maphash (lambda (name anime-2d)
               (declare (ignore name))
               (disable-anime entity anime-2d))
             table)
    (setf (anime-switcher-current-name switcher) name)
    (let ((anime-2d (gethash name table)))
      (enable-anime entity anime-2d)
      (reset-anime anime-2d :stop-p stop-p :forward-p forward-p))))

(defun get-current-anime-name (switcher)
  (anime-switcher-current-name switcher))

(defun get-current-anime-2d (switcher)
  (gethash (get-current-anime-name switcher)
           (anime-switcher-name-to-anime-2d switcher)))
