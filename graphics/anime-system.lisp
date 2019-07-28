(defpackage cl-csr-2d-game/graphics/anime-system
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs)
  (:export :make-anime-system)
  (:import-from :cl-csr-2d-game/graphics/anime
                :anime-2d
                :process-anime))
(in-package :cl-csr-2d-game/graphics/anime-system)

(defstruct
    (anime-system
      (:include ecs-system
                (target-component-types '(anime-2d))
                (process (lambda (entity)
                           (do-ecs-components-of-entity
                               (anime-2d entity :component-type 'anime-2d)
                             (process-anime anime-2d)))))))
