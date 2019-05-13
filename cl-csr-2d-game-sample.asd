#|
  This file is a part of cl-csr-2d-game project.
  Copyright (c) 2019 eshamster
|#

(in-package :cl-user)
(defpackage cl-csr-2d-game-sample-asd
  (:use :cl :asdf))
(in-package :cl-csr-2d-game-sample-asd)

(defsystem cl-csr-2d-game-sample
  :version "0.1"
  :author "eshamster"
  :license "LLGPL"
  :depends-on (:cl-csr-2d-game
               :parenscript
               :ps-experiment
               :cl-ps-ecs
               :cl-markup
               :clack
               :ningle)
  :components ((:module "sample"
                :serial t
                :components
                ((:file "common")
                 (:file "sample-animation")
                 (:file "sample-basic-models")
                 (:file "sample-collision")
                 (:file "sample-input")
                 (:file "sample-storage")
                 (:file "sample-simple")
                 (:file "sample-text")
                 (:file "sample-texture")
                 (:file "sample-ui")
                 (:file "cl-csr-2d-game-sample"))))
  :description "Sample for cl-csr-2d-game")
