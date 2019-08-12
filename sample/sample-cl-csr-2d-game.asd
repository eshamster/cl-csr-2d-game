#|
  This file is a part of cl-csr-2d-game project.
  Copyright (c) 2019 eshamster (hamgoostar@gmail.com)
|#

#|
  A sample of cl-csr-2d-game in Common Lisp

  Author: eshamster (hamgoostar@gmail.com)
|#

(defsystem "sample-cl-csr-2d-game"
  :version "0.1.0"
  :author "eshamster"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :license "MIT"
  :depends-on (:cl-markup
               :alexandria
               :ningle
               :quri
               :proto-cl-client-side-rendering
               :sample-cl-csr-2d-game/main)
  :description "A sample of Client side rendering in Common Lisp")
