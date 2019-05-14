#|
  This file is a part of cl-csr-2d-game project.
  Copyright (c) 2019 eshamster
|#

#|
  A library to create 2d game using Parenscript and three.js

  Author: eshamster
|#

(defsystem cl-csr-2d-game
  :version "0.1"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :author "eshamster"
  :license "LLGPL"
  :depends-on (:parenscript
               :ps-experiment
               :dexador
               :cl-ps-ecs
               :cl-csr-2d-game/main
               :proto-cl-client-side-rendering)
  :description "A library to create 2d game using Parenscript and three.js"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-csr-2d-game/t))))

(defsystem cl-csr-2d-game/t
  :class :package-inferred-system
  :depends-on (:cl-csr-2d-game
               :ps-experiment/t
               :cl-ppcre
               :rove
               :alexandria
               "ps-experiment/t/test-utils"
               "cl-csr-2d-game/t/test-utils"
               "cl-csr-2d-game/t/utils"
               "cl-csr-2d-game/t/logger"
               "cl-csr-2d-game/t/game-state"
               "cl-csr-2d-game/t/basic-components"
               "cl-csr-2d-game/t/calc"
               "cl-csr-2d-game/t/collision"
               "cl-csr-2d-game/t/collision-system"
               "cl-csr-2d-game/t/script-system"
               "cl-csr-2d-game/t/stage-generator"
               "cl-csr-2d-game/t/basic-generator")
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
