(defpackage sample-cl-csr-2d-game/main
  (:nicknames :sample-cl-csr-2d-game)
  (:use :cl
        :sample-cl-csr-2d-game/server
        :sample-cl-csr-2d-game/admin/admin)
  (:export :start-sample
           :stop-sample))
