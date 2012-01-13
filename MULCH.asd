;;;; MULCH.asd

(asdf:defsystem #:mulch
  :serial t
  :depends-on (#:usocket)
  :components ((:file "package")
               (:file "mulch")))

