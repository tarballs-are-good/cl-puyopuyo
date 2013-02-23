;;;; cl-puyopuyo.asd

(asdf:defsystem #:cl-puyopuyo
  :serial t
  :description "Describe cl-puyopuyo here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:lispbuilder-sdl
               #:bordeaux-threads
               #:lispbuilder-sdl-ttf
               #:lispbuilder-sdl-gfx)
  :components ((:file "package")
               (:file "cl-puyopuyo")))

