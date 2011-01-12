(defpackage :easy-match-system (:use :asdf :cl))
(in-package :easy-match-system)
     
(defsystem "easy-match"
  :description "easy-match: simple pattern matching."
  :version "0.2"
  :author "Kevin Bulusek <kbulusek@gmail.com>"
  :licence "BSD"
  :components ((:file "packages")
	       (:file "easy-match" :depends-on ("packages"))))
