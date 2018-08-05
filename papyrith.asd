;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package #:asdf-user)

(defpackage :papyrith
   (:use :cl :str :anaphora))

(defsystem :papyrith
  :version "0.0.0"
  :description "papyrus compiler"
  :long-description "A compiler for the papyrus scripting language"
  :author "Nicky Nickell"
  :license "MIT"
  :serial t
  :depends-on (:uiop :str :anaphora)
  :components
  ((:file "auxfns")
   (:file "config")
   (:file "types")
   (:file "script")
   (:file "ir")
   (:file "parse")
   (:file "identifier")
   (:file "instructions")
   (:file "analyze")
   (:file "peephole")
   (:file "compile")
   (:file "assembler")))

(defsystem "papyrith/executable"
 :build-operation program-op
 :build-pathname "papyrith"
 :entry-point "papyrith::main"
 :depends-on (:unix-opts :papyrith :uiop :str :anaphora)
 :components ((:file "main")))
