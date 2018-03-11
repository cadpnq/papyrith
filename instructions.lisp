;;;; Define the instructions of the papyrus VM

(defmacro def-instruction (name &rest arguments)
  `(defstruct (,name (:constructor ,name ,arguments))
     (op ',name)
     dest arg1 arg2 condition target name))

;;; I'm not entirely happy with this -- will refactor later when writing the type checker
(defmacro def-instructions (names &rest arguments)
  `(progn ,@(loop for name in names
              collect `(def-instruction ,name ,@arguments))))

(def-instructions (integer-add integr-sub integer-mul integer-div integer-mod)
  dest arg1 arg2)

(def-instruction integer-neg
  dest arg1)

(def-instructions (float-add float-sub float-mul float-div)
  dest arg1 arg2)

(def-instruction float-neg
  dest arg1)

(define-instruction not
  dest arg1)

(def-instructions (jump-t jump-f)
  target condition)

(def-instruction jump
  target)

(def-instruction ret
  value)

(def-instructions (cmp-eq cmp-lt cmp-lte cmp-gt cmp-gte)
  dest arg1 arg2)

(def-instructions (assign cast)
  dest arg1)
