;;;; Define the instructions of the papyrus VM

(defmacro def-instruction (name &rest arguments) )
(defmacro def-instructions (names &rest arguments) )

(defstruct instruction
  op
  dest dest-type
  arg1 arg1-type
  arg2 arg2-type
  arg3 arg3-type
  arg4 arg4-type
  arg5 arg5-type
  parameters)

;;; def-instruction should expand into something like this. Note the slots for
;;; argument type. The type checker code will need these to generate the
;;; appropriate casting for us. I don't like using keywords for this, but meh,
;;; rough draft.

(defstruct (integer-add
  (:include instruction
            (op 'integer-add)
            (dest-type '(:integer-variable))
            (arg1-type '(:integer-variable :integer-constant))
            (arg2-type '(:integer-variable :integer-constant)))
  (:constructor integer-add2 (dest arg1 arg2))
  (:print-function
    (lambda (p s k)
      (format s "~A " (instruction-op p))
      (format s "~A, ~A, ~A"
              (instruction-dest p)
              (instruction-arg1 p)
              (instruction-arg2 p))))))

; (def-instructions (integer-add integr-sub integer-mul integer-div integer-mod)
;   dest arg1 arg2)
;
; (def-instruction integer-neg
;   dest arg1)
;
; (def-instructions (float-add float-sub float-mul float-div)
;   dest arg1 arg2)
;
; (def-instruction float-neg
;   dest arg1)
;
; (define-instruction not
;   dest arg1)
;
; (def-instructions (jump-t jump-f)
;   target condition)
;
; (def-instruction jump
;   target)
;
; (def-instruction ret
;   value)
;
; (def-instructions (cmp-eq cmp-lt cmp-lte cmp-gt cmp-gte)
;   dest arg1 arg2)
;
; (def-instructions (assign cast)
;   dest
;   arg1)
