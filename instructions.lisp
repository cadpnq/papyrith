;;;; Define the instructions of the papyrus VM

(defmacro def-instruction (name asm &rest arguments)
  `(defstruct (,name
     (:include instruction
               (op ',name)
               (asm ',asm)
               ,@(loop for (arg type) in arguments
                       collect `(,(symb arg "-TYPE") ,type)))
     (:constructor ,name ,(mapcar #'car arguments))
     (:print-function
       (lambda (p s k)
         (format s "~A ~{~A~^, ~}~%"
           (instruction-asm p)
           (mapcar
            #'(lambda (a) (slot-value p (car a)))
            ',arguments)))))))

(defmacro def-instructions (names &rest arguments)
  `(progn ,@(loop for (name asm) in names
                  collect `(def-instruction ,name ,asm ,@arguments))))

(defstruct instruction
  op asm target name
  dest dest-type
  arg1 arg1-type
  arg2 arg2-type
  arg3 arg3-type
  arg4 arg4-type
  arg5 arg5-type
  parameters)

;;; def-instruction should expand into something like this. Note the slots for
;;; argument type. The type checker code will need these to generate the
;;; appropriate casting for us.

; (defstruct (integer-add
;   (:include instruction
;             (op 'integer-add)
;             (asm 'iadd)
;             (dest-type +integer-variable+)
;             (arg1-type +integer-any+)
;             (arg2-type +integer-any+))
;   (:constructor integer-add (dest arg1 arg2))
;   (:print-function
;     (lambda (p s k)
;       (format s "~A " (instruction-asm p))
;       (format s "~A, ~A, ~A"
;               (instruction-dest p)
;               (instruction-arg1 p)
;               (instruction-arg2 p))))))

(def-instructions
  ((integer-add iadd)
   (integer-sub isub)
   (integer-mul imul)
   (integer-div idiv)
   (integer-mod imod))
  (dest +integer-variable+)
  (arg1 +integer-any+)
  (arg2 +integer-any+))

(def-instruction integer-neg ineg
  (dest +integer-variable+)
  (arg1 +integer-any+))

(def-instructions
  ((float-add fadd)
   (float-sub fsub)
   (float-mul fmul)
   (float-div fdiv))
  (dest +float-variable+)
  (arg1 +float-any+)
  (arg2 +float-any+))

(def-instruction float-neg fneg
  (dest +float-variable+)
  (arg1 +float-any+))

(def-instruction not not
  (dest +bool-variable+)
  (arg1 +any-any+))

(def-instructions
  ((jump-t jmpt)
   (jump-f jmpf))
  (target +label+)
  (arg1 +bool-any+))

(def-instruction jump jmp
  (target +label+))

(def-instruction ret ret
  (arg1 +any-any+))

;
; (def-instructions (cmp-eq cmp-lt cmp-lte cmp-gt cmp-gte)
;   dest arg1 arg2)
;
; (def-instructions (assign cast)
;   dest
;   arg1)
