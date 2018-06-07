;;;; Define the instructions of the papyrus VM

(defstruct instruction
  op asm target name dest arg1 arg2 arg3
  arg4 arg5 parameters)

(defmacro def-instruction (name asm &rest arguments)
  `(defstruct (,name
     (:include instruction
               (op ',name)
               (asm ',asm))
     (:constructor ,name ,arguments)
     (:print-function
       (lambda (p s k)
         (format s "~A ~{~A ~}"
           (instruction-asm p)
           (mapcar #'(lambda (a) (slot-value p a))
                   ',arguments)))))))

(defmacro def-instructions (names &rest arguments)
  `(progn ,@(loop for (name asm) in names
                  collect `(def-instruction ,name ,asm ,@arguments))))

(defstruct (label
  (:include instruction
            (op 'label))
  (:constructor label (name))
  (:print-function
    (lambda (p s k)
      (format s "~A" (instruction-name p))))))

(defun print-instruction (instruction)
  (if (label-p instruction)
    (format nil "~A:" instruction)
    (format nil "~A" instruction)))

(def-instructions
  ((integer-add iadd)
   (integer-sub isub)
   (integer-mul imul)
   (integer-div idiv)
   (integer-mod imod))
  dest
  arg1
  arg2)

(def-instruction integer-neg ineg
  dest
  arg1)

(def-instructions
  ((float-add fadd)
   (float-sub fsub)
   (float-mul fmul)
   (float-div fdiv))
  dest
  arg1
  arg2)

(def-instruction float-neg fneg
  dest
  arg1)

(def-instructions
  ((jump-t jumpt)
   (jump-f jumpf))
  arg1
  target)

(def-instruction jump jump
  target)

(def-instruction ret return
  arg1)

(def-instruction assign assign
  dest
  arg1)

(def-instruction cast-as cast
  dest
  arg1)

(def-instruction string-cat strcat
  dest
  arg1
  arg2)

(def-instructions
  ((compare-lt comparelt)
   (compare-lte comparelte)
   (compare-gt comparegt)
   (compare-gte comparegte))
  dest
  arg1
  arg2)

(def-instruction logical-not not
  dest
  arg1)

(def-instruction array-get-element arraygetelement
  dest
  arg1
  arg2)

(def-instruction array-set-element arraysetelement
  arg1
  arg2
  arg3)
