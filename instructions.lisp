;;;; Define the instructions of the papyrus VM

(in-package :papyrith)

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

(defun new-label ()
  (label (gensym "label")))

(def-instructions
  ((integer-add iadd)
   (integer-sub isubtract)
   (integer-mul imultiply)
   (integer-div idivide)
   (integer-mod imod))
  dest
  arg1
  arg2)

(def-instruction integer-neg ineg
  dest
  arg1)

(def-instructions
  ((float-add fadd)
   (float-sub fsubtract)
   (float-mul fmultiply)
   (float-div fdivide))
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

(def-instruction is is
  dest
  arg1
  arg2)

(def-instruction nop nop)

(def-instruction string-cat strcat
  dest
  arg1
  arg2)

(def-instructions
  ((compare-eq compareeq)
   (compare-lt comparelt)
   (compare-lte comparelte)
   (compare-gt comparegt)
   (compare-gte comparegte))
  dest
  arg1
  arg2)

(def-instruction logical-not not
  dest
  arg1)

(def-instruction prop-get propget)
(def-instruction prop-set propset)

(def-instruction array-create arraycreate)
(def-instruction array-length arraylength)

(def-instruction array-get-element arraygetelement
  dest
  arg1
  arg2)

(def-instruction array-set-element arraysetelement
  arg1
  arg2
  arg3)

(def-instruction array-find-element arrayfindelement)
(def-instruction array-rfind-element arrayrfindelement)
(def-instruction array-add arrayadd)
(def-instruction array-insert arrayinsert)
(def-instruction array-remove-last arrayremovelast)
(def-instruction array-remove arrayremove)
(def-instruction array-clear arrayclear)
(def-instruction array-find-struct arrayfindstruct)
(def-instruction array-rfind-struct arrayrfindstruct)

(def-instruction struct-create structcreate)
(def-instruction struct-get structget)
(def-instruction struct-set structset)

(def-instruction call-method-papyrus callmethod)
(def-instruction call-parent callparent)
(def-instruction call-static callstatic)
