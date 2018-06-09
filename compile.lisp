(defvar *bytecode*)
(defmacro emit (&rest instructions)
  `(progn
    ,@(loop for instruction in instructions
            collect `(when (instruction-p ,instruction)
                      (push ,instruction (rest (last *bytecode*)))))))

(defun compile-papyrus (code)
  (let ((*bytecode* (list (list))))
    (compile-expressions code)
    (cdr *bytecode*)))

(let ((compilers (make-hash-table)))
  (defun put-compiler (name compiler)
    (setf (gethash name compilers) compiler))

  (defun get-compiler (name)
    (gethash name compilers)))

(defmacro def-compiler (name args &rest body)
  `(put-compiler ',name
    (lambda ,args
      ,@body)))

(defvar *operators* '())
(defmacro def-operator-compiler (name args &rest body)
  `(progn
    (push ',name *operators*)
    (def-compiler ,name ,args ,@body)))

(defmacro def-simple-operator (name arg-types dest-type instruction)
  (let ((args (loop for arg in arg-types
                    for i from 1
                    collect (symb 'arg i))))
  `(def-operator-compiler ,name (,@args &optional dest)
    (unless (type-match dest ,dest-type)
      (setf dest (temp-identifier ,dest-type)))
    (let ,(loop for arg-type in arg-types
                for arg in args
                collect `(,arg (compile-as ,arg ,arg-type)))
      (emit (,instruction dest ,@args))
      dest))))

(defmacro def-dispatching-compiler (name &rest ops)
  `(def-operator-compiler ,name (arg1 arg2 &optional dest)
    (let ((arg1 (compile-expression arg1))
          (arg2 (compile-expression arg2)))
     (cond
       ,@(loop for (type compiler) in ops
               collect `((or (type-match ,type arg1)
                             (type-match ,type arg2))
                         (let ((compiler ',compiler))
                           (compile-expression `(,compiler ,arg1 ,arg2) dest))))))))

(defmacro def-math-compiler (name)
  (let ((integer-name (symb 'integer- name))
        (float-name (symb 'float- name)))
    `(progn (def-simple-operator ,integer-name (:integer :integer) :integer ,integer-name)
            (def-simple-operator ,float-name (:float :float) :float ,float-name)
            (def-dispatching-compiler ,name
                                      (:float ,float-name)
                                      (:integer ,integer-name)))))

(defmacro def-comparison-compiler (name)
  (let ((integer-name (symb 'integer- name))
        (float-name (symb 'float- name)))
    `(progn (def-simple-operator ,integer-name (:integer :integer) :bool ,integer-name)
            (def-simple-operator ,float-name (:float :float) :bool ,float-name)
            (def-dispatching-compiler ,name
                                      (:float ,float-name)
                                      (:integer ,integer-name)))))

(defun compile-expressions (code)
  (mapcar #'compile-expression code))

; for literals and identifiers just return expr
; compiling a symbol should return an identifier
;   how will we handle labels?
; for expressions call relevant compiler function
; compiler function should return identifier
(defun compile-expression (expr &optional dest)
  (typecase expr
    (list
      (let ((compiler (car expr))
            (arguments (cdr expr)))
        (when (and (member compiler *operators*)
                   dest)
          (setq arguments (append arguments `(,dest))))
        (apply (get-compiler compiler) arguments)))
    (t expr)))

(defun compile-ref (expr)
  (typecase expr
    (symbol (compile-expression expr))
    (identifier expr)
    (list
      (case (first expr)
        (dot
          (print (second expr))
          (print (compile-expression (second expr)))
          `(dot ,(compile-expression (second expr)) ,(third expr)))
        (aref `(aref ,(compile-expression (second expr))
                     ,(compile-expression (third expr))))))
    (t expr)))

(defun autocast (value type)
  (if (type-match value type)
    value
    (let ((temp (temp-identifier type)))
      (emit (cast-as temp value))
      temp)))

(def-simple-operator i+ (:int :int) :int integer-add)
(def-simple-operator f+ (:float :float) :float float-add)
(def-simple-operator s+ (:string :string) :string string-cat)

(def-dispatching-compiler plus
  (:string s+)
  (:float f+)
  (:int i+))

(def-math-compiler sub)
(def-math-compiler mul)
(def-math-compiler div)

(def-simple-operator integer-mod (:integer :integer) :integer integer-mod)

(def-compiler assign (dest arg1)
  (let ((value (compile-expression arg1 dest)))
    (unless (eq value dest)
      (emit (assign dest value))))
  dest)

(def-compiler if (&rest clauses)
  (let ((exit-label (new-label)))
    (loop for (antecedent . consequent) in clauses
          do (let ((clause-exit (new-label)))
               (emit
                 (jump-f (compile-expression antecedent) clause-exit)
                 (compile-expressions consequent)
                 (jump exit-label)
                 clause-exit)))
    (emit exit-label)))

(defvar *break-label*)
(defvar *continue-label*)
(def-compiler break ()
  (if *break-label*
    (emit (jump *break-label*))
    ; should signal an error here
    ))

(def-compiler continue ()
  (if *continue-label*
    (emit (jump *continue-label*))
    ; should signal an error here
    ))

(def-compiler while (condition &rest body)
  (let ((*break-label* (new-label))
        (*continue-label* (new-label)))
    (emit
      *continue-label*
      (jump-f (compile-expression condition) *break-label*)
      (compile-expressions body)
      (jump *continue-label*)
      *break-label*)))

(def-compiler for (initializer condition step &rest body)
  (let ((*break-label* (new-label))
        (*continue-label* (new-label))
        (entry-label (new-label)))
    (emit
      (compile-expression initializer)
      (jump entry-label)
      *continue-label*
      (compile-expression step)
      entry-label
      (jump-f (compile-expression condition) *break-label*)
      (compile-expressions body)
      (jump *continue-label*)
      *break-label*)))

(def-comparison-compiler compare-lt)
(def-comparison-compiler compare-lte)
(def-comparison-compiler compare-gt)
(def-comparison-compiler compare-gte)

(def-operator-compiler and (arg1 arg2 &optional dest)
  (let ((end-label (new-label))
        (comparison (temp-identifier :bool)))
    (unless dest
      (setq dest (temp-identifier :bool)))
    (emit
      (cast-as comparison (compile-expression arg1 comparison))
      (jump-f comparison end-label)
      (cast-as comparison (compile-expression arg2 comparison))
      end-label
      (assign dest comparison)))
  dest)

(def-operator-compiler or (arg1 arg2 &optional dest)
  (let ((end-label (new-label))
        (comparison (temp-identifier :bool)))
    (unless dest
      (setq dest (temp-identifier :bool)))
    (emit
      (cast-as comparison (compile-expression arg1 comparison))
      (jump-t comparison end-label)
      (cast-as comparison (compile-expression arg2 comparison))
      end-label
      (assign dest comparison)))
  dest)

(def-simple-operator not (+any+) :bool logical-not)

(def-compiler dot (a b)
  (list 'dot a b))

(def-compiler = (dest value)
  (let ((dest (compile-ref dest)))
    (typecase dest
      (list
        (case (first dest)
          (aref (emit (array-set-element (second dest)
                                             (third dest)
                                             (compile-expression value))))))
      (identifier
        (compile-expression value dest)))))

(def-operator-compiler aref (array index &optional dest)
  (unless dest
    (setf dest (temp-identifier (identifier-subtype array))))
  (emit
    (array-get-element dest
                       (compile-expression array)
                       (compile-expression index)))
  dest)
