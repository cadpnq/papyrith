(defun typeof (a)
  (typecase a
    (integer :integer)
    (float :float)
    (string :string)
    (identifier (identifier-type a))))

(defvar *bytecode* '(()))
(defun bytecode-append (&rest instructions)
  (loop for instruction in instructions
        do (when (instruction-p instruction)
             (setq *bytecode* (append *bytecode* `(,instruction))))))

(defun compile-papyrus (code)
  (let ((*bytecode* '(())))
    (compile-expressions code)
    (cdr *bytecode*)))

(defvar *compilers* (make-hash-table))
(defmacro def-compiler (name args &rest body)
  `(setf (gethash ',name *compilers*)
    (lambda ,args
      ,@body)))

(defvar *operators* '())
(defmacro def-operator-compiler (name args &rest body)
  `(progn
    (push ',name *operators*)
    (def-compiler ,name ,args ,@body)))

(defmacro def-binary-compiler (name arg1-type arg2-type dest-type inst)
  `(def-operator-compiler ,name (arg1 arg2 &optional dest)
    (unless dest
      (setq dest (temp-identifier ,dest-type)))
    (let ((arg1 (compile-expression arg1))
          (arg2 (compile-expression arg2))
          (dest-temp dest))
      (unless (eq (typeof dest) ,dest-type)
        (setq dest-temp (temp-identifier ,dest-type)))
      (bytecode-append
       (,inst dest-temp
         (autocast arg1 ,arg1-type)
         (autocast arg2 ,arg2-type)))
      (unless (eq dest dest-temp)
        (bytecode-append (cast-as dest dest-temp))))
    dest))

(defmacro def-dispatching-compiler (name &rest ops)
  `(def-operator-compiler ,name (arg1 arg2 &optional dest)
    (let* ((arg1 (compile-expression arg1))
           (arg2 (compile-expression arg2))
           (types (list (typeof arg1) (typeof arg2))))
     (cond
       ,@(loop for (type compiler) in ops
               collect `((member ,type types)
                         (let ((compiler ',compiler))
                           (compile-expression `(,compiler ,arg1 ,arg2) dest))))))))

(defmacro def-math-compiler (name)
 (let ((integer-name (symb 'integer- name))
        (float-name (symb 'float- name)))
   `(progn
     (def-binary-compiler ,integer-name :integer :integer :integer ,integer-name)
     (def-binary-compiler ,float-name :float :float :float ,float-name)
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
        (apply (gethash (car expr) *compilers*) arguments)))
    (t expr)))

(defun autocast (expr type &optional dest)
  (cond
    ((eq (typeof expr) type)
     expr)
    (t
     (let ((tmp (temp-identifier type)))
       (bytecode-append (cast-as tmp expr))
       tmp))))

(defun new-label ()
  (label (gensym "label")))

(def-binary-compiler i+ :integer :integer :integer integer-add)
(def-binary-compiler f+ :float :float :float float-add)
(def-binary-compiler s+ :string :string :string string-cat)

(def-dispatching-compiler plus
  (:string s+)
  (:float f+)
  (:integer i+))

(def-math-compiler sub)
(def-math-compiler mul)
(def-math-compiler div)

(def-binary-compiler integer-mod :integer :integer :integer integer-mod)

(def-compiler assign (dest arg1)
  (let ((value (compile-expression arg1 dest)))
    (unless (eq value dest)
      (bytecode-append (assign dest value))))
  dest)

(def-compiler if (&rest clauses)
  (let ((exit-label (new-label)))
    (loop for (antecedent . consequent) in clauses
          do (let ((clause-exit (new-label)))
               (bytecode-append
                 (jump-f (compile-expression antecedent) clause-exit))
               (bytecode-append
                 (compile-expressions consequent)
                 (jump exit-label)
                 clause-exit)))
    (bytecode-append exit-label)))
