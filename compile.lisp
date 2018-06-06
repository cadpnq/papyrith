
(defvar +nonevar+ (make-identifier :type :none :name 'nonevar))

(defvar *bytecode*)
(defmacro bytecode (&rest instructions)
  `(progn
    ,@(loop for instruction in instructions
            collect `(bytecode-1 ,instruction))))

(defun bytecode-1 (instruction)
  (when (instruction-p instruction)
    (push instruction (cdr (last *bytecode*)))))

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

(defmacro def-binary-compiler (name arg1-type arg2-type dest-type inst)
  `(def-operator-compiler ,name (arg1 arg2 &optional dest)
    (unless dest
      (setq dest (temp-identifier ,dest-type)))
    (let ((arg1 (compile-expression arg1))
          (arg2 (compile-expression arg2))
          (dest-temp dest))
      (unless (eq (typeof dest) ,dest-type)
        (setq dest-temp (temp-identifier ,dest-type)))
      (bytecode
       (,inst dest-temp
         (autocast arg1 ,arg1-type)
         (autocast arg2 ,arg2-type)))
      (unless (eq dest dest-temp)
        (bytecode (cast-as dest dest-temp))))
     dest))

(defmacro def-urnary-compiler (name arg1-type dest-type inst)
  `(def-operator-compiler ,name (arg1 &optional dest)
    (unless dest
      (setq dest (temp-identifier ,dest-type)))
    (let ((arg1 (compile-expression arg1))
          (dest-temp dest))
      (unless (eq (typeof dest) ,dest-type)
        (setq dest-temp (temp-identifier ,dest-type)))
      (bytecode
       (,inst dest-temp
         (autocast arg1 ,arg1-type)))
      (unless (eq dest dest-temp)
        (bytecode (cast-as dest dest-temp))))
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

(defmacro def-comparison-compiler (name)
  (let ((integer-name (symb 'integer- name))
        (float-name (symb 'float- name)))
    `(progn
      (def-binary-compiler ,integer-name :integer :integer :bool ,name)
      (def-binary-compiler ,float-name :float :float :bool ,name)
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

(defun autocast (expr type &optional dest)
  (cond
    ((eq (typeof expr) type)
     expr)
    (t
     (let ((tmp (temp-identifier type)))
       (bytecode (cast-as tmp expr))
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
      (bytecode (assign dest value))))
  dest)

(def-compiler if (&rest clauses)
  (let ((exit-label (new-label)))
    (loop for (antecedent . consequent) in clauses
          do (let ((clause-exit (new-label)))
               (bytecode
                 (jump-f (compile-expression antecedent) clause-exit)
                 (compile-expressions consequent)
                 (jump exit-label)
                 clause-exit)))
    (bytecode exit-label)))

(defvar *break-label*)
(defvar *continue-label*)
(def-compiler break ()
  (if *break-label*
    (bytecode (jump *break-label*))
    ; should signal an error here
    ))

(def-compiler continue ()
  (if *continue-label*
    (bytecode (jump *continue-label*))
    ; should signal an error here
    ))

(def-compiler while (condition &rest body)
  (let ((*break-label* (new-label))
        (*continue-label* (new-label)))
    (bytecode
      *continue-label*
      (jump-f (compile-expression condition) *break-label*)
      (compile-expressions body)
      (jump *continue-label*)
      *break-label*)))

(def-compiler for (initializer condition step &rest body)
  (let ((*break-label* (new-label))
        (*continue-label* (new-label))
        (entry-label (new-label)))
    (bytecode
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
    (bytecode
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
    (bytecode
      (cast-as comparison (compile-expression arg1 comparison))
      (jump-t comparison end-label)
      (cast-as comparison (compile-expression arg2 comparison))
      end-label
      (assign dest comparison)))
  dest)

(def-urnary-compiler not :bool :bool logical-not)

(def-compiler dot (a b)
  (list 'dot a b))

(def-compiler = (dest value)
  (let ((dest (compile-ref dest)))
    (typecase dest
      (list
        (case (first dest)
          (aref (bytecode (array-set-element (second dest)
                                             (third dest)
                                             (compile-expression value))))))
      (identifier
        (compile-expression value dest)))))

(def-operator-compiler aref (array index &optional dest)
  (unless dest
    (setf dest (temp-identifier (identifier-subtype array))))
  (bytecode
    (array-get-element dest
                       (compile-expression array)
                       (compile-expression index)))
  dest)
