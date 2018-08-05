
(in-package :papyrith)

(defvar *bytecode*)
(defmacro emit (&rest instructions)
  `(progn
    ,@(loop for instruction in instructions
            collect `(emit-1 ,instruction))))

(defun emit-1 (instruction)
  (when (instruction-p instruction)
    (push instruction (rest (last *bytecode*)))))

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

(defmacro def-math-compiler (name operator)
  (let ((integer-name (symb 'integer- name))
        (float-name (symb 'float- name)))
    `(progn (def-simple-operator ,integer-name (:int :int) :int ,integer-name)
            (def-simple-operator ,float-name (:float :float) :float ,float-name)
            (def-dispatching-compiler ,operator
                                      (:float ,float-name)
                                      (:int ,integer-name)))))

(defmacro def-comparison-compiler (name)
  (let ((integer-name (symb 'integer- name))
        (float-name (symb 'float- name)))
    `(progn (def-simple-operator ,integer-name (:int :int) :bool ,integer-name)
            (def-simple-operator ,float-name (:float :float) :bool ,float-name)
            (def-dispatching-compiler ,name
                                      (:float ,float-name)
                                      (:int ,integer-name)))))

(defun compile-expressions (code)
  (mapcar #'compile-expression code))

; for literals and identifiers just return expr
; compiling a symbol should return an identifier
;   how will we handle labels?
; for expressions call relevant compiler function
; compiler function should return identifier
(defun compile-expression (expr &optional dest)
  (typecase expr
    (list (destructuring-bind (compiler-name . arguments) expr
            (when (and (member compiler-name *operators*)
                       dest)
              (setf arguments (append arguments (list dest))))
            (let ((compiler (get-compiler compiler-name)))
              (when compiler
                (apply compiler arguments)))))
    (symbol (case expr
              (true +true+)
              (false +false+)
              (t (lookup-identifier expr))))
    (t expr)))


(defun lookup-identifier (id)
  (or (lookup id (papyrus-function-local-table *function*))
      (lookup id (papyrus-function-param-table *function*))))

(defun lookup-property (id))

(defun lookup (id list)
  (loop for identifier in list
        when (eql (identifier-name identifier)
                  id)
          return identifier))

(defun compile-as (expr type)
  (autocast (compile-expression expr) type))

(defun compile-ref (expr)
  (typecase expr
    (symbol (compile-expression expr))
    (identifier expr)
    (list (case (first expr)
            (dot (list 'dot (compile-expression (second expr)) (third expr)))
            (aref (list 'aref (compile-expression (second expr))
                              (compile-expression (third expr))))))
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

(def-dispatching-compiler +
  (:string s+)
  (:float f+)
  (:int i+))

(def-math-compiler sub -)
(def-math-compiler mul *)
(def-math-compiler div /)

(def-simple-operator integer-mod (:int :int) :int integer-mod)

(def-compiler assign (dest arg1)
  (let ((value (compile-expression arg1 dest)))
    (unless (eq value dest)
      (emit (assign dest value))))
  dest)

(def-operator-compiler == (a b &optional dest)
  (setf a (compile-expression a)
        b (compile-expression b))
  (unless (type-match dest :bool)
    (setf dest (temp-identifier :bool)))
  (if (not (type-match a b))
    nil ; TODO: error here
    (emit (compare-eq dest a b)))
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

(def-compiler return (value)
  (emit (ret (compile-as value (papyrus-function-return-type *function*)))))

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

(def-comparison-compiler <)
(def-comparison-compiler <=)
(def-comparison-compiler >)
(def-comparison-compiler >=)

(def-operator-compiler && (arg1 arg2 &optional dest)
  (let ((end-label (new-label))
        (comparison (temp-identifier :bool)))
    (when (and dest
               (or (eq (identifier-scope dest) :local)
                   (eq (identifier-scope dest) :temp)))
      (setf comparison dest))
    (unless (type-match dest :bool)
      (setf dest comparison))
    (if (or (falsy-constant arg1)
            (falsy-constant arg2))
      (emit (assign dest +false+))
      (if (eq arg1 arg2)
        (emit (cast-as dest (compile-expression arg1)))
        (emit (cast-as comparison (compile-expression arg1 comparison))
              (jump-f comparison end-label)
              (cast-as comparison (compile-expression arg2 comparison))
              end-label
              (assign dest comparison)))))
  dest)

(def-operator-compiler || (arg1 arg2 &optional dest)
  (let ((end-label (new-label))
        (comparison (temp-identifier :bool)))
    (when (and dest
               (or (eq (identifier-scope dest) :local)
                   (eq (identifier-scope dest) :temp)))
      (setf comparison dest))
    (unless (type-match dest :bool)
      (setf dest comparison))
    (if (or (truthy-constant arg1)
            (truthy-constant arg2))
      (emit (assign dest +true+))
      (if (eq arg1 arg2)
        (emit (cast-as dest (compile-expression arg1)))
        (emit (cast-as comparison (compile-expression arg1 comparison))
              (jump-t comparison end-label)
              (cast-as comparison (compile-expression arg2 comparison))
              end-label
              (assign dest comparison)))))
  dest)

(def-simple-operator ! (+any+) :bool logical-not)

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
  (setf array (compile-expression array))
  (unless (and dest (type-match dest
                                (identifier-subtype array)))
    (setf dest (temp-identifier (identifier-subtype array))))
  (emit
    (array-get-element dest
                       array
                       (compile-as index :int)))
  dest)

(def-compiler variable (type name &optional value)
  (unless (lookup-identifier name))
    (let ((local (papyrus-local name type)))
      (push local (papyrus-function-local-table *function*))
      (when value
        (compile-expression value local))))

(defun compile-script (script)
  (let ((*script* script)
        (*object* (first (script-object-table script))))
    (dolist (*property* (papyrus-object-property-table *object*))
      (compile-property *property*))
    (dolist (*state* (papyrus-object-state-table *object*))
      (compile-state *state*))))

(defun compile-property (property)
  (dolist (function (papyrus-property-functions property))
    (compile-function function)))

(defun compile-state (state)
  (dolist (*function* (papyrus-state-functions state))
    (compile-function *function*)))

(defun compile-function (function)
  (let* ((*function* function)
         (code (compile-papyrus (papyrus-function-ast *function*))))
;    (format t "unoptimized-code:~%~{~A~%~}~%" code)
    (optimize-papyrus code)
    (when (first code)
      (setf (papyrus-function-code function) code)
      (setf (papyrus-function-local-table *function*)
            (remove-duplicates (loop with dest = nil
                  with scope = nil
                  for instruction in code
                  when (instruction-dest instruction)
                    do (setf dest (instruction-dest instruction)
                             scope (identifier-scope dest))
                    when (or (eq scope :local)
                             (eq scope :temp))
                      collect dest))))))
