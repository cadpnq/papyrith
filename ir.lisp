(let ((ir-loaders (make-hash-table)))
  (defun put-ir-loader (name loader)
    (setf (gethash name ir-loaders) loader))

  (defun get-ir-loader (name)
    (gethash name ir-loaders)))

(defmacro def-ir-loader (name args &rest body)
  `(put-ir-loader ',name
    (lambda ,args
      ,@body)))

(defun load-ir (ir)
  (let ((loader (first ir))
        (arguments (rest ir)))
    (apply (get-ir-loader loader) arguments)))

(def-ir-loader script (name-and-properties &rest body)
  (let* ((*script* (make-script))
         (*object* (first (script-object-table *script*)))
         (*state* (first (papyrus-object-state-table *object*))))
    (destructuring-bind (name &key extends
                                   (source "")
                                   (modify-time 0)
                                   (compile-time 0)
                                   (user "papyrith")
                                   (computer "")
                                   (userflags 0)
                                   (docstring "")) name-and-properties
      (setf (papyrus-object-name *object*)
            name
            (script-info *script*)
            (make-info :source source
                       :modify-time modify-time
                       :compile-time compile-time
                       :user user
                       :computer computer))
      (loop for toplevel-form in body
        do (load-ir toplevel-form))
      *script*)))

(def-ir-loader function (name-and-properties &rest body)
  (when *state*
    (destructuring-bind (name &key (userflags 0)
                                   (docstring "")
                                   (return-type :none)
                                   parameters) name-and-properties
      (let ((*function* (make-papyrus-function :name name
                                               :userflags userflags
                                               :docstring docstring)))
        (when return-type
          (setf (papyrus-function-return-type *function*)
                (papyrus-type return-type)))
        (when parameters
          (setf (papyrus-function-param-table *function*)
                (loop for (type name default) in parameters
                      collect (papyrus-parameter name type default))))
        (setf (papyrus-function-ast *function*) body)
        (push *function* (slot-value *state* 'functions))))))
