(in-package :papyrith)
(sb-ext:disable-debugger)

(opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :verbose
   :description "verbose output"
   :short #\v
   :long "verbose")
  (:name :dont-assemble
   :description "don't generate any .pex files"
   :long "dont-assemble")
  (:name :keep-assembly
   :description "output .pas files when compiling"
   :long "keep-assembly")
  (:name :keep-ir
   :description "output files containing the papyrith IR when compiling"
   :long "keep-ir"))

(defun unknown-option (condition)
  (format t "warning: ~S option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defmacro unless-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (unless it
       ,@body)))

(defun file-get-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun main ()
  (multiple-value-bind (options free-args)
      (handler-case
          (handler-bind ((opts:unknown-option #'unknown-option))
            (opts:get-opts))
        (opts:missing-arg (condition)
          (format t "fatal: option ~S needs an argument!~%"
                  (opts:option condition)))
        (opts:arg-parser-failed (condition)
          (format t "fatal: cannot parse ~S as argument of ~S~%"
                  (opts:raw-arg condition)
                  (opts:option condition)))
        (opts:missing-required-option (con)
          (format t "fatal: ~A~%" con)
          (opts:exit 1)))
    ;; Here all options are checked independently, it's trivial to code any
    ;; logic to process them.
    (when-option (options :help)
      (opts:describe :usage-of "papyrith"
                     :args "FILES"))
    (let ((file (first free-args))
          ir script)
      (when (and file (probe-file file))
        (in-package :papyrith)
        (if (ends-with-p ".pir" file)
          (setf ir (uiop:read-file-form file))
          (setf ir (parse-script-from-string (file-get-contents file))))
        (when-option (options :keep-ir)
          (to-file (concat (uiop:split-name-type file) ".pir") (prin1-to-string ir)))
        (setf script (load-ir ir))
        (compile-script script)
        (when-option (options :keep-assembly)
          (to-file (concat (uiop:split-name-type file) ".pas") (prin1-to-string script)))
        (unless-option (options :dont-assemble)
          (assemble-file (uiop:split-name-type file)))))))
                                      :executable t)
