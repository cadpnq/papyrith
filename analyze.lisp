(defun previous (e l)
  (second (member e (reverse l))))

(defun live-set (identifier code all-code &optional (visited '()))
    (unless (or (member (car code) visited)
                (not code))
      (let ((instruction (first code))
            (next-instruction (second code)))
        (when (instruction-p instruction)
          (if (not visited)
            (setf visited (list instruction))
            (push instruction (cdr (last visited))))
          (let (this next branch)
            (when (instruction-target instruction)
              (setf branch (live-set
                              identifier
                              (target instruction all-code)
                              all-code
                              visited)))
            (unless (or (not next-instruction)
                        (eq (instruction-dest next-instruction)
                            identifier))
              (setf next (live-set identifier (cdr code) all-code visited)))
            (when (or (uses identifier instruction)
                      next)
              (setf this (list instruction)))
            (concatenate 'list this next branch))))))

(defun target (instruction code)
  (member-if (lambda (e) (equal (instruction-name e)
                                (instruction-target instruction)))
               code))

(defun uses (identifier instruction)
 (with-slots (arg1 arg2 arg3 arg4 arg5 parameters) instruction
   (or (eq arg1 identifier)
       (eq arg2 identifier)
       (eq arg3 identifier)
       (eq arg4 identifier)
       (eq arg5 identifier)
       (loop for param in parameters
         thereis (eq parameters identifier)))))

(defun all-bindings (all-code)
 (loop with dest
       with instruction
       for code on all-code
       do (setf instruction (first code))
       when instruction
         do (setf dest (instruction-dest instruction))
         when dest
           collect (list dest
                         instruction
                         (live-set dest (cdr code) all-code))))


(defparameter *analyzers* (list))

(defmacro def-analyzer (scopes &rest body)
  `(push (lambda (binding bindings)
           (destructuring-bind (identifier instruction set) binding
             (when (member (identifier-scope identifier)
                    ',scopes)
               ,@body)))
         *analyzers*))

(def-analyzer (:local :temp)
  (unless set
    (setf (instruction-dest instruction) +nonevar+)
    t))

(defun analyze (code)
 (let ((bindings (all-bindings code))
       (any-change nil))
   (loop for binding in bindings
     do (loop for analyzer in *analyzers*
          do (setf any-change (or (funcall analyzer binding bindings)
                                  any-change))))
   any-change))
