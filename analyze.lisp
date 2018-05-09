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
            (when (and next-instruction
                       (not (eq (instruction-dest instruction)
                                identifier)))
              (setf next (live-set identifier (cdr code) all-code visited)))
            (when (or (uses identifier instruction)
                      next)
              (setf this (list instruction)))
            (concatenate 'list this next branch))))))

(defun target (instruction code)
  (member-if (lambda (e) (equal e
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
  `(push (lambda (this bindings)
           (destructuring-bind (identifier instruction set) this
             (when (member (identifier-scope identifier)
                    ',scopes)
               ,@body)))
         *analyzers*))

(defmacro def-instruction-analyzer (instruction scopes &rest body)
  `(def-analyzer ,scopes
    (when (equal (instruction-op instruction)
                 ',instruction)
      ,@body)))

(def-analyzer (:local :temp)
  (unless set
    (setf (instruction-dest instruction) +nonevar+)
    t))

(defun papyrus-constant (val)
  (or (numberp val)
      (stringp val)
      (eq val +nonevar+)))

(def-instruction-analyzer assign (:local :temp)
  (let ((value (instruction-arg1 instruction))
        (siblings (siblings this bindings)))
    (when (and (not siblings)
               (papyrus-constant value)
               set)
      (rewrite-instructions set identifier value)
      (setf (instruction-dest instruction) +nonevar+)
      t)))

(def-analyzer (:temp)
  (loop with these = (concatenate 'list (siblings this bindings)
                                        (list this))
        with those = nil
        for binding in bindings
        for (identifier2 instruction2 set2) in bindings
        if (eq identifier identifier2)
          return nil
        else
          do (setf those (bindings-to identifier2 bindings))
             (when (and (eq (identifier-scope identifier2) :temp)
                        (eq (identifier-type identifier)
                            (identifier-type identifier2))
                        (not (loop for x in these
                                   thereis (loop for y in those
                                                 thereis (intersects x y)))))
               (mapcar #'(lambda (x) (rewrite-binding x identifier2)) these)
               (return t))))

(defun analyze (code)
  (let ((bindings (all-bindings code))
        (any-change nil))
    (loop for binding in bindings
      do (loop for analyzer in *analyzers*
           do (setf any-change (or (funcall analyzer binding bindings)
                                   any-change))))
    any-change))

(defun disjoint (binding1 binding2)
 (unless (eq binding1 binding2)
   (not (intersection (third binding1) (third binding2)))))

(defun disjoint-from-every (binding identifier bindings)
  (not (loop for binding2 in (intersecting-bindings binding bindings)
            thereis (eq identifier (first binding2)))))

(defun intersecting-bindings (binding1 bindings &optional self)
  (destructuring-bind (identifier1 instruction1 set1) binding1
   (loop for binding2 in bindings
         for (identifier2 instruction2 set2) in bindings
         unless (eq binding1 binding2)
           if (and self
                   (eq identifier1 identifier2)
                   (intersection set1 set2))
             collect binding2
           else
             when (intersection set1 set2)
               collect binding2)))

(defun rewrite-binding (binding new)
  (let ((old (first binding)))
   (setf (first binding) new
         (instruction-dest (second binding)) new)
   (loop for instruction in (third binding)
         do (rewrite-arguments instruction old new))))

(defun rewrite-arguments (instruction old new)
  (loop for slot in '(arg1 arg2 arg3 arg4 arg5)
       when (eq (slot-value instruction slot)
                 old)
         do (setf (slot-value instruction slot) new)))
