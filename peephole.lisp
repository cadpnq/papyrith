;;;; Peephole optimizer

;;; If this looks familiar then you've probably read PAIP. It is based on the
;;; optimizer from chapter 23.

(let ((optimizers (make-hash-table)))
  (defun put-optimizer (name optimizer)
    (unless (gethash name optimizers)
      (setf (gethash name optimizers) '()))
    (push optimizer (gethash name optimizers)))

  (defun get-optimizers (name)
    (gethash name optimizers)))

(defmacro def-optimizer (instructions &rest body)
  `(progn
    ,@(loop for instruction in instructions
            collect `(put-optimizer ',instruction
                      ,(make-optimizer body)))))

(defun make-optimizer (body)
  `(lambda (instruction code all-code)
    (macrolet ((kill-instruction ()
                `(setf (first code) (second code)
                       (rest code) (rest2 code)))
              (replace-instruction (new-instruction)
                `(setf (first code) ,new-instruction)))
      ,@body)))

(def-optimizer (label)
  (when (label-p (second code))
    (let ((next-label (second code)))
      (loop for branch in (branches-to instruction all-code)
            do (setf (instruction-target branch) next-label)))
    t))

(def-optimizer (label)
  (unless (branches-to instruction all-code)
    (kill-instruction)
    t))

(def-optimizer (jump)
  (unless (label-p (second code))
    (setf (rest code) (member-if #'label-p (rest code)))
    t))

(def-optimizer (jump jump-f jump-t)
  (when (and (label-p (second code))
             (equal (instruction-target instruction)
                    (second code)))
    (kill-instruction)
    t))

(def-optimizer (assign)
  (when (equal (instruction-dest instruction)
               (instruction-arg1 instruction))
    (kill-instruction)
    t))

(defun target (instruction code)
  (member-if (lambda (e) (equal))))

(defun branches-to (target-label code)
  (loop for instruction in code
        when (equal target-label (instruction-target instruction))
          collect instruction))

(defun optimize-papyrus (code)
  "Perform peephole optimization on assembly code."
  (let ((any-change nil))
    ;; Optimize each tail
    (loop for code-tail on code do
          (setf any-change (or (optimize-1 code-tail code)
                               any-change)))
    ;; If any changes were made, call optimize again
    (if any-change
        (optimize-papyrus code)
        code)))

(defun optimize-1 (code all-code)
  "Perform peephole optimization on a tail of the assembly code.
  If a change is made, return true."
  ;; Data-driven by the opcode of the first instruction
  (let* ((instr (first code))
         (optimizers (get-optimizers (instruction-op instr))))
    (when optimizers
      (loop for optimizer in optimizers
         thereis (funcall optimizer instr code all-code)))))