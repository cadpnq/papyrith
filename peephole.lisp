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
                `(remove-instruction instruction all-code))
              (replace-instruction (new-instruction)
                `(setf (first code) ,new-instruction)))
      ,@body)))

(defun remove-instruction (instruction code)
  (let* ((location (member instruction code))
         (previous
          (member (second (member (first location)
                                  (reverse code)))
                  code))
         (next (rest location)))
    (when location
      (cond
        ((and (not previous)
              (not next))
          (setf (first location) nil
                (rest location) nil))
        ((not next)
         (setf (rest previous) nil))
        (t
          (setf (first location) (second location)
                (rest location) (rest2 location)))))))

;;; When two labels are next to one another all branches pointing at the first
;;; can be rewritten to point to the second.
(def-optimizer (label)
  (when (label-p (second code))
    (let ((next-label (second code)))
      (loop for branch in (branches-to instruction all-code)
            do (setf (instruction-target branch) next-label)))
    t))

;;; When a label is followed by a jump all branches to the label can be
;;; rewritten to point to the target of the jump.
(def-optimizer (label)
  (when (jump-p (second code))
    (loop with destination = (instruction-target (second code))
          for branch in (branches-to instruction all-code)
          do (setf (instruction-target branch) destination))
    t))

;;; A label with no branches targeting it can be removed.
(def-optimizer (label)
  (unless (branches-to instruction all-code)
    (kill-instruction)
    t))

;;; All code between a jump and the next label is dead and can be removed.
;;; TODO: this also applies to the return instruction
(def-optimizer (jump)
  (unless (label-p (second code))
    (setf (rest code) (member-if #'label-p (rest code)))
    t))

;;; Branches followed by their target can be removed.
(def-optimizer (jump jump-f jump-t)
  (when (and (label-p (second code))
             (equal (instruction-target instruction)
                    (second code)))
    (kill-instruction)
    t))

;;; Assigning something to itself can be removed.
(def-optimizer (assign cast-as)
  (when (equal (instruction-dest instruction)
               (instruction-arg1 instruction))
    (kill-instruction)
    t))

;;; A math operation with two constants can be turned into an assign.
(defmacro def-math-optimizer (instructions operation)
  `(def-optimizer ,instructions
     (let ((arg1 (instruction-arg1 instruction))
           (arg2 (instruction-arg2 instruction)))
       (when (and (numberp arg1)
                  (numberp arg2))
         (replace-instruction (assign (instruction-dest instruction)
                                      (,operation arg1 arg2)))
         t))))

(def-math-optimizer (integer-add float-add) +)
(def-math-optimizer (integer-sub float-sub) -)
(def-math-optimizer (integer-mul float-mul) *)
(def-math-optimizer (integer-div float-div) /)

;;; Any instruction (except for function calls) where DEST is ::nonevar can be
;;; removed.
(def-optimizer (integer-add float-add integer-sub assign compare-gt compare-gte cast-as)
  (when (equal (instruction-dest instruction)
            +nonevar+)
    (kill-instruction)
    t))

(defun branches-to (target-label code)
  (loop for instruction in code
        when (equal target-label (instruction-target instruction))
          collect instruction))

(defun peephole (code)
 "Perform peephole optimization on assembly code."
 (when (first code)
   (let ((any-change nil))
     ;; Optimize each tail
     (loop for code-tail on code do
       (setf any-change (or (peephole-1 code-tail code)
                            any-change)))
     ;; If any changes were made, call optimize again
     (if any-change
         (peephole code)
         code)
     any-change)))

(defun peephole-1 (code all-code)
 "Perform peephole optimization on a tail of the assembly code.
 If a change is made, return true."
 ;; Data-driven by the opcode of the first instruction
 (let* ((instr (first code))
        (optimizers (get-optimizers (instruction-op instr))))
   (when (and optimizers instr)
     (loop for optimizer in optimizers
        thereis (funcall optimizer instr code all-code)))))

(defun optimize-papyrus (code)
  (loop with optimized = t
        with analyzed = t
    while (or optimized analyzed)
      do ;(setq code (remove nil code))
         (setq analyzed (analyze code))
         (setq optimized (peephole code))
    finally (return code)))
