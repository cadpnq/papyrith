(in-package :papyrith)

; from On Lisp
(defun mkstr (&rest args)
   (with-output-to-string (s)
     (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

; from Let Over Lambda
(defun |#`-reader| (stream sub-char numarg)
  (declare (ignore sub-char))
  (unless numarg (setq numarg 1))
  `(lambda ,(loop for i from 1 to numarg
                  collect (symb 'a i))
     ,(funcall
        (get-macro-character #\`) stream nil)))

(set-dispatch-macro-character
  #\# #\` #'|#`-reader|)

; from PAIP
(defun rest2 (x)
  "The rest of a list after the first TWO elements."
  (rest (rest x)))

(defun make-keyword (name) (values (intern (string-upcase name) "KEYWORD")))

(defun indent (s)
  (format nil "~{~2T~A~^~%~}" (lines (prin1-to-string s))))

(defun indent-list (l)
  (mapcar #'indent l))
