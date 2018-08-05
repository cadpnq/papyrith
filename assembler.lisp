
(in-package :papyrith)

(defun assemble-file (f)
  (print (uiop:run-program (list "PapyrusAssembler.exe" (uiop:split-name-type f)) :output t :error-output t)))
