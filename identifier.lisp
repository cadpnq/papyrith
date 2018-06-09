(defstruct (identifier
  (:include papyrus-type)
  (:print-function
    (lambda (p s k)
      (format s "~A" (identifier-name p)))))
   name scope)

(defvar +nonevar+ (make-identifier :type :none :name 'nonevar))
(defvar +self+ (make-identifier :type :self :name 'self))
(defvar +true+ (make-identifier :type :bool :name 'true))
(defvar +false+ (make-identifier :type :bool :name 'false))
(defvar +none+ (make-identifier :type :none :name 'none))

(defstruct (papyrus-parameter
  (:include identifier
            (scope :parameter)))
  default-value)

(defun print-parameter-definition (parameter)
  (format nil ".param ~A ~A" (papyrus-parameter-name parameter)
                             (print-type parameter)))

(defstruct (papyrus-local
  (:include identifier
            (scope :local))))

(defun print-local-definition (local)
  (format nil ".local ~A ~A" (papyrus-local-name local)
                             (print-type local)))

(defstruct (papyrus-variable
  (:include identifier
            (scope :variable)))
  (user-flags 0)
  initial-value)

(defun print-variable-definition (variable)
  (with-slots (name type user-flags initial-value) variable
    (format nil ".variable ~A ~A~%~
                 .userFlags ~A~%~
                 .initialValue ~A~%~
                 .endVariable" name type user-flags initial-value)))

(defstruct (papyrus-property
  (:include identifier
            (scope :property)))
  auto
  user-flags
  docstring
  autovar
  functions)

(defun print-property-definition (property)
  (with-slots (name type auto user-flags docstring autovar functions identifier) property
    (if auto
      (format s ".property ~A ~A auto~%~
                 .userFlags ~A~%~
                 .docString ~S~%~
                 .autoVar ~A~%~
                 .endProperty" name type user-flags docstring autovar)
      (format s ".property ~A ~A~%~
                 .userFlags ~A~%~
                 .docString ~S~%~
                 ~A~%~
                 .endProperty" name type user-flags docstring functions))))

(defun papyrus-local (name type)
  (let ((local (papyrus-type type #'make-papyrus-local)))
    (setf (papyrus-local-name local) name)
    local))

(defun papyrus-parameter (name type &optional value)
  (let ((parameter (papyrus-type type #'make-papyrus-parameter)))
    (setf (papyrus-parameter-name parameter) name
          (papyrus-parameter-default-value parameter) value)
     parameter))

(defun temp-identifier (type)
  (papyrus-local (gensym (mkstr "::" type)) type))
