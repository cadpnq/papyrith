;;;; Papyrus data type information

(defstruct (papyrus-type
  (:print-function
    (lambda (p s k)
      (format s "~A" (print-type p)))))
  type
  subtype)

(defun print-type (type)
  (with-slots (type subtype) type
    (case type
      (:array (format nil "~A[]" subtype))
      ((:struct :object) (format nil "~A" subtype))
      (t (format nil "~A" type)))))

(defun papyrus-type (type &optional (constructor #'make-papyrus-type))
  (typecase type
    (keyword (funcall constructor :type type))
    (list (funcall constructor :type (first type)
                               :subtype (second type)))))

(defvar +any+ (papyrus-type :any))
(defvar +int+ (papyrus-type :int))
(defvar +float+ (papyrus-type :float))
(defvar +bool+ (papyrus-type :bool))
(defvar +string+ (papyrus-type :string))
(defvar +struct+ (papyrus-type :struct))
(defvar +array+ (papyrus-type :array))
(defvar +var+ (papyrus-type :var))
(defvar +object+ (papyrus-type :object))
(defvar +label+ (papyrus-type :label))

(defun type-match (type1 type2)
  (unless (papyrus-type-p type1)
    (setf type1 (papyrus-type type1)))
  (unless (papyrus-type-p type2)
    (setf type2 (papyrus-type type2)))
  (or (or (eq type1 +any+)
          (eq type2 +any+))
      (and (eq (papyrus-type-type type1)
               (papyrus-type-type type2))
           (eq (papyrus-type-subtype type1)
               (papyrus-type-subtype type2)))))

(defun typeof (a)
  (typecase a
    (papyrus-type a)
    (integer +int+)
    (float +float+)
    (string +string+)))

