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
    (papyrus-type (funcall constructor :type (papyrus-type-type type)
                                       :subtype (papyrus-type-subtype type)))
    (string +string+)
    (integer +int+)
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

(defvar *autocast-rules* '((+bool+ +any+)
                           (+int+)
                           (+float+ +int+)
                           (+string+ +any+)
                           (+object+ +object+)
                           (+array+)
                           (+struct+)
                           (+var+ +bool+ +int+ +float+ +string+ +object+ +struct+)))

(defun type-match (type1 type2)
  (when (and type1 type2)
    (unless (papyrus-type-p type1)
      (setf type1 (papyrus-type type1)))
    (unless (papyrus-type-p type2)
      (setf type2 (papyrus-type type2)))
    (or (or (eq type1 +any+)
            (eq type2 +any+))
        (and (eq (papyrus-type-type type1)
                 (papyrus-type-type type2))
             (eq (papyrus-type-subtype type1)
                 (papyrus-type-subtype type2))))))

(defun typeof (a)
  (typecase a
    (papyrus-type a)
    (integer +int+)
    (float +float+)
    (string +string+)))

(defun papyrus-constant (val)
  (or (numberp val)
      (stringp val)
      (eq val +false+)
      (eq val +true+)
      (eq val +nonevar+)))

(defun falsy-constant (val)
  (when (papyrus-constant val)
    (or (eq val +false+)
        (typecase val
          (integer (eq val 0))
          (float (eq val 0.0))
          (string (string= val ""))))))

(defun truthy-constant (val)
  (when (papyrus-constant val)
    (or (eq val +true+)
        (not (falsy-constant val)))))

