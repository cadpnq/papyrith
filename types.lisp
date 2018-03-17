;;;; Define the papyrus data types

;;; How will we handle different object types? As it sits an ObjectReference
;;; or an Activator will both be an object, but how do we distinguish between
;;; them in parameter lists?

(defconstant +label+ '(label))

(defconstant +integer-variable+ '(integer-variable))
(defconstant +integer-literal+ '(integer-literal))
(defconstant +integer-any+ (append +integer-variable+ +integer-literal+))

(defconstant +float-variable+ '(float-variable))
(defconstant +float-literal+ '(float-literal))
(defconstant +float-any+ (append +float-variable+ +float-literal+))

(defconstant +bool-variable+ '(bool-variable))
(defconstant +bool-literal+ '(bool-literal))
(defconstant +bool-any+ (append +bool-variable+ +bool-literal+))

(defconstant +string-variable+ '(string-variable))
(defconstant +string-literal+ '(string-literal))
(defconstant +string-any+ (append +string-variable+ +string-literal+))

(defconstant +struct-variable+ '(struct-variable))

(defconstant +var-variable+ '(var-variable))

(defconstant +object-variable+ '(object-variable))

(defconstant +integer-array-variable+ '(integer-array-variable))
(defconstant +float-array-variable+ '(float-array-variable))
(defconstant +bool-array-variable+ '(bool-array-variable))
(defconstant +string-array-variable+ '(string-array-variable))
(defconstant +struct-array-variable+ '(struct-array-variable))
(defconstant +var-array-variable+ '(var-array-variable))
(defconstant +object-array-variable+ '(object-array-variable))

(defconstant +any-array+
  (append
   +integer-array-variable+
   +float-array-variable+
   +bool-array-variable+
   +string-array-variable+
   +struct-array-variable+
   +var-array-variable+
   +object-array-variable+))

(defconstant +any-any+
  (append
   +integer-any+
   +float-any+
   +bool-any+
   +string-any+
   +struct-variable+
   +var-variable+
   +object-variable+
   +any-array+))
