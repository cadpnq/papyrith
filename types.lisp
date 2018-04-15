;;;; Define the papyrus data types

;;; How will we handle different object types? As it sits an ObjectReference
;;; or an Activator will both be an object, but how do we distinguish between
;;; them in parameter lists?

(defvar +label+ '(label))

(defvar +integer-variable+ '(integer-variable))
(defvar +integer-literal+ '(integer-literal))
(defvar +integer-any+ (append +integer-variable+ +integer-literal+))

(defvar +float-variable+ '(float-variable))
(defvar +float-literal+ '(float-literal))
(defvar +float-any+ (append +float-variable+ +float-literal+))

(defvar +bool-variable+ '(bool-variable))
(defvar +bool-literal+ '(bool-literal))
(defvar +bool-any+ (append +bool-variable+ +bool-literal+))

(defvar +string-variable+ '(string-variable))
(defvar +string-literal+ '(string-literal))
(defvar +string-any+ (append +string-variable+ +string-literal+))

(defvar +struct-variable+ '(struct-variable))

(defvar +var-variable+ '(var-variable))

(defvar +object-variable+ '(object-variable))

(defvar +integer-array-variable+ '(integer-array-variable))
(defvar +float-array-variable+ '(float-array-variable))
(defvar +bool-array-variable+ '(bool-array-variable))
(defvar +string-array-variable+ '(string-array-variable))
(defvar +struct-array-variable+ '(struct-array-variable))
(defvar +var-array-variable+ '(var-array-variable))
(defvar +object-array-variable+ '(object-array-variable))

(defvar +any-array+
  (append
   +integer-array-variable+
   +float-array-variable+
   +bool-array-variable+
   +string-array-variable+
   +struct-array-variable+
   +var-array-variable+
   +object-array-variable+))

(defvar +any-variable+
 (append
   +integer-variable+
   +float-variable+
   +bool-variable+
   +string-variable+
   +struct-variable+
   +var-variable+
   +object-variable+
   +any-array+))

(defvar +any-any+
  (append
   +integer-any+
   +float-any+
   +bool-any+
   +string-any+
   +struct-variable+
   +var-variable+
   +object-variable+
   +any-array+))
