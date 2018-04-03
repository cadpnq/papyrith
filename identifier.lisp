
(defstruct (identifier
  (:print-function
    (lambda (p s k)
      (format s "~A" (identifier-name p)))))
   name type scope)

(defun temp-int ()
  (make-identifier :name (gensym "temp_int")
                   :type :integer
                   :scope :temp))

(defun temp-float ()
 (make-identifier :name (gensym "temp_float")
                  :type :float
                  :scope :temp))

(defun temp-identifier (type)
  (make-identifier :name (gensym (string type))
                   :type type
                   :scope :temp))
