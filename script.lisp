;;;; Structures for holding the different parts of a script.

(defvar *script*)
(defvar *object*)
(defvar *state*)
(defvar *function*)
(defvar *property*)

(defmacro defstruct-printing (name printer &rest slots)
  `(defstruct (,name
    (:print-function
      (lambda (p s k)
        (with-slots (,@(loop for slot in slots
                             if (listp slot)
                               collect (first slot)
                             else
                               collect slot)) p
          ,printer))))
    ,@slots))

(defstruct-printing script
  (format s "~A~%~A~%~
             .objectTable~%~
             ~{~A~%~}~
             .endObjectTable"
    info
    user-flags-ref
    object-table)
  info
  (user-flags-ref (make-user-flags-ref))
  (object-table (list (make-papyrus-object))))

(defstruct-printing papyrus-function
  (format s ".function ~A ~:[~;static~]~%~
             .userFlags ~A~%~
             .docString ~S~%~
             .return ~A~%~
             .paramTable~%~
             ~{~A~%~}~
             .endParamTable~%~
             .localTable~%~
             ~{~A~%~}~
             .endLocalTable~%~
             .code~%~
             ~{~A~%~}~
             .endCode~%~
             .endFunction" name static userflags docstring return-type
                           (mapcar #'print-parameter-definition param-table)
                           (mapcar #'print-local-definition local-table)
                           (mapcar #'print-instruction code))
  name
  (userflags 0)
  (docstring "")
  (return-type "NONE")
  (param-table (list))
  (local-table (list))
  code
  ast
  (static nil))

(defstruct-printing papyrus-state
  (if functions
    (format s ".state ~A~%~
               ~{~A~%~}~
               .endState" name functions))
  (name "")
  functions)

(defstruct-printing info
  (format s ".info~%~
             ~2T.source ~S~%~
             ~2T.modifyTime ~A~%~
             ~2T.compileTime ~A~%~
             ~2T.user ~S~%~
             ~2T.computer ~S~%~
             .endInfo"
    source
    modify-time
    compile-time
    user
    computer)
  (source "")
  (modify-time 0)
  (compile-time 0)
  (user "")
  (computer ""))

(defstruct-printing user-flags-ref
  (format s ".userFlagsRef~%~
             ~T.flag default ~A~%~
             ~T.flag mandatory ~A~%~
             ~T.flag collapsedonbase ~A~%~
             ~T.flag hidden ~A~%~
             ~T.flag collapsedonref ~A~%~
             ~T.flag conditional ~A~%~
             .endUserFlagsRef" default mandatory collapsedonbase hidden collapsedonref conditional)
  (default 2)
  (mandatory 5)
  (collapsedonbase 4)
  (hidden 0)
  (collapsedonref 3)
  (conditional 1))

(defstruct-printing papyrus-object
  (format s ".object ~A ~A~%~
             .userFlags ~A~%~
             .docString ~S~%~
             .autoState ~A~%~
             .structTable~%~
             ~{~A~%~}~
             .endStructTable~%~
             .variableTable~%~
             ~{~A~%~}~
             .endVariableTable~%~
             .propertyTable~%~
             ~{~A~%~}~
             .endPropertyTable~%~
             .propertyGroupTable~%~
             ~{~A~%~}~
             .endPropertyGroupTable~%~
             .stateTable~%~
             ~{~A~%~}~
             .endStateTable~%~
             .endObject" name extends user-flags docstring autostate struct-table
                         (mapcar #'print-variable-definition variable-table)
                         (mapcar #'print-property-definition property-table)
                         property-group-table state-table)
  name
  (extends "")
  (user-flags 0)
  (docstring "")
  (autostate "")
  (struct-table (list))
  (variable-table (list))
  (property-table (list))
  (property-group-table (list (make-property-group)))
  (state-table (list (make-papyrus-state))))

(defstruct-printing property-group
  (when properties
    (format s ".propertyGroup ~A~%~
               .userFlags ~A~%~
               .docString ~S~%~
               ~{~A~%~}~
               .endPropertyGroup" name user-flags docstring properties))
  (name "")
  (user-flags 0)
  (docstring "")
  properties)

(defstruct papyrus-struct
  members)

(defstruct papyrus-struct-member
  name
  type
  user-flags
  initial-value
  docstring)
