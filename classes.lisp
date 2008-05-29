(in-package :vim)

(defclass vim-var ()
  ((name :initarg :name :accessor name-of)
   (var-type :initarg :type :accessor var-type-of)
   (values :initarg :values :accessor values-of)
   (init-func :initarg :init-func :accessor init-func-of)
   (doc :initarg :doc :accessor doc-of)))

