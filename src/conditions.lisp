(in-package #:colorlisp)


(define-condition colorlisp-error (error)
  ()
  (:documentation "Base condition for ColorLisp failures."))


(define-condition unsupported-language (colorlisp-error)
  ((designator
    :initarg :designator
    :reader unsupported-language-designator
    :documentation "The language designator that could not be resolved."))
  (:report
   (lambda (condition stream)
     (format stream "ColorLisp does not support the language ~S."
             (unsupported-language-designator condition))))
  (:documentation "Signaled when an explicit language designator is unknown."))


(define-condition native-build-error (colorlisp-error)
  ((command
    :initarg :command
    :reader native-build-error-command
    :documentation "The compiler command that failed.")
   (cause
    :initarg :cause
    :reader native-build-error-cause
    :documentation "The condition raised by the process runner."))
  (:report
   (lambda (condition stream)
     (format stream "Could not build ColorLisp's native Tree-sitter library: ~A"
             (native-build-error-cause condition))))
  (:documentation "Signaled when ColorLisp cannot build its native library."))


(define-condition query-error (colorlisp-error)
  ((language
    :initarg :language
    :reader query-error-language
    :documentation "The grammar whose highlight query failed.")
   (offset
    :initarg :offset
    :reader query-error-offset
    :documentation "The byte offset of the query error.")
   (type
    :initarg :type
    :reader query-error-type
    :documentation "The Tree-sitter query error category."))
  (:report
   (lambda (condition stream)
     (format stream "Invalid ~A highlight query at byte ~D (~A)."
             (query-error-language condition)
             (query-error-offset condition)
             (query-error-type condition))))
  (:documentation "Signaled when a bundled Tree-sitter query is invalid."))


(define-condition highlight-error (colorlisp-error)
  ((detail
    :initarg :detail
    :reader highlight-error-detail
    :documentation "A description of the highlighting failure."))
  (:report
   (lambda (condition stream)
     (write-string (highlight-error-detail condition) stream)))
  (:documentation "Signaled when a highlighting session cannot be completed."))
