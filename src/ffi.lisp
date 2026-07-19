(in-package #:colorlisp)


(cffi:defcfun ("colorlisp_session_new" colorlisp--session-new) :pointer
  (language-name :string)
  (source :pointer)
  (source-length :uint32)
  (query-source :pointer)
  (query-length :uint32)
  (error-offset :pointer)
  (error-type :pointer))

(cffi:defcfun ("colorlisp_session_delete" colorlisp--session-delete) :void
  (session :pointer))

(cffi:defcfun ("colorlisp_session_next_match" colorlisp--session-next-match) :boolean
  (session :pointer)
  (pattern-index :pointer)
  (capture-count :pointer))

(cffi:defcfun ("colorlisp_session_capture" colorlisp--session-capture) :boolean
  (session :pointer)
  (capture-position :uint32)
  (capture-id :pointer)
  (start-byte :pointer)
  (end-byte :pointer))

(cffi:defcfun ("colorlisp_session_capture_name" colorlisp--session-capture-name)
    :pointer
  (session :pointer)
  (capture-id :uint32)
  (length :pointer))

(cffi:defcfun ("colorlisp_session_predicate_step_count"
               colorlisp--session-predicate-step-count)
    :uint32
  (session :pointer)
  (pattern-index :uint32))

(cffi:defcfun ("colorlisp_session_predicate_step"
               colorlisp--session-predicate-step)
    :boolean
  (session :pointer)
  (pattern-index :uint32)
  (position :uint32)
  (step-type :pointer)
  (value-id :pointer))

(cffi:defcfun ("colorlisp_session_string_value"
               colorlisp--session-string-value)
    :pointer
  (session :pointer)
  (string-id :uint32)
  (length :pointer))


(defmacro with-colorlisp-session ((variable language-name source-octets
                                   query-octets)
                                  &body body)
  "Bind VARIABLE to a native highlighting session during BODY."
  `(progn
     (native-ensure-loaded)
     (cffi:with-pointer-to-vector-data (source-pointer ,source-octets)
       (cffi:with-pointer-to-vector-data (query-pointer ,query-octets)
         (cffi:with-foreign-objects ((error-offset :uint32)
                                     (error-type :uint32))
           (let ((,variable
                   (colorlisp--session-new
                    ,language-name
                    source-pointer
                    (length ,source-octets)
                    query-pointer
                    (length ,query-octets)
                    error-offset
                    error-type)))
             (when (cffi:null-pointer-p ,variable)
               (error 'query-error
                      :language ,language-name
                      :offset (cffi:mem-ref error-offset :uint32)
                      :type (colorlisp--query-error-name
                             (cffi:mem-ref error-type :uint32))))
             (unwind-protect
                  (progn ,@body)
               (colorlisp--session-delete ,variable))))))))


(defun colorlisp--foreign-string (pointer length)
  "Decode LENGTH UTF-8 bytes at POINTER into a Lisp string."
  (cffi:foreign-string-to-lisp pointer
                               :count length
                               :encoding :utf-8))


(defun colorlisp--query-error-name (value)
  "Return a readable name for a Tree-sitter query error VALUE."
  (case value
    (0 :none)
    (1 :syntax)
    (2 :node-type)
    (3 :field)
    (4 :capture)
    (5 :structure)
    (6 :language)
    (#.(1- (expt 2 32)) :unknown-language)
    (#.(- (expt 2 32) 2) :allocation)
    (#.(- (expt 2 32) 3) :incompatible-language)
    (#.(- (expt 2 32) 4) :parse)
    (#.(- (expt 2 32) 5) :cursor-allocation)
    (otherwise value)))
