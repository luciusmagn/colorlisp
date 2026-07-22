(in-package #:colorlisp)


(defstruct (span
            (:constructor span--create (start end category capture pattern)))
  "A semantic highlight over character offsets START through END."
  (start 0 :type fixnum :read-only t)
  (end 0 :type fixnum :read-only t)
  (category :plain :type keyword :read-only t)
  (capture "" :type string :read-only t)
  (pattern 0 :type fixnum :read-only t))


(defstruct (segment
            (:constructor segment--create (start end category text)))
  "A complete source segment with a semantic CATEGORY and TEXT."
  (start 0 :type fixnum :read-only t)
  (end 0 :type fixnum :read-only t)
  (category :plain :type keyword :read-only t)
  (text "" :type string :read-only t))


(defstruct (raw-capture
            (:constructor raw-capture--create
                (start-byte end-byte capture-id capture-name pattern)))
  "A native query capture expressed in UTF-8 byte offsets."
  (start-byte 0 :type fixnum)
  (end-byte 0 :type fixnum)
  (capture-id 0 :type fixnum)
  (capture-name "" :type string)
  (pattern 0 :type fixnum))


(defvar *query-cache* (make-hash-table :test #'equal)
  "Highlight query text cached by relative pathname.")


(defun colorlisp--query-source (relative-pathname)
  "Read and cache a bundled query at RELATIVE-PATHNAME."
  (or (gethash relative-pathname *query-cache*)
      (setf (gethash relative-pathname *query-cache*)
            (uiop:read-file-string
             (merge-pathnames relative-pathname
                              (colorlisp--source-root))))))


(defun colorlisp--utf-8-byte-map (source)
  "Return a vector mapping UTF-8 boundary offsets in SOURCE to character offsets."
  (let* ((octet-count (length (babel:string-to-octets source :encoding :utf-8)))
         (mapping     (make-array (1+ octet-count)
                                  :element-type 'fixnum
                                  :initial-element -1))
         (byte        0))
    (loop for character across source
          for character-index from 0
          for code = (char-code character)
          do (setf (aref mapping byte) character-index)
             (incf byte
                   (cond
                     ((<= code #x7f) 1)
                     ((<= code #x7ff) 2)
                     ((<= code #xffff) 3)
                     (t 4))))
    (setf (aref mapping byte) (length source))
    mapping))


(defun colorlisp--capture-name (session capture-id)
  "Return CAPTURE-ID's query capture name in SESSION."
  (cffi:with-foreign-object (length :uint32)
    (let ((pointer (colorlisp--session-capture-name session capture-id length)))
      (colorlisp--foreign-string pointer (cffi:mem-ref length :uint32)))))


(defun colorlisp--query-string (session string-id)
  "Return STRING-ID's query string in SESSION."
  (cffi:with-foreign-object (length :uint32)
    (let ((pointer (colorlisp--session-string-value session string-id length)))
      (colorlisp--foreign-string pointer (cffi:mem-ref length :uint32)))))


(defun colorlisp--current-match-captures (session pattern capture-count)
  "Copy the current native match's CAPTURE-COUNT captures."
  (loop for position below capture-count
        collect
        (cffi:with-foreign-objects ((capture-id :uint32)
                                    (start-byte :uint32)
                                    (end-byte :uint32))
          (unless (colorlisp--session-capture session position
                                              capture-id start-byte end-byte)
            (error 'highlight-error
                   :detail "Tree-sitter returned an invalid capture position."))
          (let ((id (cffi:mem-ref capture-id :uint32)))
            (raw-capture--create
             (cffi:mem-ref start-byte :uint32)
             (cffi:mem-ref end-byte :uint32)
             id
             (colorlisp--capture-name session id)
             pattern)))))


(defun colorlisp--predicate-steps (session pattern)
  "Return PATTERN's predicate steps as Lisp operands."
  (let ((count (colorlisp--session-predicate-step-count session pattern)))
    (loop for position below count
          collect
          (cffi:with-foreign-objects ((step-type :uint32)
                                      (value-id :uint32))
            (unless (colorlisp--session-predicate-step
                     session pattern position step-type value-id)
              (error 'highlight-error
                     :detail "Tree-sitter returned an invalid predicate step."))
            (let ((type  (cffi:mem-ref step-type :uint32))
                  (value (cffi:mem-ref value-id :uint32)))
              (case type
                (0 :done)
                (1 (list :capture value))
                (2 (list :string (colorlisp--query-string session value)))
                (otherwise
                 (error 'highlight-error
                        :detail (format nil
                                        "Unknown Tree-sitter predicate step ~D."
                                        type)))))))))


(defun colorlisp--split-predicates (steps)
  "Split query predicate STEPS at :DONE markers."
  (let ((predicates nil)
        (current nil))
    (dolist (step steps)
      (if (eq step :done)
          (when current
            (push (nreverse current) predicates)
            (setf current nil))
          (push step current)))
    (when current
      (push (nreverse current) predicates))
    (nreverse predicates)))


(defun colorlisp--capture-text (capture source byte-map)
  "Return CAPTURE's text from SOURCE using BYTE-MAP."
  (let ((start (aref byte-map (raw-capture-start-byte capture)))
        (end   (aref byte-map (raw-capture-end-byte capture))))
    (when (or (minusp start) (minusp end))
      (error 'highlight-error
             :detail "A Tree-sitter capture did not end on UTF-8 boundaries."))
    (subseq source start end)))


(defun colorlisp--capture-values (capture-id captures source byte-map)
  "Return texts captured under CAPTURE-ID in the current match."
  (loop for capture in captures
        when (= capture-id (raw-capture-capture-id capture))
          collect (colorlisp--capture-text capture source byte-map)))


(defun colorlisp--operand-values (operand captures source byte-map)
  "Resolve a query predicate OPERAND into one or more strings."
  (ecase (first operand)
    (:string
     (list (second operand)))
    (:capture
     (colorlisp--capture-values (second operand) captures source byte-map))))


(defun colorlisp--predicate-name (operand)
  "Normalize the leading operator OPERAND of a query predicate."
  (let ((name (second operand)))
    (string-downcase
     (if (and (plusp (length name)) (char= (char name 0) #\#))
         (subseq name 1)
         name))))


(defun colorlisp--predicate-match-p (values pattern any-p)
  "Test VALUES against PATTERN, using existential semantics when ANY-P."
  (funcall (if any-p #'some #'every)
           (lambda (value)
             (not (null (cl-ppcre:scan pattern value))))
           values))


(defun colorlisp--predicate-equal-p (left right any-p)
  "Compare LEFT and RIGHT strings, using existential semantics when ANY-P."
  (let ((comparisons
          (loop for left-value in left
                append (loop for right-value in right
                             collect (string= left-value right-value)))))
    (funcall (if any-p #'some #'every) #'identity comparisons)))


(defun colorlisp--predicate-satisfied-p (predicate captures source byte-map)
  "Return true when PREDICATE accepts the current match."
  (let* ((name      (colorlisp--predicate-name (first predicate)))
         (arguments (rest predicate))
         (values    (mapcar (lambda (operand)
                              (colorlisp--operand-values
                               operand captures source byte-map))
                            arguments)))
    (cond
      ((member name '("match?" "any-match?") :test #'string=)
       (and (= (length values) 2)
            (colorlisp--predicate-match-p
             (first values)
             (first (second values))
             (string= name "any-match?"))))
      ((member name '("eq?" "any-eq?") :test #'string=)
       (and (= (length values) 2)
            (colorlisp--predicate-equal-p
             (first values)
             (second values)
             (string= name "any-eq?"))))
      ((member name '("not-eq?" "any-not-eq?") :test #'string=)
       (and (= (length values) 2)
            (not (colorlisp--predicate-equal-p
                  (first values)
                  (second values)
                  (string= name "any-not-eq?")))))
      ((string= name "any-of?")
       (and (>= (length values) 2)
            (every (lambda (value)
                     (member value
                             (mapcan #'copy-list (rest values))
                             :test #'string=))
                   (first values))))
      ((string= name "not-any-of?")
       (and (>= (length values) 2)
            (every (lambda (value)
                     (not (member value
                                  (mapcan #'copy-list (rest values))
                                  :test #'string=)))
                   (first values))))
      ;; Locality properties require a locals query. Without one no node has
      ;; the local property, so an is-not local constraint is satisfied.
      ((string= name "is-not?")
       t)
      ((string= name "is?")
       nil)
      (t
       (error 'highlight-error
              :detail (format nil "Unsupported query predicate ~A." name))))))


(defun colorlisp--match-satisfied-p (session pattern captures source byte-map)
  "Return true when every predicate for PATTERN accepts CAPTURES."
  (every (lambda (predicate)
           (colorlisp--predicate-satisfied-p
            predicate captures source byte-map))
         (colorlisp--split-predicates
          (colorlisp--predicate-steps session pattern))))


(defun colorlisp--layer-captures (grammar query source source-octets byte-map)
  "Return accepted raw captures for one GRAMMAR and QUERY layer."
  (let* ((query-source (colorlisp--query-source query))
         (query-octets (babel:string-to-octets query-source :encoding :utf-8))
         (result nil))
    (with-colorlisp-session (session grammar source-octets query-octets)
      (cffi:with-foreign-objects ((pattern :uint32)
                                  (capture-count :uint32))
        (loop while (colorlisp--session-next-match
                     session pattern capture-count)
              for pattern-value = (cffi:mem-ref pattern :uint32)
              for captures = (colorlisp--current-match-captures
                              session pattern-value
                              (cffi:mem-ref capture-count :uint32))
              when (colorlisp--match-satisfied-p
                    session pattern-value captures source byte-map)
                do (setf result (nconc result captures)))))
    result))


(defun colorlisp--capture-category (capture-name)
  "Map a Tree-sitter CAPTURE-NAME to a stable semantic category."
  (let ((name (string-downcase capture-name)))
    (cond
      ((string= name "none") :plain)
      ((or (search "comment" name) (string= name "spell")) :comment)
      ((search "escape" name) :escape)
      ((or (search "string" name)
           (search "character" name)
           (string= name "text.literal"))
       :string)
      ((search "keyword" name) :keyword)
      ((search "operator" name) :operator)
      ((search "punctuation" name) :punctuation)
      ((or (search "number" name)
           (search "numeric" name)
           (search "float" name))
       :number)
      ((or (search "constant" name) (search "boolean" name)) :constant)
      ((or (search "type" name) (search "constructor" name)) :type)
      ((search "macro" name) :macro)
      ((search "method" name) :method)
      ((search "function" name) :function)
      ((search "parameter" name) :parameter)
      ((or (search "property" name)
           (search "field" name)
           (search "member" name))
       :property)
      ((search "variable.builtin" name) :builtin)
      ((search "variable" name) :variable)
      ((or (search "module" name) (search "namespace" name)) :namespace)
      ((search "label" name) :label)
      ((or (search "attribute" name) (search "tag" name)) :attribute)
      ((search "title" name) :heading)
      ((or (search "uri" name) (search "reference" name)) :link)
      ((search "embedded" name) :embedded)
      ((search "special" name) :special)
      (t :plain))))


(defun colorlisp--auxiliary-capture-p (capture)
  "Return true when CAPTURE only supports a query predicate."
  (let ((name (raw-capture-capture-name capture)))
    (and (plusp (length name))
         (char= #\_ (char name 0)))))


(defun colorlisp--capture->span (capture byte-map)
  "Convert a raw CAPTURE into a character-offset span using BYTE-MAP."
  (let ((start (aref byte-map (raw-capture-start-byte capture)))
        (end   (aref byte-map (raw-capture-end-byte capture))))
    (when (or (minusp start) (minusp end))
      (error 'highlight-error
             :detail "A Tree-sitter capture did not end on UTF-8 boundaries."))
    (span--create start end
                  (colorlisp--capture-category
                   (raw-capture-capture-name capture))
                  (raw-capture-capture-name capture)
                  (raw-capture-pattern capture))))


(defun colorlisp--span-less-specific-p (left right)
  "Order spans so more specific and later patterns overwrite LEFT first."
  (let ((left-length  (- (span-end left) (span-start left)))
        (right-length (- (span-end right) (span-start right))))
    (if (= left-length right-length)
        (< (span-pattern left) (span-pattern right))
        (> left-length right-length))))


(defun colorlisp--resolve-spans (source spans)
  "Resolve overlapping SPANS over SOURCE into non-overlapping semantic spans."
  (let ((categories (make-array (length source) :initial-element nil))
        (captures   (make-array (length source) :initial-element nil)))
    (dolist (span (sort (copy-list spans) #'colorlisp--span-less-specific-p))
      (loop for index from (span-start span) below (span-end span)
            do (setf (aref categories index) (span-category span)
                     (aref captures index) (span-capture span))))
    (let ((result nil)
          (start 0))
      (labels ((emit (end)
                 (let ((category (aref categories start)))
                   (when category
                     (push (span--create start end category
                                         (or (aref captures start) "") 0)
                           result)))))
        (loop for index from 1 below (length source)
              unless (and (eq (aref categories index)
                              (aref categories start))
                          (equal (aref captures index)
                                 (aref captures start)))
                do (emit index)
                   (setf start index))
        (when (plusp (length source))
          (emit (length source))))
      (nreverse result))))


(defun colorlisp--resolve-language (language pathname source)
  "Resolve an explicit LANGUAGE or detect one from PATHNAME and SOURCE."
  (cond
    (language
     (language-find language))
    (pathname
     (language-detect pathname :source source))
    (t
     nil)))


(defun highlight-spans (source &key language pathname)
  "Return non-overlapping semantic spans for SOURCE.

LANGUAGE may be a registered language or designator. When it is absent,
PATHNAME and SOURCE's shebang are used for detection. Unknown detected files
return no spans; an unsupported explicit language signals UNSUPPORTED-LANGUAGE.
Offsets are Common Lisp character offsets, not UTF-8 byte offsets."
  (check-type source string)
  (let ((resolved (colorlisp--resolve-language language pathname source)))
    (when (and resolved (plusp (length source)))
      (let* ((source-octets (babel:string-to-octets source :encoding :utf-8))
             (byte-map      (colorlisp--utf-8-byte-map source))
             (captures
               (loop for (grammar query) in (language--layers resolved)
                     append (colorlisp--layer-captures
                             grammar query source source-octets byte-map))))
        (colorlisp--resolve-spans
         source
         (loop for capture in captures
               unless (colorlisp--auxiliary-capture-p capture)
                 collect (colorlisp--capture->span capture byte-map)))))))


(defun highlight-segments (source &key language pathname)
  "Return complete, ordered semantic segments covering SOURCE."
  (check-type source string)
  (if (zerop (length source))
      nil
      (let ((spans (highlight-spans source
                                    :language language
                                    :pathname pathname))
            (segments nil)
            (position 0))
        (dolist (span spans)
          (when (< position (span-start span))
            (push (segment--create position (span-start span) :plain
                                   (subseq source position (span-start span)))
                  segments))
          (push (segment--create (span-start span) (span-end span)
                                 (span-category span)
                                 (subseq source
                                         (span-start span)
                                         (span-end span)))
                segments)
          (setf position (span-end span)))
        (when (< position (length source))
          (push (segment--create position (length source) :plain
                                 (subseq source position))
                segments))
        (nreverse segments))))
