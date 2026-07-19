(defpackage #:colorlisp
  (:use #:cl)
  (:export
   #:colorlisp-error
   #:highlight-error
   #:language
   #:language-aliases
   #:language-detect
   #:language-extensions
   #:language-find
   #:language-name
   #:language-names
   #:native-build-error
   #:query-error
   #:query-error-language
   #:query-error-offset
   #:query-error-type
   #:segment
   #:segment-category
   #:segment-end
   #:segment-start
   #:segment-text
   #:highlight-segments
   #:highlight-spans
   #:span
   #:span-capture
   #:span-category
   #:span-end
   #:span-start
   #:unsupported-language
   #:unsupported-language-designator))

(in-package #:colorlisp)
