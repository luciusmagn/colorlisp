(in-package #:colorlisp)


(defclass language ()
  ((name
    :initarg :name
    :reader language-name
    :type keyword
    :documentation "The canonical keyword name of the language.")
   (aliases
    :initarg :aliases
    :reader language-aliases
    :type list
    :documentation "Lowercase names accepted as explicit language designators.")
   (extensions
    :initarg :extensions
    :reader language-extensions
    :type list
    :documentation "Lowercase filename extensions recognized for the language.")
   (filenames
    :initarg :filenames
    :reader language--filenames
    :type list
    :documentation "Lowercase complete filenames recognized for the language.")
   (layers
    :initarg :layers
    :reader language--layers
    :type list
    :documentation "Grammar and query layers used to highlight the language."))
  (:documentation "A registered source language and its Tree-sitter layers."))


(defun language--create (name grammar &key aliases extensions filenames layers)
  "Create a language registry entry for NAME and GRAMMAR."
  (make-instance 'language
                 :name name
                 :aliases (mapcar #'string-downcase
                                  (cons (symbol-name name) aliases))
                 :extensions (mapcar #'string-downcase extensions)
                 :filenames (mapcar #'string-downcase filenames)
                 :layers (or layers
                             (list (list grammar
                                         (format nil "languages/~A/highlights.scm"
                                                 (string-downcase
                                                  (symbol-name name))))))))


(defparameter *languages*
  (list
   (language--create :common-lisp "commonlisp"
                     :aliases '("commonlisp" "cl" "lisp")
                     :extensions '("lisp" "lsp" "cl" "asd"))
   (language--create :scheme "scheme"
                     :aliases '("scm")
                     :extensions '("scm" "ss" "sld"))
   (language--create :rust "rust"
                     :aliases '("rs")
                     :extensions '("rs"))
   (language--create :c "c"
                     :extensions '("c" "h"))
   (language--create :cpp "cpp"
                     :aliases '("c++" "cxx")
                     :extensions '("cc" "cpp" "cxx" "hh" "hpp" "hxx"))
   (language--create :python "python"
                     :aliases '("py")
                     :extensions '("py" "pyw"))
   (language--create :go "go"
                     :extensions '("go"))
   (language--create :shell "bash"
                     :aliases '("bash" "sh" "zsh" "ksh")
                     :extensions '("sh" "bash" "zsh" "ksh"))
   (language--create :toml "toml"
                     :extensions '("toml")
                     :filenames '("cargo.lock"))
   (language--create :javascript "javascript"
                     :aliases '("js" "node")
                     :extensions '("js" "mjs" "cjs"))
   (language--create :typescript "typescript"
                     :aliases '("ts")
                     :extensions '("ts" "mts" "cts"))
   (language--create :tsx "tsx"
                     :aliases '("jsx")
                     :extensions '("tsx" "jsx"))
   (language--create :json "json"
                     :extensions '("json" "jsonl")
                     :filenames '("flake.lock"))
   (language--create :yaml "yaml"
                     :aliases '("yml")
                     :extensions '("yaml" "yml"))
   (language--create :markdown "markdown"
                     :aliases '("md")
                     :extensions '("md" "markdown")
                     :layers '(("markdown" "languages/markdown/highlights.scm")
                               ("markdown_inline"
                                "languages/markdown-inline/highlights.scm")))
   (language--create :html "html"
                     :aliases '("htm")
                     :extensions '("html" "htm"))
   (language--create :css "css"
                     :extensions '("css"))
   (language--create :nix "nix"
                     :extensions '("nix"))
   (language--create :java "java"
                     :extensions '("java"))
   (language--create :ruby "ruby"
                     :aliases '("rb")
                     :extensions '("rb" "rake" "gemspec")
                     :filenames '("gemfile" "rakefile"))
   (language--create :lua "lua"
                     :extensions '("lua")))
  "Languages bundled with ColorLisp.")


(defun language--normalized-designator (designator)
  "Return a lowercase string for a language DESIGNATOR."
  (string-downcase
   (etypecase designator
     (string designator)
     (symbol (symbol-name designator)))))


(defun language-find (designator &key (errorp t))
  "Resolve DESIGNATOR to a language.

When ERRORP is false, return NIL for an unsupported designator."
  (if (typep designator 'language)
      designator
      (let* ((normalized (language--normalized-designator designator))
             (language
               (find-if (lambda (candidate)
                          (member normalized
                                  (language-aliases candidate)
                                  :test #'string=))
                        *languages*)))
        (cond
          (language
           language)
          (errorp
           (error 'unsupported-language :designator designator))
          (t
           nil)))))


(defun language-names ()
  "Return the canonical names of all bundled languages."
  (mapcar #'language-name *languages*))


(defun language--pathname-filename (pathname)
  "Return PATHNAME's final component in lowercase."
  (let ((name (pathname-name pathname))
        (type (pathname-type pathname)))
    (string-downcase
     (cond
       ((and name type)
        (format nil "~A.~A" name type))
       (name
        (string name))
       (t
        "")))))


(defun language--pathname-extension (pathname)
  "Return PATHNAME's lowercase extension, or NIL."
  (let ((type (pathname-type pathname)))
    (and type (string-downcase (string type)))))


(defun language--first-line (source)
  "Return the first line of SOURCE."
  (subseq source 0 (or (position #\Newline source) (length source))))


(defun language--from-shebang (source)
  "Detect a supported script language from SOURCE's shebang."
  (when (and source
             (>= (length source) 2)
             (string= "#!" source :end2 2))
    (let ((line (string-downcase (language--first-line source))))
      (cond
        ((or (search "bash" line) (search "/sh" line)
             (search " zsh" line) (search " ksh" line))
         (language-find :shell))
        ((search "python" line)
         (language-find :python))
        ((search "ruby" line)
         (language-find :ruby))
        ((search "lua" line)
         (language-find :lua))
        ((or (search "sbcl" line) (search "ccl" line)
             (search "ecl" line))
         (language-find :common-lisp))
        ((or (search "scheme" line) (search "guile" line)
             (search "racket" line))
         (language-find :scheme))
        (t
         nil)))))


(defun language-detect (pathname &key source)
  "Detect a supported language from PATHNAME and optional SOURCE.

Return NIL when the pathname and shebang are not recognized."
  (let* ((pathname  (pathname pathname))
         (filename  (language--pathname-filename pathname))
         (extension (language--pathname-extension pathname)))
    (or (find-if (lambda (language)
                   (member filename
                           (language--filenames language)
                           :test #'string=))
                 *languages*)
        (and extension
             (find-if (lambda (language)
                        (member extension
                                (language-extensions language)
                                :test #'string=))
                      *languages*))
        (language--from-shebang source))))
