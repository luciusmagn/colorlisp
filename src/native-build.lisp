(in-package #:colorlisp)


(defparameter *native-cache-version*
  "0.1.0-ts-0.26.11-20260720"
  "Version key for native sources and vendored grammars.")

(defvar *native-library* nil
  "CFFI handle for the loaded ColorLisp native library.")


(defun colorlisp--source-root ()
  "Return the root of the loaded ColorLisp ASDF system."
  (asdf:system-source-directory "colorlisp"))


(defun colorlisp--cache-root ()
  "Return ColorLisp's XDG-compatible cache directory."
  (let ((xdg-cache-home (uiop:getenv "XDG_CACHE_HOME")))
    (merge-pathnames
     (format nil "colorlisp/native/~A/" *native-cache-version*)
     (if (and xdg-cache-home (plusp (length xdg-cache-home)))
         (uiop:ensure-directory-pathname xdg-cache-home)
         (merge-pathnames ".cache/" (user-homedir-pathname))))))


(defun colorlisp--bundled-library-path ()
  "Return the optional prebuilt native library pathname."
  (merge-pathnames "native/libcolorlisp-tree-sitter.so"
                   (colorlisp--source-root)))


(defun colorlisp--cached-library-path ()
  "Return the native library pathname in the user cache."
  (merge-pathnames "libcolorlisp-tree-sitter.so"
                   (colorlisp--cache-root)))


(defun colorlisp--native-source-pathnames ()
  "Return every C translation unit needed by the native library."
  (let ((root (colorlisp--source-root)))
    (mapcar
     (lambda (relative)
       (namestring (merge-pathnames relative root)))
     '("native/colorlisp-tree-sitter.c"
       "vendor/tree-sitter/src/lib.c"
       "vendor/grammars/rust/parser.c"
       "vendor/grammars/rust/scanner.c"
       "vendor/grammars/common-lisp/parser.c"
       "vendor/grammars/scheme/parser.c"
       "vendor/grammars/c/parser.c"
       "vendor/grammars/python/parser.c"
       "vendor/grammars/python/scanner.c"
       "vendor/grammars/go/parser.c"
       "vendor/grammars/shell/parser.c"
       "vendor/grammars/shell/scanner.c"
       "vendor/grammars/toml/parser.c"
       "vendor/grammars/toml/scanner.c"
       "vendor/grammars/cpp/parser.c"
       "vendor/grammars/cpp/scanner.c"
       "vendor/grammars/javascript/parser.c"
       "vendor/grammars/javascript/scanner.c"
       "vendor/grammars/typescript/parser.c"
       "vendor/grammars/typescript/scanner.c"
       "vendor/grammars/tsx/parser.c"
       "vendor/grammars/tsx/scanner.c"
       "vendor/grammars/json/parser.c"
       "vendor/grammars/yaml/parser.c"
       "vendor/grammars/yaml/scanner.c"
       "vendor/grammars/markdown/parser.c"
       "vendor/grammars/markdown/scanner.c"
       "vendor/grammars/markdown-inline/parser.c"
       "vendor/grammars/markdown-inline/scanner.c"
       "vendor/grammars/html/parser.c"
       "vendor/grammars/html/scanner.c"
       "vendor/grammars/css/parser.c"
       "vendor/grammars/css/scanner.c"
       "vendor/grammars/nix/parser.c"
       "vendor/grammars/nix/scanner.c"
       "vendor/grammars/java/parser.c"
       "vendor/grammars/ruby/parser.c"
       "vendor/grammars/ruby/scanner.c"
       "vendor/grammars/lua/parser.c"
       "vendor/grammars/lua/scanner.c"))))


(defun colorlisp--native-include-arguments ()
  "Return compiler include arguments for runtime and grammar headers."
  (let ((root (colorlisp--source-root)))
    (mapcar
     (lambda (relative)
       (format nil "-I~A" (namestring (merge-pathnames relative root))))
     '("vendor/tree-sitter/include/"
       "vendor/grammars/rust/"
       "vendor/grammars/common-lisp/"
       "vendor/grammars/scheme/"
       "vendor/grammars/c/"
       "vendor/grammars/python/"
       "vendor/grammars/go/"
       "vendor/grammars/shell/"
       "vendor/grammars/toml/"
       "vendor/grammars/cpp/"
       "vendor/grammars/javascript/"
       "vendor/grammars/typescript/"
       "vendor/grammars/tsx/"
       "vendor/grammars/json/"
       "vendor/grammars/yaml/"
       "vendor/grammars/markdown/"
       "vendor/grammars/markdown-inline/"
       "vendor/grammars/html/"
       "vendor/grammars/css/"
       "vendor/grammars/nix/"
       "vendor/grammars/java/"
       "vendor/grammars/ruby/"
       "vendor/grammars/lua/"))))


(defun colorlisp--build-native-library (target)
  "Build the native library and atomically publish it at TARGET."
  (ensure-directories-exist target)
  (let* ((temporary
           (merge-pathnames
            (format nil ".libcolorlisp-tree-sitter-~D-~D.so"
                    (get-universal-time)
                    (random most-positive-fixnum))
            (uiop:pathname-directory-pathname target)))
         (command
           (append (list (or (uiop:getenv "CC") "cc")
                         "-shared" "-fPIC" "-O2" "-std=gnu11"
                         "-fvisibility=hidden"
                         "-o" (namestring temporary))
                   (colorlisp--native-include-arguments)
                   (colorlisp--native-source-pathnames))))
    (handler-case
        (progn
          (uiop:run-program command
                            :output *standard-output*
                            :error-output *error-output*)
          (uiop:rename-file-overwriting-target temporary target))
      (error (condition)
        (when (probe-file temporary)
          (delete-file temporary))
        (error 'native-build-error :command command :cause condition)))))


(defun colorlisp--native-library-path ()
  "Return an existing native library, building one when necessary."
  (let ((override (uiop:getenv "COLORLISP_NATIVE_LIBRARY"))
        (bundled  (colorlisp--bundled-library-path))
        (cached   (colorlisp--cached-library-path)))
    (cond
      ((and override (plusp (length override)))
       (or (probe-file override)
           (error 'highlight-error
                  :detail (format nil
                                  "COLORLISP_NATIVE_LIBRARY does not exist: ~A"
                                  override))))
      ((probe-file bundled)
       bundled)
      ((probe-file cached)
       cached)
      (t
       (colorlisp--build-native-library cached)
       cached))))


(defun native-ensure-loaded ()
  "Build when needed and load ColorLisp's native Tree-sitter library."
  (unless *native-library*
    (setf *native-library*
          (cffi:load-foreign-library (colorlisp--native-library-path))))
  *native-library*)
