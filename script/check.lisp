(require 'asdf)

(let* ((script (truename *load-truename*))
       (root   (uiop:pathname-parent-directory-pathname
                (uiop:pathname-directory-pathname script)))
       (quicklisp (merge-pathnames "quicklisp/setup.lisp"
                                   (user-homedir-pathname))))
  (when (probe-file quicklisp)
    (load quicklisp)
    (setf (symbol-value
           (find-symbol "*LOCAL-PROJECT-DIRECTORIES*" "QL"))
          nil))
  (pushnew root asdf:*central-registry* :test #'equal)
  (asdf:test-system "colorlisp"))
