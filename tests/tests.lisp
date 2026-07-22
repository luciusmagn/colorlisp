(defpackage #:colorlisp/tests
  (:use #:cl)
  (:export #:run-tests))

(in-package #:colorlisp/tests)


(defvar *failures* nil)


(defmacro check (form &optional description)
  `(unless ,form
     (push (or ,description ',form) *failures*)))


(defun segments--categories (source language)
  "Return all non-plain categories produced for SOURCE in LANGUAGE."
  (remove :plain
          (mapcar #'colorlisp:segment-category
                  (colorlisp:highlight-segments source :language language))))


(defun test-language-registry ()
  "Test explicit aliases and pathname detection."
  (dolist (case '(("file.rs" :rust)
                  ("system.asd" :common-lisp)
                  ("file.scm" :scheme)
                  ("file.c" :c)
                  ("file.hpp" :cpp)
                  ("file.py" :python)
                  ("file.go" :go)
                  ("file.sh" :shell)
                  ("file.toml" :toml)
                  ("file.js" :javascript)
                  ("file.ts" :typescript)
                  ("file.tsx" :tsx)
                  ("file.json" :json)
                  ("file.yml" :yaml)
                  ("file.md" :markdown)
                  ("file.html" :html)
                  ("file.css" :css)
                  ("file.nix" :nix)
                  ("file.java" :java)
                  ("file.rb" :ruby)
                  ("file.lua" :lua)))
    (check (eq (second case)
               (colorlisp:language-name
                (colorlisp:language-detect (first case))))
           (format nil "detect ~A" (first case))))
  (check (eq :shell
             (colorlisp:language-name
              (colorlisp:language-detect
               "script" :source "#!/usr/bin/env bash
echo yes
")))
         "detect shell shebang")
  (check (null (colorlisp:language-detect "README"))
         "unknown pathname returns nil"))


(defun test-native-library ()
  "Test that packagers can materialize and locate the native library."
  (check (probe-file (colorlisp:native-library-path))
         "native library pathname exists"))


(defun test-native-include-arguments ()
  "Test that native builds use the vendored Tree-sitter runtime headers."
  (let ((expected
          (format nil "-I~A"
                  (namestring
                   (merge-pathnames
                    "vendor/tree-sitter/src/"
                    (asdf:system-source-directory "colorlisp"))))))
    (check (member expected
                   (colorlisp::colorlisp--native-include-arguments)
                   :test #'string=)
           "native build includes vendored Tree-sitter source headers")))


(defun test-supported-grammars ()
  "Smoke-test every bundled grammar and query."
  (dolist
      (case
       '((:common-lisp "(defun hello (name) (format t \"Hello, ~A\" name))")
         (:scheme "(define (square x) (* x x))")
         (:rust "fn main() { let answer: i32 = 42; }")
         (:c "int main(void) { return 0; }")
         (:cpp "class Example { public: int value() const { return 1; } };")
         (:python "def greet(name):
    return f\"Hello {name}\"
")
         (:go "package main
func main() { println(42) }
")
         (:shell "#!/bin/sh
for item in one two; do echo \"$item\"; done
")
         (:toml "[package]
name = \"colorlisp\"
version = \"0.1.0\"
")
         (:javascript "function greet(name) { return `Hello ${name}`; }")
         (:typescript "interface User { name: string }
const user: User = {name: 'A'};")
         (:tsx "const node = <strong>Hello</strong>;")
         (:json "{\"name\": \"ColorLisp\", \"enabled\": true}")
         (:yaml "name: ColorLisp
enabled: true
")
         (:markdown "# ColorLisp

Use **semantic** colors.
")
         (:html "<main class=\"content\">Hello</main>")
         (:css ".content { color: green; }")
         (:nix "{ pkgs ? import <nixpkgs> {} }: pkgs.hello")
         (:java "class Main { public static void main(String[] args) {} }")
         (:ruby "def greet(name)
  puts \"Hello #{name}\"
end
")
         (:lua "local function greet(name) return \"Hello \" .. name end")))
    (handler-case
        (check (segments--categories (second case) (first case))
               (format nil "highlight ~A" (first case)))
      (error (condition)
        (push (format nil "highlight ~A signaled ~A" (first case) condition)
              *failures*)))))


(defun test-semantic-output ()
  "Test semantic categories, complete coverage, and Unicode offsets."
  (let* ((source "(defun λ-name (value) ; note
  (format t \"λ=~A\" value))")
         (segments (colorlisp:highlight-segments
                    source :language :common-lisp)))
    (check (string= source
                    (apply #'concatenate 'string
                           (mapcar #'colorlisp:segment-text segments)))
           "segments cover the original Unicode source")
    (check (member :keyword (mapcar #'colorlisp:segment-category segments))
           "Common Lisp keyword category")
    (check (member :function (mapcar #'colorlisp:segment-category segments))
           "Common Lisp function category")
    (check (member :comment (mapcar #'colorlisp:segment-category segments))
           "Common Lisp comment category")
    (check (member :string (mapcar #'colorlisp:segment-category segments))
           "Common Lisp string category"))
  (let ((segments (colorlisp:highlight-segments
                   "unrecognized" :pathname "README.unknown")))
    (check (= 1 (length segments)) "unknown source has one plain segment")
    (check (eq :plain (colorlisp:segment-category (first segments)))
           "unknown source is plain")))


(defun test-capture-normalization ()
  "Test normalization of grammar-specific capture names."
  (let ((categories
          (mapcar #'colorlisp:segment-category
                  (colorlisp:highlight-segments
                   "fn main() { 42 }" :language :rust))))
    (check (member :number categories)
           "numeric constants normalize to the number category")))


(defun run-tests ()
  "Run the complete ColorLisp test suite and return true on success."
  (setf *failures* nil)
  (test-native-library)
  (test-native-include-arguments)
  (test-language-registry)
  (test-supported-grammars)
  (test-semantic-output)
  (test-capture-normalization)
  (if *failures*
      (progn
        (format *error-output* "~&ColorLisp failures:~%")
        (dolist (failure (nreverse *failures*))
          (format *error-output* "  ~A~%" failure))
        nil)
      (progn
        (format t "~&ColorLisp tests passed.~%")
        t)))
