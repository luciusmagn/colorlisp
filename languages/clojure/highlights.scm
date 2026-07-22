;; Literals

(num_lit) @number

[
  (char_lit)
  (str_lit)
] @string

[
 (bool_lit)
 (nil_lit)
] @constant.builtin

(kwd_lit) @constant

(regex_lit) @string.special

;; Symbols are variables by default. A symbol in function position is refined
;; below, followed by the language's special forms.

(sym_lit) @variable

(list_lit
  .
  (sym_lit) @function)

((list_lit
   .
   (sym_lit) @_definition
   .
   (sym_lit) @function)
 (#any-of? @_definition
   "def" "defn" "defn-" "defmacro" "defmulti" "defmethod" "defonce"))

((list_lit
   .
   (sym_lit) @_namespace-form
   .
   (sym_lit) @namespace)
 (#any-of? @_namespace-form "ns" "in-ns"))

((list_lit
   .
   (sym_lit) @keyword)
 (#any-of? @keyword
   "def" "defn" "defn-" "defmacro" "defmulti" "defmethod" "defonce"
   "deftype" "defrecord" "defprotocol" "ns" "in-ns" "fn" "fn*" "let"
   "letfn" "loop" "recur" "if" "if-let" "if-some" "when" "when-let"
   "when-some" "cond" "condp" "case" "do" "quote" "var" "set!" "new"
   "throw" "try" "catch" "finally" "monitor-enter" "monitor-exit"))

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

;; Comments

(comment) @comment

(dis_expr
  marker: "#_" @comment)

;; Treat quasiquotation as operators for the purpose of highlighting.

[
 "'"
 "`"
 "~"
 "@"
 "~@"
] @operator
