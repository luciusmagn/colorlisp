; Comments

[
  (comment)
  (block_comment)
] @comment

; Literals

(str_lit) @string
(format_specifier) @string.escape
(char_lit) @string.special
(path_lit) @string.special
(num_lit) @number
(complex_num_lit) @number
(nil_lit) @constant.builtin
(kwd_lit) @constant
(fancy_literal) @variable
(package_lit) @namespace
(sym_lit) @variable

; Reader syntax

[
  (quoting_lit)
  (var_quoting_lit)
  (syn_quoting_lit)
  (unquoting_lit)
  (unquote_splicing_lit)
  (read_cond_lit)
  (splicing_read_cond_lit)
  (include_reader_macro)
  (self_referential_reader_macro)
] @punctuation.special

; Definitions

(defun_header
  (defun_keyword) @keyword)

(defun_header
  function_name: [(sym_lit) (package_lit)] @function)

; Calls and special operators

(list_lit
  . [(sym_lit) (package_lit)] @function)

((list_lit
   . [(sym_lit) (package_lit)] @keyword)
 (#match? @keyword "(?i)^(cl:|common-lisp:)?(block|catch|compiler-let|declare|eval-when|flet|function|go|if|labels|let|let\\*|load-time-value|locally|macrolet|multiple-value-call|multiple-value-prog1|progn|progv|quote|return-from|setq|symbol-macrolet|tagbody|the|throw|unwind-protect)$"))

; Loop grammar

[
  (loop_macro)
  (loop_clause)
  (for_clause_word)
  (accumulation_verb)
  (condition_clause)
  (termination_clause)
  (while_clause)
  (repeat_clause)
  (with_clause)
] @keyword
