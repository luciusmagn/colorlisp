#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "tree_sitter/api.h"

#if defined(__GNUC__) || defined(__clang__)
#define COLORLISP_PUBLIC __attribute__((visibility("default")))
#else
#define COLORLISP_PUBLIC
#endif

typedef struct {
  TSParser *parser;
  TSTree *tree;
  TSQuery *query;
  TSQueryCursor *cursor;
  TSQueryMatch match;
} ColorLispSession;

const TSLanguage *tree_sitter_rust(void);
const TSLanguage *tree_sitter_commonlisp(void);
const TSLanguage *tree_sitter_scheme(void);
const TSLanguage *tree_sitter_clojure(void);
const TSLanguage *tree_sitter_haskell(void);
const TSLanguage *tree_sitter_ocaml(void);
const TSLanguage *tree_sitter_ocaml_interface(void);
const TSLanguage *tree_sitter_c(void);
const TSLanguage *tree_sitter_python(void);
const TSLanguage *tree_sitter_go(void);
const TSLanguage *tree_sitter_bash(void);
const TSLanguage *tree_sitter_toml(void);
const TSLanguage *tree_sitter_cpp(void);
const TSLanguage *tree_sitter_javascript(void);
const TSLanguage *tree_sitter_typescript(void);
const TSLanguage *tree_sitter_tsx(void);
const TSLanguage *tree_sitter_json(void);
const TSLanguage *tree_sitter_yaml(void);
const TSLanguage *tree_sitter_markdown(void);
const TSLanguage *tree_sitter_markdown_inline(void);
const TSLanguage *tree_sitter_html(void);
const TSLanguage *tree_sitter_css(void);
const TSLanguage *tree_sitter_nix(void);
const TSLanguage *tree_sitter_java(void);
const TSLanguage *tree_sitter_ruby(void);
const TSLanguage *tree_sitter_lua(void);

static const TSLanguage *colorlisp_language_named(const char *name) {
  if (strcmp(name, "rust") == 0) return tree_sitter_rust();
  if (strcmp(name, "commonlisp") == 0) return tree_sitter_commonlisp();
  if (strcmp(name, "scheme") == 0) return tree_sitter_scheme();
  if (strcmp(name, "clojure") == 0) return tree_sitter_clojure();
  if (strcmp(name, "haskell") == 0) return tree_sitter_haskell();
  if (strcmp(name, "ocaml") == 0) return tree_sitter_ocaml();
  if (strcmp(name, "ocaml_interface") == 0) {
    return tree_sitter_ocaml_interface();
  }
  if (strcmp(name, "c") == 0) return tree_sitter_c();
  if (strcmp(name, "python") == 0) return tree_sitter_python();
  if (strcmp(name, "go") == 0) return tree_sitter_go();
  if (strcmp(name, "bash") == 0) return tree_sitter_bash();
  if (strcmp(name, "toml") == 0) return tree_sitter_toml();
  if (strcmp(name, "cpp") == 0) return tree_sitter_cpp();
  if (strcmp(name, "javascript") == 0) return tree_sitter_javascript();
  if (strcmp(name, "typescript") == 0) return tree_sitter_typescript();
  if (strcmp(name, "tsx") == 0) return tree_sitter_tsx();
  if (strcmp(name, "json") == 0) return tree_sitter_json();
  if (strcmp(name, "yaml") == 0) return tree_sitter_yaml();
  if (strcmp(name, "markdown") == 0) return tree_sitter_markdown();
  if (strcmp(name, "markdown_inline") == 0) return tree_sitter_markdown_inline();
  if (strcmp(name, "html") == 0) return tree_sitter_html();
  if (strcmp(name, "css") == 0) return tree_sitter_css();
  if (strcmp(name, "nix") == 0) return tree_sitter_nix();
  if (strcmp(name, "java") == 0) return tree_sitter_java();
  if (strcmp(name, "ruby") == 0) return tree_sitter_ruby();
  if (strcmp(name, "lua") == 0) return tree_sitter_lua();
  return NULL;
}

COLORLISP_PUBLIC ColorLispSession *colorlisp_session_new(
    const char *language_name,
    const char *source,
    uint32_t source_length,
    const char *query_source,
    uint32_t query_length,
    uint32_t *error_offset,
    uint32_t *error_type) {
  const TSLanguage *language = colorlisp_language_named(language_name);
  ColorLispSession *session = NULL;
  TSNode root;

  *error_offset = 0;
  *error_type = TSQueryErrorNone;
  if (language == NULL) {
    *error_type = UINT32_MAX;
    return NULL;
  }

  session = calloc(1, sizeof(ColorLispSession));
  if (session == NULL) {
    *error_type = UINT32_MAX - 1;
    return NULL;
  }

  session->parser = ts_parser_new();
  if (session->parser == NULL ||
      !ts_parser_set_language(session->parser, language)) {
    *error_type = UINT32_MAX - 2;
    goto failure;
  }

  session->tree = ts_parser_parse_string(
      session->parser, NULL, source, source_length);
  if (session->tree == NULL) {
    *error_type = UINT32_MAX - 3;
    goto failure;
  }

  session->query = ts_query_new(
      language, query_source, query_length, error_offset,
      (TSQueryError *)error_type);
  if (session->query == NULL) goto failure;

  session->cursor = ts_query_cursor_new();
  if (session->cursor == NULL) {
    *error_type = UINT32_MAX - 4;
    goto failure;
  }

  root = ts_tree_root_node(session->tree);
  ts_query_cursor_exec(session->cursor, session->query, root);
  return session;

failure:
  if (session->cursor != NULL) ts_query_cursor_delete(session->cursor);
  if (session->query != NULL) ts_query_delete(session->query);
  if (session->tree != NULL) ts_tree_delete(session->tree);
  if (session->parser != NULL) ts_parser_delete(session->parser);
  free(session);
  return NULL;
}

COLORLISP_PUBLIC void colorlisp_session_delete(ColorLispSession *session) {
  if (session == NULL) return;
  ts_query_cursor_delete(session->cursor);
  ts_query_delete(session->query);
  ts_tree_delete(session->tree);
  ts_parser_delete(session->parser);
  free(session);
}

COLORLISP_PUBLIC bool colorlisp_session_next_match(
    ColorLispSession *session,
    uint32_t *pattern_index,
    uint32_t *capture_count) {
  if (!ts_query_cursor_next_match(session->cursor, &session->match)) return false;
  *pattern_index = session->match.pattern_index;
  *capture_count = session->match.capture_count;
  return true;
}

COLORLISP_PUBLIC bool colorlisp_session_capture(
    const ColorLispSession *session,
    uint32_t capture_position,
    uint32_t *capture_id,
    uint32_t *start_byte,
    uint32_t *end_byte) {
  const TSQueryCapture *capture;
  if (capture_position >= session->match.capture_count) return false;
  capture = &session->match.captures[capture_position];
  *capture_id = capture->index;
  *start_byte = ts_node_start_byte(capture->node);
  *end_byte = ts_node_end_byte(capture->node);
  return true;
}

COLORLISP_PUBLIC const char *colorlisp_session_capture_name(
    const ColorLispSession *session,
    uint32_t capture_id,
    uint32_t *length) {
  return ts_query_capture_name_for_id(session->query, capture_id, length);
}

COLORLISP_PUBLIC uint32_t colorlisp_session_predicate_step_count(
    const ColorLispSession *session,
    uint32_t pattern_index) {
  uint32_t count = 0;
  ts_query_predicates_for_pattern(session->query, pattern_index, &count);
  return count;
}

COLORLISP_PUBLIC bool colorlisp_session_predicate_step(
    const ColorLispSession *session,
    uint32_t pattern_index,
    uint32_t position,
    uint32_t *step_type,
    uint32_t *value_id) {
  uint32_t count = 0;
  const TSQueryPredicateStep *steps = ts_query_predicates_for_pattern(
      session->query, pattern_index, &count);
  if (position >= count) return false;
  *step_type = steps[position].type;
  *value_id = steps[position].value_id;
  return true;
}

COLORLISP_PUBLIC const char *colorlisp_session_string_value(
    const ColorLispSession *session,
    uint32_t string_id,
    uint32_t *length) {
  return ts_query_string_value_for_id(session->query, string_id, length);
}
