#include <stdbool.h>
#include <stddef.h>
#include <wctype.h>

#include <tree_sitter/parser.h>

enum TokenType {
  OCAML_COMMENT,
  COMMENT,
  LINE_MARKER,
  JS_LIGO_ATTRIBUTE,
  AUTOMATIC_SEMICOLON
};

#define TAKE_WHILE_1(predicate) \
  if (!predicate(lexer->lookahead)) return false; \
  while (predicate(lexer->lookahead)) lexer->advance(lexer, false);

void *tree_sitter_JsLigo_external_scanner_create() { return NULL; }
void tree_sitter_JsLigo_external_scanner_destroy(void *p) {}
void tree_sitter_JsLigo_external_scanner_reset(void *p) {}
unsigned tree_sitter_JsLigo_external_scanner_serialize(void *p, char *buffer) { return 0; }
void tree_sitter_JsLigo_external_scanner_deserialize(void *p, const char *b, unsigned n) {}

static void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

// Reference: https://github.com/tree-sitter/tree-sitter-typescript/blob/master/common/scanner.h#L72-L160
static bool scan_automatic_semicolon(TSLexer *lexer, const bool *valid_symbols){
  lexer->result_symbol = AUTOMATIC_SEMICOLON;
  lexer->mark_end(lexer);

  for (;;) {
    if (lexer->lookahead == 0) return true;
    if (lexer->lookahead == '}') {
      // Automatic semicolon insertion breaks detection of object patterns
      // in a typed context:
      //   type F = ({a}: {a: number}) => number;
      // Therefore, disable automatic semicolons when followed by typing
      do {
        skip(lexer);
      } while (iswspace(lexer->lookahead));
      if (lexer->lookahead == ':') return false;
      return true;
    }
    if (!iswspace(lexer->lookahead)) return false;
    if (lexer->lookahead == '\n') break;
    skip(lexer);
  }

  skip(lexer);

  switch (lexer->lookahead) {
  case ',':
  case '.':
  case ';':
  case '*':
  case '%':
  case '>':
  case '<':
  case '=':
  case '?':
  case '^':
  case '|':
  case '&':
  case '/':
  case ':':
    return false;

    // Insert a semicolon before `--` and `++`, but not before binary `+` or `-`.
  case '+':
    skip(lexer);
    return lexer->lookahead == '+';
  case '-':
    skip(lexer);
    return lexer->lookahead == '-';

    // Don't insert a semicolon before `!=`, but do insert one before a unary `!`.
  case '!':
    skip(lexer);
    return lexer->lookahead != '=';

  }

  return true;
}

bool tree_sitter_JsLigo_external_scanner_scan(
    void *payload,
    TSLexer *lexer,
    const bool *valid_symbols) {
  while (iswspace(lexer->lookahead)) lexer->advance(lexer, true);

  if (lexer->lookahead == '/') {
    bool is_attribute = false;

    lexer->advance(lexer, false);

    if (lexer->lookahead == '/') {
      lexer->advance(lexer, false);

      // skip white spaces
      while (iswspace(lexer->lookahead)) lexer->advance(lexer, false);

      // If single-line comment is of the form `// @<att-name>` it is an attribute
      if (lexer->lookahead == '@') {
        is_attribute = true;
      }

      for (;;) {
        switch (lexer->lookahead) {
        case '\n':
        case '\0':
          lexer->result_symbol = is_attribute ? JS_LIGO_ATTRIBUTE : COMMENT;
          return true;
        default:
          lexer->advance(lexer, false);
          break;
        }
      }
    } else if (lexer->lookahead == '*') {
      lexer->advance(lexer, false);

      // skip white spaces
      while (iswspace(lexer->lookahead)) lexer->advance(lexer, false);

      // If multi-line comment is of the form `/* @<att-name> */` it is an attribute
      if (lexer->lookahead == '@') {
        is_attribute = true;
      }

      bool after_star = false;
      unsigned nesting_depth = 1;
      for (;;) {
        switch (lexer->lookahead) {
        case '\0':
          return false;
        case '*':
          lexer->advance(lexer, false);
          after_star = true;
          break;
        case '/':
          if (after_star) {
            lexer->advance(lexer, false);
            after_star = false;
            nesting_depth--;
            if (nesting_depth == 0) {
              lexer->result_symbol = is_attribute ? JS_LIGO_ATTRIBUTE : OCAML_COMMENT;
              return true;
            }
          } else {
            lexer->advance(lexer, false);
            after_star = false;
            if (lexer->lookahead == '*') {
              nesting_depth++;
              lexer->advance(lexer, false);
            }
          }
          break;
        default:
          lexer->advance(lexer, false);
          after_star = false;
          break;
        }
      }
    }
  } else if (lexer->lookahead == '#' && !lexer->get_column(lexer)) {
    // Lex line markers so that our comment lexer is happy.
    lexer->advance(lexer, false);
    TAKE_WHILE_1(iswspace);
    TAKE_WHILE_1(iswdigit); // linenum
    TAKE_WHILE_1(iswspace);
    if (lexer->lookahead != '"') return false; // filename
    lexer->advance(lexer, false);
    while (lexer->lookahead != '"') lexer->advance(lexer, false);
    if (lexer->lookahead != '"') return false;
    lexer->advance(lexer, false);
    if (lexer->lookahead != '\n') {
      TAKE_WHILE_1(iswspace);
      TAKE_WHILE_1(iswdigit); // flag
    }
    if (lexer->lookahead != '\n') return false;
    lexer->advance(lexer, false);
    lexer->result_symbol = LINE_MARKER;
    return true;
  } else if (valid_symbols[AUTOMATIC_SEMICOLON]) {
    return scan_automatic_semicolon(lexer, valid_symbols);
  }

  return false;
}