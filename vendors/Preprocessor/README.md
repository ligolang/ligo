# A preprocessor a la C# in OCaml

The following preprocessing directives are supported
  * #define
  * #elif
  * #else
  * #endif
  * #endregion
  * #error
  * #if
  * #include
  * #region
  * #undef

Note: Because it is meant for LIGO, there is no error raised for
invalid preprocessing directives, as the symbol `#` is valid in
PascaLIGO (cons operator for lists). Also, the preprocessor may report an error on some weird but valid PascaLIGO contracts, like

const include : list (int) = list [1]
const l : list (int) = 0
# include
