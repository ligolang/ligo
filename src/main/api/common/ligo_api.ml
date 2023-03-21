open Api_helpers

let dump_changelog display_format no_colour () =
  let value = Changelog.changelog in
  let format = Ligo_formatter.changelog_format in
  format_result ~display_format ~no_colour format (fun ~raise:_ -> value)


module Compile = Compile
module Transpile = Transpile
module Transpile_with_ast = Transpile_with_ast
module Run = Run
module Info = Info
module Print = Print
module Mutate = Mutate
