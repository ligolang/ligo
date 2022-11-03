# SPDX-FileCopyrightText: 2022 Oxhead Alpha
# SPDX-License-Identifier: LicenseRef-MIT-OA

require 'shellwords'

# Lints Haskell files inside the project using HLint
def lint(files, options = {})
  final_options = options.merge(json: true)

  issues = files
    .map { |file| Shellwords.escape(file) }
    .map do |file|
      out = `hlint lint #{file} #{to_hlint_options(final_options)}`
      if $?.success? then
        out
      else
        fail 'Running Hlint failed, please check the logs'
        # Command execution has printed the stderr automatically,
        # let's separate it visually
        print "\n"
        exit 1
      end
    end
    .reject { |s| s == '' }
    .map { |lint_result| JSON.parse(lint_result).flatten }
    .flatten

  send_inline_comment(issues)

end

def send_inline_comment(results)
  dir = "#{Dir.pwd}/"
  results.each do |r|
    filename = r['file'].gsub(dir, '')

    prompt = r['severity'] == 'Suggestion' || r['severity'] == 'Warning' ? 'Why Not' : ''
    prompt = r['severity'] == 'Error' ? 'Error description' : prompt

    message = <<~HEREDOC

      :warning: Found "#{r['hint']}"

      ```haskell
        #{r['from']}
      ```

      #{prompt}

      ```haskell
        #{r['to']}
      ```
    HEREDOC

    markdown(message, file: filename, line: r['startLine'])
  end
end

def to_hlint_options(options = {})
  options.
  # filter not null
  reject { |_key, value| value.nil? }.
  # map booleans arguments equal true
  map { |key, value| value.is_a?(TrueClass) ? [key, ''] : [key, value] }.
  # map booleans arguments equal false
  map { |key, value| value.is_a?(FalseClass) ? ["no-#{key}", ''] : [key, value] }.
  # replace underscore by hyphen
  map { |key, value| [key.to_s.tr('_', '-'), value] }.
  # prepend '--' into the argument
  map { |key, value| ["--#{key}", value] }.
  # reduce everything into a single string
  reduce('') { |args, option| "#{args} #{option[0]} #{option[1]}" }.
  # strip leading spaces
  strip
end

# get all affected files by the changes in the current diff
affected_files = git.added_files + git.modified_files

# limit files to .hs files
haskell_files = affected_files.select { |file| file.end_with?('.hs') }

# avoid overwriting and keep the values of duplicate 'hint' keys separate
hlint_options =
    {:hint => 'tools/debugger/.hlint.yaml'}
    .merge('hint' => 'tools/debugger/.hlint-universum.yaml')
    .merge('ignore' => "'Parse error'")

# run hlint on the files and comment inline in the PR
lint haskell_files, hlint_options
