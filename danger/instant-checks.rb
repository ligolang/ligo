# SPDX-FileCopyrightText: 2021 Oxhead Alpha
# SPDX-License-Identifier: LicenseRef-MIT-OA

# Checks that, when hit, should be fixed as soon as possible.

require_relative 'helpers'
require_relative 'trailing-whitespaces'

if mr_merging_branches?
  message(
    "This merge request looks like plain merge of one branch into another.\n"\
    "Checks won't be performed."
  )
else

# TODO: uncomment this when bugs are fixed.
# check_trailing_whitespaces()

# Clean commits history
if git.commits.any? { |c| c.subject =~ /^Merge branch/ }
  # This is a warning because `add-changelog-entry` job
  # produces merge commit.
  warn('Please, no merge commits. Rebase for the win.')
end

# Proper commit style
# Note: we do not use commit_lint plugin because it triggers on fixup commits
git.commits.each { |commit|
  # If any of these substrings is included into commit message,
  # we are fine with issue tag absence.
  exclusions = [
    # In lower-case
    "changelog"
  ]

  if commit.fixup? || commit.wip? || exclusions.any? { |exc| commit.subject.downcase.include?(exc) }
    next
  end

  subject = commit.subject
  subject_payload = subject.sub(issue_tags_pattern, "")
  subject_ticked = commit.subject_ticked

  unless has_valid_issue_tags(subject)
    warn("In #{commit.sha} message lacks issue id: #{subject_ticked}.")
  end

  if subject_payload.start_with?(" ")
    warn("Extra space in commit #{commit.sha} subject after the issue tags: #{subject_ticked}.")
  elsif !subject_payload.start_with?(/[A-Z]/)
    warn("In #{commit.sha} subject does not begin from uppercase letter: #{subject_ticked}.")
  end

  if subject[-1..-1] == '.'
    warn("In #{commit.sha} message ends with a dot: #{subject_ticked} :fire_engine:")
  end

  if subject.length > 90
    fail("Nooo, such long commit message names do not work (`#{commit.sha}`).")
  elsif subject.length > 72
    warn("In commit #{commit.sha} message is too long (#{subject.length} chars), keep it within 72 characters length.")
  end

  if commit.message_body.empty?
    unless commit.chore? || exclusions.any? { |exc| subject.downcase.include?(exc) }
      fail("Commit #{commit.sha} lacks description :unamused:")
    end
  else
    # Checks on description

    if !commit.blank_line_after_subject?
      warn("In #{commit.sha} blank line is missing after the commit's subject.")
    end

    if !commit.chore?
      description_patterns = [
        /^Problem:[ \n].*^Solution:[ \n]/m,
        /^Motivation:[ \n].*/m,
        /And yes, I don't care about templates/
      ]
      unless description_patterns.any? { |pattern| pattern.match?(commit.description) }
        warn(
          "Description of #{commit.sha} does not follow the template.\n"\
          "Try `Problem:`/`Solution:` structure."
        )
      end
    end
  end

}

# Proper MR content
mr_title_payload = githost.mr_title_payload

unless has_valid_issue_tags(mr_title_payload)
  warn(
    "Inappropriate title for PR.\n"\
    "Should start from issue ID (e.g. `[#123]`) or `[Chore]` tag.\n"\
    "Note: please use `[Chore]` also for tickets tracked internally on YouTrack."
  )
end

## Supplying a link to an YT ticket
all_YT_tickets = githost.mr_title.scan(/\b[A-Z]+-\d+\b/).uniq.map do |ticket_id|
  link = "https://issues.serokell.io/issue/#{ticket_id}"
  "[#{ticket_id}](#{link})"
end.join(', ')
unless all_YT_tickets.empty?
  message("Mentioned YT tickets: #{all_YT_tickets}.")
end

end  # check on plain branches merge
