from argparse import ArgumentParser
from gitlab import Gitlab
from marko.ext.gfm import GFM
from marko.md_renderer import MarkdownRenderer
from marko import block, inline, Markdown
import json
import re
import sys


gfm = Markdown(renderer=MarkdownRenderer, extensions=[GFM])


parser = ArgumentParser(
    description="Script for parsing MR information from MR template"
)

parser.add_argument("--hostname", type=str,
                    help="hostname of gitlab api endpoint")

parser.add_argument("--project-id", type=str, help="project id")

parser.add_argument("--mr-id", type=str, help="merge request id")

parser.add_argument("--token", type=str, help="private token")


HOSTNAME_REGEX = re.compile("^https?://[^/]+")
COMMENT_REGEX = re.compile("<!---.*-->")

TYPE_MAP = ["fixed", "added", "breaking", "performance", "none"]
COMPONENT_MAP = ["compiler", "website", "webide", "vscode-plugin", "debugger"]


def is_heading(elem, title):
    # I miss pattern matching :(
    return (
        isinstance(elem, block.Heading)
        and elem.level == 2
        and isinstance(elem.children, list)
        and len(elem.children) == 1
        and isinstance(elem.children[0], inline.RawText)
        and elem.children[0].children == title
    )


def get_checked_element(section_title):
    # Get concerned component
    elems = get_prefixed_elem(markdown.children, section_title, block.List)
    checks = [i for i, par in enumerate(
        elems.children) if par.children[0].checked]
    if len(checks) != 1:
        print(
            f"Expected exactly 1 '{section_title}', but got {len(checks)}", file=sys.stderr)
        exit(1)
    return checks[0]


def get_prefixed_elem(elems, title, type):
    idx = next(i for i, elem in enumerate(elems) if is_heading(elem, title))

    return next(elem for elem in elems[idx:] if isinstance(elem, type))


def get_changelog(elems):
    idx1 = next(i for i, elem in enumerate(elems)
                if is_heading(elem, "Changelog"))

    # Find next lvl2 heading
    idx2 = (
        idx1
        + 1
        + next(
            i
            for i, elem in enumerate(elems[idx1 + 1:])
            if isinstance(elem, block.Heading) and elem.level == 2
        )
    )

    # Elems for changelog are the range (idx1, idx2) (hence [idx1 + 1, idx2))
    elems = elems[idx1 + 1: idx2]

    # Call to __enter__ required to set prefixes to ""
    gfm.renderer.__enter__()

    # Render the elems
    changelog_details = "".join([gfm.renderer.render(elem) for elem in elems])
    changelog_details = re.sub(COMMENT_REGEX, '', changelog_details)

    return changelog_details


if __name__ == "__main__":
    args = parser.parse_args()

    host = HOSTNAME_REGEX.match(args.hostname).group(0)

    if not host:
        print(
            f"Host {host} doesn't match expected hostname format", file=sys.stderr)
        exit(1)

    gl = Gitlab(url=host, private_token=args.token)
    project = gl.projects.get(args.project_id)
    mr = project.mergerequests.get(args.mr_id)

    title = mr.title
    author = mr.author["username"]
    raw_description = mr.description

    markdown = gfm.parse(raw_description)

    # Get type details
    type = TYPE_MAP[get_checked_element("Types of changes")]

    # Get changelog details
    if type != "none":
        component = COMPONENT_MAP[get_checked_element("Component")]

        changelog_details = get_changelog(markdown.children)
        # Force \n before ``` to avoid a bug in case of block code in list
        changelog_details = changelog_details.replace('```', '\n```')
        # To save the \n it should be \\n which will be parsed later as line break
        changelog_details = changelog_details.replace('\n', '\\\\n')
        # description is stored between '' in changelog file. so if there is ' we have to skip it
        changelog_details = changelog_details.replace("\"", "\\\"")
        # formatter put <p> and </p> we don't want them
        changelog_details = changelog_details.replace('<p>', '')
        changelog_details = changelog_details.replace('</p>', '')

        title = title.replace("\"", "\\\"")
    else:
       sys.exit(101)

    target_file = f"changelog-tools/{component}/{args.mr_id}"
    if component == "compiler":
       target_file = f"changelog/{args.mr_id}"
    f = open(target_file, "x")
    f.write(f'''
author: {author}
description: "{changelog_details}"
merge_request: '{args.mr_id}'
title: "{title}"
type: {type}
            ''')
    f.close()
