#!/usr/bin/env python3

# last refactor: 2021-04-20

# pip3 install GitPython
from git import Repo
import re, subprocess, sys
from datetime import datetime, date

repo = Repo()

files = subprocess.check_output(['git', 'ls-tree', '-r', 'HEAD', '--name-only', '-z'])
files = files.decode('utf-8').strip('\0').split('\0')

scores = []

now = date.today()

for i,file in enumerate(files):
    print('\033[2K[' + str(round(i * 100 / len(files), 2)) + '% ' + file + ']', file=sys.stderr, end='\r')

    # rules on files to skip entirely go here:
    if file.startswith(('changelog/', 'gitlab-pages/')):
        continue # skip file.
    
    commits = subprocess.check_output(['git', 'log', '--format=%H', file]).decode('utf-8').strip('\n').split('\n')

    # number of commits since last review
    # sum of length of diffs
    # length of file
    # for commit in commits:

    date_file_added = subprocess.check_output(['git', 'log', '--diff-filter=A', '--date=short', '--format=%ad', '--', file]).decode('utf-8').strip('\n').split('\n')[0]
    date_file_added = datetime.strptime(date_file_added, '%Y-%m-%d')
    days_since_file_added = (now - date_file_added.date()).days

    # default to last_refactor
    days_since_last_refactor = None
    head_contents = subprocess.check_output(['git', 'show', 'HEAD' + ':' + file])
    m = re.search(br'last refactor: ([0-9]{4}-[0-9]{2}-[0-9]{2})', head_contents)
    if m is not None:
        last = m.group(1).decode('utf-8')
        last = datetime.strptime(last, '%Y-%m-%d')
        days_since_last_refactor = (now - last.date()).days
    
    #contents = subprocess.check_output(['git', 'show', commit + ':' + file]).strip(b'\n').split(b'\n')
    
    # TODO: use other scores here
    score = (days_since_last_refactor or days_since_file_added)

    print('\033[2K', file=sys.stderr, end='')
    scores.append([file, score])

scores.sort(key=lambda x: x[1])

print('\033[2K', file=sys.stderr, end='')
sys.stderr.flush()
print('\n'.join([x[0] for x in scores]))