# LIGO Project Indexing

When a LIGO file is opened, the language server will scan for other LIGO files to keep track of which files are included and how they relate to the currently opened file. To avoid doing excessive work, the language server might look for a `.ligoproject` file, indicating which directories to be scanned.

The LIGO Language Server will look for the contents of the directory where the opened file is, as well as all its parent directories. Supposing that `/home/johndoe/ligo/foo.mligo` is open, then the language server will look for a `.ligoproject` file in `/home/jonhdoe/ligo`, `/home/jonhdoe`, `/home`, and `/`, in this order, using the first encounter of `.ligoproject`, if it exists.

If this file is not found, the language sever will ask the user to choose between three options:

1. The currently open folder in Visual Studio Code, if open.
2. The root Git project directory, if there's one, obtained with `git rev-parse --show-toplevel`.
3. No indexing, meaning that only the currently open file will be indexed.

If the user has chosen option *1.* or *2.*, the language server will ask whether a `.ligoproject` file should be created automatically at the specified path if the choice is "Yes".

It's expected that `.ligoproject` is commited and pushed to repositories containing LIGO files.

## Specification of `.ligoproject`

For now, the `.ligoproject` file only serves as a marker and no data is read or written to it, meaning it's an empty file by default, but this might change in the future.
