* EXIT CODE

This command returns 0 upon successful completion, or a non-zero exit code
otherwise. Typical values are 2 (no matches found), 11 (database schema
mismatch) and 12 (failed to acquire database lock).

** no matches found (2)

Nothing matching found; try a different query

** database schema mismatch (11)

You need to re-initialize ~mu~, see *mu-init(1*)

** failed to acquire lock (19)

Some other program has exclusive access to the mu (Xapian) database

# Local Variables:
# mode: org
# End:
