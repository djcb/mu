* EXIT CODE

This command returns 0 upon successful completion, or a non-zero exit code
otherwise.

    0. success
    2. no matches found. Try a different query
   11. database schema mismatch. You need to re-initialize ~mu~, see *mu-init(1*)
   19. failed to acquire lock. Some other program has exclusive access to the mu database

# Local Variables:
# mode: org
# End:
