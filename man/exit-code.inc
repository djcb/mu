#+include: macros.inc

* EXIT CODE

This command returns 0 upon successful completion, or a non-zero exit code
otherwise.

    0. Success
    2. No matches found. Try a different query
   11. Database schema mismatch: re-initialize *mu*, see {{{man-link(mu-init,1)}}}
   19. Failed to acquire lock. Some other program has exclusive access to the *mu* database
   99. Caught an exception

# Local Variables:
# mode: org
# End:
