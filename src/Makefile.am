INCLUDES=$(XAPIAN_CXXFLAGS) $(GMIME_CFLAGS) $(GLIB_CFLAGS)

bin_PROGRAMS=			\
	mu

mu_SOURCES=			\
	mu-config.h		\
	mu-config.c		\
	mu-index.c		\
	mu-index.h		\
	mu-log.c		\
	mu-log.h		\
	mu-maildir.c		\
	mu-maildir.h		\
	mu-msg-fields.c		\
	mu-msg-fields.h		\
	mu-msg-flags.c		\
	mu-msg-flags.h		\
	mu-msg-gmime.c		\
	mu-msg-gmime.h		\
	mu-msg-str.c		\
	mu-msg-str.h		\
	mu-msg-xapian-priv.hh	\
	mu-msg-xapian.cc	\
	mu-msg-xapian.h		\
	mu-query.h		\
	mu-query.c		\
	mu-query-xapian.cc	\
	mu-query-xapian.h	\
	mu-walk.c		\
	mu-walk.h		\
	mu-result.h		\
	mu-store-xapian.cc	\
	mu-store-xapian.h	\
	mu-util.h		\
	mu-util.c		\
	mu.c

mu_LDADD=			\
	$(XAPIAN_LIBS)		\
	$(GMIME_LIBS)		\
	$(GLIB_LIBS)
