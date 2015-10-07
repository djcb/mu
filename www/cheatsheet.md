---
layout: default
permalink: code/mu/cheatsheet.html
---

# Mu Cheatsheet

  Here are some tips for using `mu`. If you want to know more, please
  refer to the `mu` man pages. For a quick warm-up, there's also the
  `mu-easy` man-page.
 
## Indexing your mail
``` $ mu index```

If `mu` did not guess the right Maildir, you can set it explicitly:

``` $ mu index --maildir=~/MyMaildir```

### Excluding directories from indexing

If you want to exclude certain directories from being indexed (for example,
directories with spam-messages), put a file called `.noindex` in the directory
to exlude, and it will be ignored when indexing (including its children)

## Finding messages

After you have indexed your messages, you can search them. Here are some
examples. Also note the `--threads` argument to get a threaded display of
the messages, and `--color` to get colors (both since 0.9.7).

### messages about Helsinki (in message body, subject, sender, ...)
``` $ mu find Helsinki```

### messages to Jack with subject jellyfish containing the word tumbleweed
``` $ mu find to:Jack subject:jellyfish tumbleweed```

### messages between 2 kilobytes and a 2Mb, written in December 2009 with an attachment from Bill
``` $ mu find size:2k..2m date:20091201..20093112 flag:attach from:bill```

### signed messages about apples *OR* oranges
```  $ mu find flag:signed apples OR oranges```

### messages about yoghurt in the Sent Items folder (note the quoting):
```  $ mu find maildir:'/Sent Items' yoghurt```


### unread messages about things starting with 'soc' (soccer, society, socrates, ...)
```  $ mu find 'subject:soc*' flag:unread```

Note, the '*' only works at the /end/ of a search term, and you need to
quote it or the shell will interpret it before `mu` sees it.
(searching using the '*' wildcard is available since mu 0.9.6)

### finding messages with images as attachment
```  $ mu find 'mime:image/*' ```
	(since mu version 0.9.8)

### finding messages with 'milk' in one of its text parts (such as text-based attachments):
```  $ mu find embed:milk ```
	(since mu version 0.9.8)

### finding /all/ your messages
```  $ mu find ""```
	(since mu version 0.9.7)

## Finding contacts

Contacts (names + email addresses) are cached separately, and can be
searched with `mu cfind` (after your messages have been indexed):

### all contacts with 'john' in either name or e-mail address
``` $ mu cfind john```

    `mu cfind` takes a regular expression for matching.

You can export the contact information to a number of formats for use
in e-mail clients. For example:

### export /all/ your contacts to the `mutt` addressbook format
``` $ mu cfind --format=mutt-alias```

Other formats are: `plain`, `mutt-ab`, `wl` (Wanderlust), `org-contact`,
`bbdb` and `csv` (comma-separated values).

## Retrieving attachments from messages

You can retrieve attachments from messages using `mu extract`, which takes a
message file as an argument. Without any other arguments, it displays the
MIME-parts of the message. You can then get specific attachments:

``` $ mu extract --parts=3,4 my-msg-file```

will get you parts 3 and 4. You can also extract files based on their name:

``` $ mu extract my-msg-file '.*\.jpg'```

The second argument is a case-insensitive regular expression, and the command
will extract any files matching the pattern -- in the example, all
`.jpg`-files.

Do not confuse the '.*' /regular expression/ in `mu extract` (and `mu
cfind`) with the '*' /wildcard/ in `mu find`.

## Getting more colorful output

Some of the `mu` commands, such as `mu find`, `mu cfind` and `mu view`
support colorized output. By default this is turned off, but you can enable
it with `--color`, or setting the `MU_COLORS` environment variable to
non-empty.

``` $ mu find --color capibara```

   (since `mu` version 0.9.6)

## Integration with mail clients

The `mu-find` man page contains examples for `mutt` and `wanderlust`. And
since version 0.9.8, `mu` includes its own e-mail client for `emacs`, `mu4e`.

## Viewing specific messages

You can view message contents with `mu view`; it does not use the database
and simply takes a message file as it's argument:

``` $ mu view ~/Maildir/inbox/cur/message24```

You can also use `--color` to get colorized output, and `--summary` to get a
summary of the message contents instead of the whole thing.

## Further processing of matched messages

If you need to process the results of your queries with some other program,
you can return the results as a list of absolute paths to the messages found:

For example, to get the number of lines in all your messages mentioning
/banana/, you could use something like:

``` $ mu find --exec='wc -l'```

Note that we use 'l', so the returned message paths will be quoted. This is
useful if you have maildirs with spaces in their names.

For further processing, also the ~--format`(xml|sexp)~ can be useful. For
example,

``` $ mu find --format=xml pancake```

will give you a list of pancake-related messages in XML-format.
