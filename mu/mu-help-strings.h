/* Do not edit - auto-generated. */
static const struct {
	MuConfigCmd cmd;
	const char *usage;
	const char *long_help;
} MU_HELP_STRINGS[] = {
	{ MU_CONFIG_CMD_ADD,

	"mu add <file> [<files>]\n",

	"mu add is the command to add specific measage files to the\n"
	"database. Each of the files must be specified with an\n"
	"absolute path\n"
	},

	{ MU_CONFIG_CMD_CFIND,

	"mu cfind [options] [<pattern>]\n",

	"mu cfind is the mu command to find contacts in the mu database and export them\n"
	"for use in other programs.\n"
	},

	{ MU_CONFIG_CMD_EXTRACT,

	"mu extract [options] <file>\n",

	"mu extract is the mu command to display and save message parts\n"
	"(attachments), and open them with other tools.\n"
	},

	{ MU_CONFIG_CMD_FIND,

	"mu find [options] <search expression>\n",

	"mu find is the mu command for searching e-mail message that were\n"
	"stored earlier using mu index(1).\n"
	},

	{ MU_CONFIG_CMD_HELP,

	"mu help <command>\n",

	"mu find is the mu command to get help about <command>.\n"
	},

	{ MU_CONFIG_CMD_INDEX,

	"mu index [options]\n",

	"mu index is the mu command for scanning the contents of Maildir\n"
	"directories and storing the results in a Xapian database.The\n"
	"data can then be queried using mu-find(1).\n"
	},

	{ MU_CONFIG_CMD_MKDIR,

	"mu mkdir [options] <dir> [<dirs>]\n",

	"mu mkdir is the mu command for creating Maildirs.It does not\n"
	"use the mu database.\n"
	},

	{ MU_CONFIG_CMD_REMOVE,

	"mu remove [options] <file> [<files>]\n",

	"mu remove is the mu command to remove messages from the database.\n"
	},

	{ MU_CONFIG_CMD_SERVER,

	"mu server [options]\n",

	"mu server starts a simple shell in which one can query and\n"
	"manipulate the mu database.The output of the commands is terms\n"
	"of Lisp symbolic expressions (s-exps).mu server is not meant for\n"
	"use by humans; instead, it is designed specificallyfor the\n"
	"mu4e e-mail client.\n"
	},

	{ MU_CONFIG_CMD_VERIFY,

	"mu verify [options] <msgfile>\n",

	"mu verify is the mu command for verifying message signatures\n"
	"(such as PGP/GPG signatures)and displaying information about them.\n"
	"The command works on message files, and does not require\n"
	"the message to be indexed in the database.\n"
	},

	{ MU_CONFIG_CMD_VIEW,

	"mu view [options] <file> [<files>]\n",

	"mu view is the mu command for displaying e-mail message files. It\n"
	"works on message files and does not require the message to be\n"
	"indexed in the database.\n"
	},

	{ MU_CONFIG_CMD_INSPECT,

	"mu inspect [options] [<types>]\n",

	"mu inspect is the mu command for types and terms stored in the\n"
	"database. If no <types> are specified, prints all types.\n"
	},

};
/* the end */
