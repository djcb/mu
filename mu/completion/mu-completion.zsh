#compdef mu

# completions for zsh; place in a file called _mu in a directory
# in ${fpath}

_mu_add() {
  _arguments -S \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '--muhome=[Specify alternative mu directory]:dir:_files -/' \
    '(-V --version)'{-V,--version}'[Display program version information and exit]' \
    '(-q --quiet)'{-q,--quiet}'[Hide non-essential output]' \
    '(-v --verbose)'{-v,--verbose}'[Show verbose output]' \
    '--nocolor[Don'\''t show ANSI colors]' \
    '*:files:_files'
}

_mu_cfind() {
  _arguments -S \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '(-o --format)'{-o+,--format=}'[Output format]:format:(bbdb csv json mutt-ab mutt-alias org-contact plain wl)' \
    '(-p --personal)'{-p,--personal}'[Only show '\''personal'\'' contacts]' \
    '--after=[Only show results after some timestamps]:time_t:' \
    '(-n --maxnum)'{-n+,--maxnum=}'[Maximum number of results]:number:' \
    '--muhome=[Specify alternative mu directory]:dir:_files -/' \
    '(-V --version)'{-V,--version}'[Display program version information and exit]' \
    '(-q --quiet)'{-q,--quiet}'[Hide non-essential output]' \
    '(-v --verbose)'{-v,--verbose}'[Show verbose output]' \
    '--nocolor[Don'\''t show ANSI colors]' \
    '1::pattern:'
}

_mu_extract() {
  _arguments -S \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '(-r --auto-retrieve)'{-r,--auto-retrieve}'[Attempt to automatically retrieve online keys]' \
    '--decrypt[Attempt to decrypt]' \
    '(-a --save-attachments)'{-a,--save-attachments}'[Save all attachments]' \
    '--save-all[Save all MIME parts]' \
    '--overwrite[Overwrite existing files]' \
    '--play[Attempt to open the extracted parts]' \
    '*''--parts=[Save specific parts (comma-sep'\''d list)]:parts:' \
    '--target-dir=[Target directory for saving]:dir:_files -/' \
    '(-u --uncooked)'{-u,--uncooked}'[Avoid massaging extracted file-names]' \
    '--matches=[Regular expression for files to save]:filename-rx:' \
    '(-V --version)'{-V,--version}'[Display program version information and exit]' \
    '(-q --quiet)'{-q,--quiet}'[Hide non-essential output]' \
    '(-v --verbose)'{-v,--verbose}'[Show verbose output]' \
    '--nocolor[Don'\''t show ANSI colors]' \
    '1::message-path:_files' \
    '2::filename-rx:'
}

_mu_fields() {
  _arguments -S \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '(-V --version)'{-V,--version}'[Display program version information and exit]' \
    '(-q --quiet)'{-q,--quiet}'[Hide non-essential output]' \
    '(-v --verbose)'{-v,--verbose}'[Show verbose output]' \
    '--nocolor[Don'\''t show ANSI colors]'
}

_mu_find() {
  _arguments -S \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '(-t --threads)'{-t,--threads}'[Show message threads]' \
    '(-u --skip-dups)'{-u,--skip-dups}'[Show only one of messages with same message-id]' \
    '(-r --include-related)'{-r,--include-related}'[Include related messages in results]' \
    '(-a --analyze)'{-a,--analyze}'[Analyze the query]' \
    '(-o --format)'{-o+,--format=}'[Output format]:format:(json json2 links plain sexp xml)' \
    '(-n --maxnum)'{-n+,--maxnum=}'[Maximum number of results]:number:' \
    '(-f --fields)'{-f+,--fields=}'[Fields to display]:value:' \
    '(-s --sortfield)'{-s+,--sortfield=}'[Field to sort the results by]:field:(a bcc c cc changed d date f flags from g h i k l labels language list m maildir message-id p path priority q r references s size subject t tags thread to u utc-offset v w x z)' \
    '(-z --reverse)'{-z,--reverse}'[Sort in descending order]' \
    '(-b --bookmark)'{-b+,--bookmark=}'[Use bookmarked query]:bookmark:' \
    '--clearlinks[Clear old links first]' \
    '--linksdir=[Target directory for symlinks]:dir:_files -/' \
    '--after=[Only show messages whose message file was changed after some timestamp]:time_t:' \
    '--summary-len=[Use up to so many lines for the summary]:lines:' \
    '--exec=[Command to execute on message file]:command:' \
    '--muhome=[Specify alternative mu directory]:dir:_files -/' \
    '(-V --version)'{-V,--version}'[Display program version information and exit]' \
    '(-q --quiet)'{-q,--quiet}'[Hide non-essential output]' \
    '(-v --verbose)'{-v,--verbose}'[Show verbose output]' \
    '--nocolor[Don'\''t show ANSI colors]' \
    '*:query:'
}

_mu_help() {
  _arguments -S \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '(-V --version)'{-V,--version}'[Display program version information and exit]' \
    '(-q --quiet)'{-q,--quiet}'[Hide non-essential output]' \
    '(-v --verbose)'{-v,--verbose}'[Show verbose output]' \
    '--nocolor[Don'\''t show ANSI colors]' \
    '1::command:'
}

_mu_index() {
  _arguments -S \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '--lazy-check[Skip based on dir-timestamps]' \
    '--nocleanup[Don'\''t clean up database after indexing]' \
    '--reindex[Perform a complete reindexing]' \
    '--muhome=[Specify alternative mu directory]:dir:_files -/' \
    '(-V --version)'{-V,--version}'[Display program version information and exit]' \
    '(-q --quiet)'{-q,--quiet}'[Hide non-essential output]' \
    '(-v --verbose)'{-v,--verbose}'[Show verbose output]' \
    '--nocolor[Don'\''t show ANSI colors]'
}

_mu_info() {
  _arguments -S \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '--muhome=[Specify alternative mu directory]:dir:_files -/' \
    '(-V --version)'{-V,--version}'[Display program version information and exit]' \
    '(-q --quiet)'{-q,--quiet}'[Hide non-essential output]' \
    '(-v --verbose)'{-v,--verbose}'[Show verbose output]' \
    '--nocolor[Don'\''t show ANSI colors]' \
    '1::topic:'
}

_mu_init() {
  _arguments -S \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '(-m --maildir)'{-m+,--maildir=}'[Root maildir]:maildir:_files -/' \
    '*'{--personal-address=,--my-address=}'[Personal e-mail address or regexp (can be used multiple times)]:address:' \
    '*''--ignored-address=[Ignored e-mail address or regexp]:address:' \
    '--max-message-size=[Maximum allowed message size in bytes]:value:' \
    '--batch-size=[Maximum size of database transaction]:value:' \
    '--support-ngrams[Support CJK n-grams for querying/indexing]' \
    '--reinit[Re-initialize database with current settings]' \
    '--muhome=[Specify alternative mu directory]:dir:_files -/' \
    '(-V --version)'{-V,--version}'[Display program version information and exit]' \
    '(-q --quiet)'{-q,--quiet}'[Hide non-essential output]' \
    '(-v --verbose)'{-v,--verbose}'[Show verbose output]' \
    '--nocolor[Don'\''t show ANSI colors]'
}

_mu_labels_update() {
  _arguments -S \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '*''--labels=[One or more comma-separated +label,-label]:delta-label:' \
    '(-n --dry-run)'{-n,--dry-run}'[Output what would change without changing anything]' \
    '--muhome=[Specify alternative mu directory]:dir:_files -/' \
    '1:query:'
}

_mu_labels_clear() {
  _arguments -S \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '(-n --dry-run)'{-n,--dry-run}'[Output what would change without changing anything]' \
    '--muhome=[Specify alternative mu directory]:dir:_files -/' \
    '1:query:'
}

_mu_labels_list() {
  _arguments -S \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '--muhome=[Specify alternative mu directory]:dir:_files -/'
}

_mu_labels_restore_list() {
  _arguments -S \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '--muhome=[Specify alternative mu directory]:dir:_files -/'
}

_mu_labels_export() {
  _arguments -S \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '--muhome=[Specify alternative mu directory]:dir:_files -/' \
    '1::output:_files'
}

_mu_labels_import() {
  _arguments -S \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '(-n --dry-run)'{-n,--dry-run}'[Output what would change without changing anything]' \
    '--muhome=[Specify alternative mu directory]:dir:_files -/' \
    '1:input:_files'
}

_mu_labels_commands() {
  local -a commands
  commands=(
    'update:update labels'
    'clear:clear all labels from matched messages'
    'list:list labels in the store'
    'restore-list:restore the labels cache'
    'export:export labels to a file'
    'import:import labels from a file'
  )
  _describe -t commands 'command' commands
}

_mu_labels() {
  local curcontext="$curcontext" state line ret=1
  _arguments -S -C \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '(-V --version)'{-V,--version}'[Display program version information and exit]' \
    '(-q --quiet)'{-q,--quiet}'[Hide non-essential output]' \
    '(-v --verbose)'{-v,--verbose}'[Show verbose output]' \
    '--nocolor[Don'\''t show ANSI colors]' \
    '1: :_mu_labels_commands' \
    '*::arg:->args' && ret=0

  case $state in
    (args)
      curcontext="${curcontext%:*:*}:mu-labels-${words[1]}:"
      case ${words[1]} in
        (update) _mu_labels_update && ret=0;;
        (clear) _mu_labels_clear && ret=0;;
        (list) _mu_labels_list && ret=0;;
        (restore-list) _mu_labels_restore_list && ret=0;;
        (export) _mu_labels_export && ret=0;;
        (import) _mu_labels_import && ret=0;;
      esac
      ;;
  esac

  return ret
}

_mu_mkdir() {
  _arguments -S \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '--mode=[Set the access mode (octal)]:mode:' \
    '(-V --version)'{-V,--version}'[Display program version information and exit]' \
    '(-q --quiet)'{-q,--quiet}'[Hide non-essential output]' \
    '(-v --verbose)'{-v,--verbose}'[Show verbose output]' \
    '--nocolor[Don'\''t show ANSI colors]' \
    '*:dirs:_files -/'
}

_mu_move() {
  _arguments -S \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '--change-name[Change name of target file]' \
    '--update-dups[Update duplicate messages too]' \
    '(-n --dry-run)'{-n,--dry-run}'[Print target name, but do not change anything]' \
    '--flags=[Target flags]:flags:' \
    '--muhome=[Specify alternative mu directory]:dir:_files -/' \
    '(-V --version)'{-V,--version}'[Display program version information and exit]' \
    '(-q --quiet)'{-q,--quiet}'[Hide non-essential output]' \
    '(-v --verbose)'{-v,--verbose}'[Show verbose output]' \
    '--nocolor[Don'\''t show ANSI colors]' \
    '1:source:_files' \
    '2::destination:_files -/'
}

_mu_remove() {
  _arguments -S \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '--muhome=[Specify alternative mu directory]:dir:_files -/' \
    '(-V --version)'{-V,--version}'[Display program version information and exit]' \
    '(-q --quiet)'{-q,--quiet}'[Hide non-essential output]' \
    '(-v --verbose)'{-v,--verbose}'[Show verbose output]' \
    '--nocolor[Don'\''t show ANSI colors]' \
    '*:files:_files'
}

_mu_scm() {
  _arguments -S \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '--listen[Start SCM REPL on a domain socket]' \
    '--eval=[Expression to evaluate]:value:' \
    '--muhome=[Specify alternative mu directory]:dir:_files -/' \
    '(-V --version)'{-V,--version}'[Display program version information and exit]' \
    '(-q --quiet)'{-q,--quiet}'[Hide non-essential output]' \
    '(-v --verbose)'{-v,--verbose}'[Show verbose output]' \
    '--nocolor[Don'\''t show ANSI colors]' \
    '1::script-path:_files' \
    '*:script-args:'
}

_mu_server() {
  _arguments -S \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '--commands[List available commands]' \
    '--eval=[Evaluate mu server expression]:value:' \
    '--allow-temp-file[Allow for the temp-file optimization]' \
    '--listen[Start SCM REPL on a domain socket]' \
    '--muhome=[Specify alternative mu directory]:dir:_files -/' \
    '(-V --version)'{-V,--version}'[Display program version information and exit]' \
    '(-q --quiet)'{-q,--quiet}'[Hide non-essential output]' \
    '(-v --verbose)'{-v,--verbose}'[Show verbose output]' \
    '--nocolor[Don'\''t show ANSI colors]'
}

_mu_verify() {
  _arguments -S \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '(-r --auto-retrieve)'{-r,--auto-retrieve}'[Attempt to automatically retrieve online keys]' \
    '--decrypt[Attempt to decrypt]' \
    '(-V --version)'{-V,--version}'[Display program version information and exit]' \
    '(-q --quiet)'{-q,--quiet}'[Hide non-essential output]' \
    '(-v --verbose)'{-v,--verbose}'[Show verbose output]' \
    '--nocolor[Don'\''t show ANSI colors]' \
    '*:message-paths:_files'
}

_mu_view() {
  _arguments -S \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '(-o --format)'{-o+,--format=}'[Output format]:format:(html plain sexp)' \
    '(-r --auto-retrieve)'{-r,--auto-retrieve}'[Attempt to automatically retrieve online keys]' \
    '--decrypt[Attempt to decrypt]' \
    '--summary-len=[Use up to so many lines for the summary]:lines:' \
    '--terminate[Insert form-feed after each message]' \
    '(-V --version)'{-V,--version}'[Display program version information and exit]' \
    '(-q --quiet)'{-q,--quiet}'[Hide non-essential output]' \
    '(-v --verbose)'{-v,--verbose}'[Show verbose output]' \
    '--nocolor[Don'\''t show ANSI colors]' \
    '*:message-paths:_files'
}

_mu_commands() {
  local -a commands
  commands=(
    'add:Add messages to the database'
    'cfind:Find contacts matching some pattern'
    'extract:Extract attachments and other MIME-parts'
    'fields:Superseded by '\''mu info'\'''
    'find:Find messages matching some query'
    'help:Show help information'
    'index:Scan maildirs and store information'
    'info:Show information about mu'
    'init:Initialize the mu database'
    'labels:Manage message labels'
    'mkdir:Create a new Maildir'
    'move:Move a message or change its flags'
    'remove:Remove message from file-system and database'
    'scm:Start Guile/Scheme shell or run script'
    'server:Start a mu server (for mu4e)'
    'verify:Verify cryptographic signatures'
    'view:View specific messages'
  )
  _describe -t commands 'command' commands
}

_mu() {
  local curcontext="$curcontext" state line ret=1
  _arguments -S -C \
    '(-V --version)'{-V,--version}'[Display program version information and exit]' \
    '(-h --help)'{-h,--help}'[Show help information]' \
    '--help-all[Show help for all commands]' \
    '(-q --quiet)'{-q,--quiet}'[Hide non-essential output]' \
    '(-v --verbose)'{-v,--verbose}'[Show verbose output]' \
    '--nocolor[Don'\''t show ANSI colors]' \
    '1: :_mu_commands' \
    '*::arg:->args' && ret=0

  case $state in
    (args)
      curcontext="${curcontext%:*:*}:mu-${words[1]}:"
      case ${words[1]} in
        (add) _mu_add && ret=0;;
        (cfind) _mu_cfind && ret=0;;
        (extract) _mu_extract && ret=0;;
        (fields) _mu_fields && ret=0;;
        (find) _mu_find && ret=0;;
        (help) _mu_help && ret=0;;
        (index) _mu_index && ret=0;;
        (info) _mu_info && ret=0;;
        (init) _mu_init && ret=0;;
        (labels) _mu_labels && ret=0;;
        (mkdir) _mu_mkdir && ret=0;;
        (move) _mu_move && ret=0;;
        (remove) _mu_remove && ret=0;;
        (scm) _mu_scm && ret=0;;
        (server) _mu_server && ret=0;;
        (verify) _mu_verify && ret=0;;
        (view) _mu_view && ret=0;;
      esac
      ;;
  esac

  return ret
}

_mu "$@"
