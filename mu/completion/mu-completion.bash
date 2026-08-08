# bash completion for mu
#
# Install by sourcing this file from your .bashrc, or by placing it, named 'mu',
# in the bash-completion completions directory.

_mu_cmd_add() {
  case "$prev" in
    --muhome) COMPREPLY=($(compgen -d -- "$cur")); return;;
  esac
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-h --help --help-all --muhome -V --version -q --quiet -v --verbose --nocolor" -- "$cur"));;
    *) COMPREPLY=($(compgen -f -- "$cur"));;
  esac
}

_mu_cmd_cfind() {
  case "$prev" in
    -o|--format) COMPREPLY=($(compgen -W "bbdb csv json mutt-ab mutt-alias org-contact plain wl" -- "$cur")); return;;
    --after) return;;
    -n|--maxnum) return;;
    --muhome) COMPREPLY=($(compgen -d -- "$cur")); return;;
  esac
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-h --help --help-all -o --format -p --personal --after -n --maxnum --muhome -V --version -q --quiet -v --verbose --nocolor" -- "$cur"));;
  esac
}

_mu_cmd_extract() {
  case "$prev" in
    --parts) return;;
    --target-dir) COMPREPLY=($(compgen -d -- "$cur")); return;;
    --matches) return;;
  esac
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-h --help --help-all -r --auto-retrieve --decrypt -a --save-attachments --save-all --overwrite --play --parts --target-dir -u --uncooked --matches -V --version -q --quiet -v --verbose --nocolor" -- "$cur"));;
    *) COMPREPLY=($(compgen -f -- "$cur"));;
  esac
}

_mu_cmd_fields() {
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-h --help --help-all -V --version -q --quiet -v --verbose --nocolor" -- "$cur"));;
  esac
}

_mu_cmd_find() {
  case "$prev" in
    -o|--format) COMPREPLY=($(compgen -W "json json2 links plain sexp xml" -- "$cur")); return;;
    -n|--maxnum) return;;
    -f|--fields) return;;
    -s|--sortfield) COMPREPLY=($(compgen -W "a bcc c cc changed d date f flags from g h i k l labels language list m maildir message-id p path priority q r references s size subject t tags thread to u utc-offset v w x z" -- "$cur")); return;;
    -b|--bookmark) return;;
    --linksdir) COMPREPLY=($(compgen -d -- "$cur")); return;;
    --after) return;;
    --summary-len) return;;
    --exec) return;;
    --muhome) COMPREPLY=($(compgen -d -- "$cur")); return;;
  esac
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-h --help --help-all -t --threads -u --skip-dups -r --include-related -a --analyze -o --format -n --maxnum -f --fields -s --sortfield -z --reverse -b --bookmark --clearlinks --linksdir --after --summary-len --exec --muhome -V --version -q --quiet -v --verbose --nocolor" -- "$cur"));;
  esac
}

_mu_cmd_help() {
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-h --help --help-all -V --version -q --quiet -v --verbose --nocolor" -- "$cur"));;
  esac
}

_mu_cmd_index() {
  case "$prev" in
    --muhome) COMPREPLY=($(compgen -d -- "$cur")); return;;
  esac
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-h --help --help-all --lazy-check --nocleanup --reindex --muhome -V --version -q --quiet -v --verbose --nocolor" -- "$cur"));;
  esac
}

_mu_cmd_info() {
  case "$prev" in
    --muhome) COMPREPLY=($(compgen -d -- "$cur")); return;;
  esac
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-h --help --help-all --muhome -V --version -q --quiet -v --verbose --nocolor" -- "$cur"));;
  esac
}

_mu_cmd_init() {
  case "$prev" in
    -m|--maildir) COMPREPLY=($(compgen -d -- "$cur")); return;;
    --personal-address|--my-address) return;;
    --ignored-address) return;;
    --max-message-size) return;;
    --batch-size) return;;
    --muhome) COMPREPLY=($(compgen -d -- "$cur")); return;;
  esac
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-h --help --help-all -m --maildir --personal-address --my-address --ignored-address --max-message-size --batch-size --support-ngrams --reinit --muhome -V --version -q --quiet -v --verbose --nocolor" -- "$cur"));;
  esac
}

_mu_cmd_labels_update() {
  case "$prev" in
    --labels) return;;
    --muhome) COMPREPLY=($(compgen -d -- "$cur")); return;;
  esac
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-h --help --help-all --labels -n --dry-run --muhome" -- "$cur"));;
  esac
}

_mu_cmd_labels_clear() {
  case "$prev" in
    --muhome) COMPREPLY=($(compgen -d -- "$cur")); return;;
  esac
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-h --help --help-all -n --dry-run --muhome" -- "$cur"));;
  esac
}

_mu_cmd_labels_list() {
  case "$prev" in
    --muhome) COMPREPLY=($(compgen -d -- "$cur")); return;;
  esac
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-h --help --help-all --muhome" -- "$cur"));;
  esac
}

_mu_cmd_labels_restore_list() {
  case "$prev" in
    --muhome) COMPREPLY=($(compgen -d -- "$cur")); return;;
  esac
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-h --help --help-all --muhome" -- "$cur"));;
  esac
}

_mu_cmd_labels_export() {
  case "$prev" in
    --muhome) COMPREPLY=($(compgen -d -- "$cur")); return;;
  esac
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-h --help --help-all --muhome" -- "$cur"));;
    *) COMPREPLY=($(compgen -f -- "$cur"));;
  esac
}

_mu_cmd_labels_import() {
  case "$prev" in
    --muhome) COMPREPLY=($(compgen -d -- "$cur")); return;;
  esac
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-h --help --help-all -n --dry-run --muhome" -- "$cur"));;
    *) COMPREPLY=($(compgen -f -- "$cur"));;
  esac
}

_mu_cmd_labels() {
  local i=$1 sub=
  while (( i < COMP_CWORD )); do
    case "${COMP_WORDS[i]}" in
      =|-*) ((i++));;
      *) sub="${COMP_WORDS[i]}"; ((i++)); break;;
    esac
  done
  case "$sub" in
    update) _mu_cmd_labels_update "$i"; return;;
    clear) _mu_cmd_labels_clear "$i"; return;;
    list) _mu_cmd_labels_list "$i"; return;;
    restore-list) _mu_cmd_labels_restore_list "$i"; return;;
    export) _mu_cmd_labels_export "$i"; return;;
    import) _mu_cmd_labels_import "$i"; return;;
  esac
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-h --help --help-all -V --version -q --quiet -v --verbose --nocolor" -- "$cur"));;
    *) COMPREPLY=($(compgen -W "update clear list restore-list export import" -- "$cur"));;
  esac
}

_mu_cmd_mkdir() {
  case "$prev" in
    --mode) return;;
  esac
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-h --help --help-all --mode -V --version -q --quiet -v --verbose --nocolor" -- "$cur"));;
    *) COMPREPLY=($(compgen -d -- "$cur"));;
  esac
}

_mu_cmd_move() {
  case "$prev" in
    --flags) return;;
    --muhome) COMPREPLY=($(compgen -d -- "$cur")); return;;
  esac
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-h --help --help-all --change-name --update-dups -n --dry-run --flags --muhome -V --version -q --quiet -v --verbose --nocolor" -- "$cur"));;
    *) COMPREPLY=($(compgen -f -- "$cur"));;
  esac
}

_mu_cmd_remove() {
  case "$prev" in
    --muhome) COMPREPLY=($(compgen -d -- "$cur")); return;;
  esac
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-h --help --help-all --muhome -V --version -q --quiet -v --verbose --nocolor" -- "$cur"));;
    *) COMPREPLY=($(compgen -f -- "$cur"));;
  esac
}

_mu_cmd_scm() {
  case "$prev" in
    --eval) return;;
    --muhome) COMPREPLY=($(compgen -d -- "$cur")); return;;
  esac
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-h --help --help-all --listen --eval --muhome -V --version -q --quiet -v --verbose --nocolor" -- "$cur"));;
    *) COMPREPLY=($(compgen -f -- "$cur"));;
  esac
}

_mu_cmd_server() {
  case "$prev" in
    --eval) return;;
    --muhome) COMPREPLY=($(compgen -d -- "$cur")); return;;
  esac
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-h --help --help-all --commands --eval --allow-temp-file --listen --muhome -V --version -q --quiet -v --verbose --nocolor" -- "$cur"));;
  esac
}

_mu_cmd_verify() {
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-h --help --help-all -r --auto-retrieve --decrypt -V --version -q --quiet -v --verbose --nocolor" -- "$cur"));;
    *) COMPREPLY=($(compgen -f -- "$cur"));;
  esac
}

_mu_cmd_view() {
  case "$prev" in
    -o|--format) COMPREPLY=($(compgen -W "html plain sexp" -- "$cur")); return;;
    --summary-len) return;;
  esac
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-h --help --help-all -o --format -r --auto-retrieve --decrypt --summary-len --terminate -V --version -q --quiet -v --verbose --nocolor" -- "$cur"));;
    *) COMPREPLY=($(compgen -f -- "$cur"));;
  esac
}

_mu_cmd() {
  local i=$1 sub=
  while (( i < COMP_CWORD )); do
    case "${COMP_WORDS[i]}" in
      =|-*) ((i++));;
      *) sub="${COMP_WORDS[i]}"; ((i++)); break;;
    esac
  done
  case "$sub" in
    add) _mu_cmd_add "$i"; return;;
    cfind) _mu_cmd_cfind "$i"; return;;
    extract) _mu_cmd_extract "$i"; return;;
    fields) _mu_cmd_fields "$i"; return;;
    find) _mu_cmd_find "$i"; return;;
    help) _mu_cmd_help "$i"; return;;
    index) _mu_cmd_index "$i"; return;;
    info) _mu_cmd_info "$i"; return;;
    init) _mu_cmd_init "$i"; return;;
    labels) _mu_cmd_labels "$i"; return;;
    mkdir) _mu_cmd_mkdir "$i"; return;;
    move) _mu_cmd_move "$i"; return;;
    remove) _mu_cmd_remove "$i"; return;;
    scm) _mu_cmd_scm "$i"; return;;
    server) _mu_cmd_server "$i"; return;;
    verify) _mu_cmd_verify "$i"; return;;
    view) _mu_cmd_view "$i"; return;;
  esac
  case "$cur" in
    -*) COMPREPLY=($(compgen -W "-V --version -h --help --help-all -q --quiet -v --verbose --nocolor" -- "$cur"));;
    *) COMPREPLY=($(compgen -W "add cfind extract fields find help index info init labels mkdir move remove scm server verify view" -- "$cur"));;
  esac
}

_mu() {
  local cur prev
  COMPREPLY=()
  cur="${COMP_WORDS[COMP_CWORD]}"
  prev="${COMP_WORDS[COMP_CWORD-1]}"
  # handle '--opt=value' (bash splits on '=')
  [[ "$cur" == "=" ]] && cur=""
  [[ "$prev" == "=" && $COMP_CWORD -ge 2 ]] && prev="${COMP_WORDS[COMP_CWORD-2]}"

  _mu_cmd 1
}

complete -F _mu mu
