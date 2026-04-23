# setup direnv
mkvenv_envrc() {
  local venv_name="${1:-.venv}"
  local envrc_path=".envrc"

  if [ -f "$envrc_path" ]; then
    printf ".envrc already exists. Overwrite? [y/N] "
    read -r ans
    case "$ans" in
      [yY]|[yY][eE][sS]) ;;
      *) echo "Aborted."; return 1 ;;
    esac
  fi

  cat > "$envrc_path" <<EOF
export VIRTUAL_ENV="\$PWD/$venv_name"
layout python3
EOF

  command -v direnv >/dev/null 2>&1 || {
    echo "direnv not found in PATH"
    return 1
  }

  direnv allow
  echo "Created $envrc_path for $venv_name"
}

alias mkenvrc=mkvenv_envrc

ch() {
  local default_config="${CHUNKHOUND_CONFIG_FILE:-$HOME/.config/chunkhound/default.json}"

  if [[ $# -eq 0 ]]; then
    chunkhound
    return
  fi

  local cmd="$1"
  shift

  case "$cmd" in
    index|mcp|search|research|calibrate)
      for arg in "$@"; do
        if [[ "$arg" == "--config" ]]; then
          chunkhound "$cmd" "$@"
          return
        fi
      done
      chunkhound "$cmd" --config "$default_config" "$@"
      ;;
    *)
      chunkhound "$cmd" "$@"
      ;;
  esac
}

# make it easier to convert markdown to org
md2org() {
  pandoc -f markdown+lists_without_preceding_blankline-auto_identifiers \
         -t org \
         --wrap=preserve \
         "$@"
}
