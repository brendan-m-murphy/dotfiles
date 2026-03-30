# setup direnv
mkvenv_envrc() {
  # Usage:
  #   mkvenv_envrc           -> uses ".venv"
  #   mkvenv_envrc myenv     -> uses "myenv"

  local venv_name="${1:-.venv}"
  local envrc_path=".envrc"

  [ -d "$venv_name" ] || {
    echo "Warning: $venv_name does not exist"
  }

  # Refuse to overwrite unless explicitly confirmed
  if [ -f "$envrc_path" ]; then
    printf ".envrc already exists. Overwrite? [y/N] "
    read -r ans
    case "$ans" in
      [yY][eE][sS]|[yY]) ;;
      *) echo "Aborted."; return 1 ;;
    esac
  fi

  printf 'layout python %s\n' "$venv_name" > "$envrc_path"

  # Ensure direnv is available
  if ! command -v direnv >/dev/null 2>&1; then
    echo "direnv not found in PATH"
    return 1
  fi

  direnv allow

  echo "Created $envrc_path with: layout python $venv_name"
}
