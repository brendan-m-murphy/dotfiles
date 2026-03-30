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
