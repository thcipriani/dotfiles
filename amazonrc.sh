#!/bin/sh

s3cfg="$HOME/.s3cfg"
key='AWS_ACCESS_KEY_ID'
secret='AWS_SECRET_ACCESS_KEY'
aws_key=''
aws_secret=''
shell=''

err() {
  printf "ERROR: %s" "$@"
}

get_shell() {
  shell=$(basename ${SHELL:-/bin/sh})
}

get_keys() {
  aws_key=$(awk -F'=' '/^access_key/ {gsub(/^[ \t]+/, "", $2); print $2}' "$s3cfg")
  aws_secret=$(awk -F'=' '/^secret_key/ {gsub(/^[ \t]+/, "", $2); print $2}' "$s3cfg")
}

export_keys() {
  get_keys
  get_shell
  if [ "${shell}" = 'fish' ]; then
    printf 'set -e %s; set -x -U %s %s\n' "$key" "$key" "$aws_key"
    printf 'set -e %s; set -x -U %s %s\n' "$secret" "$secret" "$aws_secret"
  else
    printf 'export %s=%s\n' "$key" "$aws_key"
    printf 'export %s=%s\n' "$secret" "$aws_secret"
  fi
}

check_s3cfg() {
  if [ ! -f "$s3cfg" ]; then
    err $s3cfg 'not found'
    exit 78
  fi
}

main() {
  check_s3cfg
  export_keys
}

main "$@"