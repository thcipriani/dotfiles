#!/bin/sh

distro=''
script=''
path=$(cd $(dirname "$0") && pwd)

fatal() {
  printf "[ ERR ] %s\n" "$@"
  exit 1
}

if [ ! -d "${path}/bootstrap" ]; then
  fatal "Try: `cd ${path} && git init submodules --update --recursive` then re-run"
  exit 1
fi

if [ "$(uname -s)" = 'Darwin' ]; then
  distro='mac_os_x'
elif [ -f /etc/lsb-release ]; then
  . /etc/lsb-release
  distro="$(echo "${DISTRIB_ID}" | tr [:upper:] [:lower:])"
elif [ -x /usr/bin/lsb_release ]; then
  distro=$(lsb_release -a | grep '^Distributor ID:' | cut -f2 | tr [:upper:] [:lower:])
fi

script="${path}/bootstrap/${distro}.sh"

if [ ! -f "$script" ]; then
  fatal "Cannot find bootstrap script for ${distro}"
fi

printf "Ready to install puppet...maybe [y/N] "
read ans

if ! echo $ans | grep -iq ^y; then
  fatal Cancelled
fi

sudo su - -c "chmod +x ${script}"
sudo su - -c "${script}"
sudo su - -c "chmod -x ${script}"
