# == vim for debian
#
# Only tested on jessie, and even then, barely
#
class vim::debian {
  package { 'vim-nox': }
}
