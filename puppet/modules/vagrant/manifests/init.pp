class vagrant {
  case $::operatingsystem {
    'Ubuntu': { include ::vagrant::ubuntu }
    'Debian': { include ::vagrant::debian }
    'default': {
      package { 'vagrant': }
    }
  }
}
