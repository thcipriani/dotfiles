class vim {
  case $::operatingsystem {
    'Ubuntu': { include ::vim::ubuntu }
    'Debian': { include ::vim::debian }
  }
}
