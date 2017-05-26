class haskell {
  case $::operatingsystem {
    'Ubuntu': { include ::haskell::ubuntu }
    'Debian': { include ::haskell::debian }
  }
}
