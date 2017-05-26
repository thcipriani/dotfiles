class php::params {
  case $::osfamily {
    'Debian': {
      $php_package_name = 'php5'
      $php_cli_package_name = 'php5-cli'
    }
  }
}
