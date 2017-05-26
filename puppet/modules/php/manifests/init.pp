class php {
  include ::php::params

  package {[
    $::php::params::php_package_name,
    $::php::params::php_cli_package_name,
  ]:
    ensure => present
  }
}
