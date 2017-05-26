class phpsh {
  include ::php

  package{ 'phpsh':
    require => Package[$::php::params::php_package_name],
    provider => 'pip',
  }
}
