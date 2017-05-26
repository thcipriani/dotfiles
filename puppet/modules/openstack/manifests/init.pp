class openstack {
  package { 'python-novaclient':
    provider => 'pip',
    ensure   => present,
  }
}
