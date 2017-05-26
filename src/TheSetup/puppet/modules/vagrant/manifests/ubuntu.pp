class vagrant::ubuntu(
  $version = '',
  $arch = '',
) {

  $sysarch = $arch ? {
    undef   => $::hardwaremodel,
    default => $arch,
  }

  exec { 'download-vagrant':
    command => "wget -q https://dl.bintray.com/mitchellh/vagrant/vagrant_${version}_${sysarch}.deb",
    cwd     => '/usr/local/src',
    creates => "/usr/local/src/vagrant_${version}_${sysarch}.deb",
  }

  exec { 'install-vagrant':
    command => "dpkg -i vagrant_${version}_${sysarch}.deb",
    cwd     => '/usr/local/src',
    unless  => 'dpkg -l | grep -q vagrant',
    require => Exec['download-vagrant'],
  }
}
