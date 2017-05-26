# == haskell for ubuntu
#
class haskell::ubuntu {
  package { 'software-properties-common':
    require => Exec['apt-update'],
  }

  exec { 'add-hvr-ppa':
    command => 'apt-add-repository -y ppa:hvr/ghc && apt-get update',
    require => Package['software-properties-common'],
  }

  package {[
    'cabal-install-1.22',
    'ghc-7.8.4',
    'happy-1.19.4',
    'alex-3.1.3',]:
    require => Exec['add-hvr-ppa'],
  }

  file { '/etc/profile.d/haskell.sh':
    source => 'puppet:///modules/haskell/haskell.sh',
  }

  exec { "update-cabal":
    command => "/opt/cabal/1.22/bin/cabal update",
    environment => "HOME=/root",
    path => [
      '/opt/cabal/1.22/bin',
      '/opt/ghc/7.8.4/bin',
      '/opt/happy/1.19.4/bin',
      '/opt/alex/3.1.3/bin',
      '/bin/',
      '/sbin/' ,
      '/usr/bin/',
      '/usr/sbin/',
      '/usr/local/bin',
    ],
    unless  => "test -f /root/.cabal/packages/hackage.haskell.org/00-index.tar.gz",
    require => Package['cabal-install-1.22'],
  }

}

define haskell::install (
  $pkg = $title
) {
  exec { "cabal-$name":
    command => "cabal install --global ${pkg}",
    environment => "HOME=/root",
    path => [
      '/opt/cabal/1.22/bin',
      '/opt/ghc/7.8.4/bin',
      '/opt/happy/1.19.4/bin',
      '/opt/alex/3.1.3/bin',
      '/bin/',
      '/sbin/' ,
      '/usr/bin/',
      '/usr/sbin/',
      '/usr/local/bin',
    ],
    timeout => "0",
    require => Exec["update-cabal"],
    onlyif  =>
      "test -z \"$(/opt/ghc/7.8.4/bin/ghc-pkg list --simple-output ${pkg})\"";
  }
}