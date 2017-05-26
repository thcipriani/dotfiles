# == haskell for debian
#
# Only tested on jessie
#
class haskell::debian {

    package {[
        # Libraries needed for xmonad stuffs
        'libxrandr-dev',
        'libx11-dev',
        'zlib1g-dev',
        'libxft-dev',
        'libxpm-dev',

        'cabal-install',
        'ghc',
        'happy',
        'alex',
        'darcs',
    ]:
        require => Exec['apt-get update'],
    }

    ::haskell::install{ [
        'xmobar',
        'pandoc',
        'yeganesh',
    ]:
        require => Exec['apt-get update'],
    }

    exec { 'cabal update':
        unless      => 'test -f /root/.cabal/packages/hackage.haskell.org/00-index.tar.gz',
        environment => 'HOME=/root',
        require     => Package['cabal-install'],
    }

    exec { 'grab-latest-contrib':
        command => 'darcs get http://code.haskell.org/XMonadContrib',
        unless  => 'test -d /root/XMonadContrib',
        cwd     => '/root',
        require => Package['darcs'],
    }

    exec { 'grab-latest-xmonad':
        command => 'darcs get http://code.haskell.org/xmonad',
        cwd     => '/root',
        unless  => 'test -d /root/xmonad',
        require => Package['darcs'],
    }

    exec { 'xmonad-and-xmonad-contrib':
        command => 'cabal install --global /root/xmonad /root/XMonadContrib',
        onlyif  => 'test -z "$(ghc-pkg list --simple-output xmonad-contrib)"',
        require => [
            Exec['grab-latest-xmonad'],
            Exec['grab-latest-contrib'],
            Exec['cabal update'],
        ],
    }

}

define haskell::install (
  $pkg = $title
) {
  exec { "cabal-${name}":
    command     => "cabal install --global ${pkg}",
    environment => 'HOME=/root',
    timeout     => 0,
    require     => Exec['cabal update'],
    unless      => "test -x /usr/local/bin/${pkg}";
  }
}
