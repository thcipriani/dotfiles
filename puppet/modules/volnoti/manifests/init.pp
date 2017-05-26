# == volnoti
#
# shows your volume real nice-like
#
class volnoti {
    package { [
        'libdbus-1-dev',
        'libdbus-glib-1-dev',
        'libgtk2.0-dev',
    ]
        : }
    exec { 'download-volnoti':
        command => 'wget -q -O /usr/local/src/volnoti-0.1.tar.gz http://github.com/downloads/davidbrazdil/volnoti/volnoti-0.1.tar.gz',
        creates => '/usr/local/src/volnoti-0.1.tar.gz'
    }

    exec { 'unpack-volnoti':
        command => 'tar xvzf volnoti-0.1.tar.gz',
        cwd     => '/usr/local/src',
        creates => '/usr/local/src/volnoti-0.1',
        require => Exec['download-volnoti'],
    }

    exec { 'install-volnoti':
        command => '/usr/local/src/volnoti-0.1/configure --prefix=/usr && make && make install',
        creates => '/usr/bin/volnoti',
        cwd     => '/usr/local/src/volnoti-0.1',
        require => Exec['unpack-volnoti'],
    }
}
