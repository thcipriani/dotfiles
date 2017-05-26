class backports::jessie {
    exec { 'debian-backports':
        command => 'echo "deb http://http.debian.net/debian jessie-backports main" > /etc/apt/sources.list.d/jessie-backports.list',
        creates => '/etc/apt/sources.list.d/jessie-backports.list',
        notify  => Exec['apt-get update'],
    }
}
