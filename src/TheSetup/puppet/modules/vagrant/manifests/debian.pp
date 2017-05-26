class vagrant::debian {
    package { [
        'vagrant',
        'virtualbox-5.1',
    ]:
        require => [
            Exec['linux-headers'],
            Exec['virtualbox-apt'],
        ]
    }

    exec { 'virtualbox-key':
        command => 'wget -q https://www.virtualbox.org/download/oracle_vbox.asc -O- | apt-key add - && wget -q https://www.virtualbox.org/download/oracle_vbox_2016.asc -O- | apt-key add -',
        unless  => 'apt-key list | grep -q virtualbox',
    }

    exec { 'virtualbox-apt':
        command => 'echo "deb http://download.virtualbox.org/virtualbox/debian jessie contrib" > /etc/apt/sources.list.d/virtualbox.list',
        creates => '/etc/apt/sources.list.d/virtualbox.list',
        notify  => Exec['apt-get update'],
        require => Exec['virtualbox-key'],
    }

    exec { 'linux-headers':
        command => 'apt-get install linux-headers-$(uname -r|sed "s,[^-]*-[^-]*-,,")',
        unless  => 'dpkg -l linux-headers-$(uname -r|sed "s,[^-]*-[^-]*-,,")',
    }
}
