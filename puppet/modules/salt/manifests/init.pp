class salt(
    $master = 'salt',
) {
    package{'salt-minion':}

    file{'/etc/salt/minion':
        content => template('salt/minion.erb'),
        owner   => 'root',
        group   => 'root',
        mode    => '0644',
    }

    service{'salt-minion':
        ensure    => running,
        require   => Package['salt-minion'],
        subscribe => File['/etc/salt/minion'],
    }
}