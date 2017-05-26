class docker::jessie {
    include backports::jessie

    package { 'docker.io':
        require => Exec['apt-get update'],
    }
}
