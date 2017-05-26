# == python setup
#
class python {
    package{ [
        'python-setuptools',
        'python-dev',
        'build-essential',
    ]: }

    exec { 'easy_install pip':
        require => Package['python-setuptools'],
        unless  => 'test -x /usr/local/bin/pip',
    }

    package{ [
        'virtualenv',
        's3cmd',
    ] :
        provider => pip,
        require  => Exec['easy_install pip'],
    }
}
