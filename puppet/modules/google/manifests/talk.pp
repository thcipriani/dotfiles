# == class google::talk
#
# installs google talk
class google::talk {
    include google

    exec { 'talk-apt':
        command => 'echo "deb http://dl.google.com/linux/talkplugin/deb/ stable main" > /etc/apt/sources.list.d/google-talk.list',
        creates => '/etc/apt/sources.list.d/google-talk.list',
        notify  => Exec['apt-get update'],
    }

    package { 'google-talkplugin':
        require => [
            Exec['talk-apt'],
            Exec['apt-get update'],
        ]
    }

}
