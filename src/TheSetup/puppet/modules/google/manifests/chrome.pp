# == class google chrome
#
# installs google chrome
#
class google::chrome {
    include google

    exec { 'chrome-apt':
        command => 'echo "deb http://dl.google.com/linux/chrome/deb/ stable main" > /etc/apt/sources.list.d/google-chrome.list',
        creates => '/etc/apt/sources.list.d/google-chrome.list',
        notify  => Exec['apt-get update'],
    }

    package { 'google-chrome-stable':
        require => [
            Exec['chrome-apt'],
            Exec['apt-get update'],
        ],
    }

}
