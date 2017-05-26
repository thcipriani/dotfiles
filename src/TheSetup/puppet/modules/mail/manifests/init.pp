# == Class: mail
#
# Adds necessary packages and offlineimap systemd timer
#
class mail(
    $user,
    $group,
    $homedir,
    $systemd = 1,
) {
    include user

    package { [
        'mutt',
        'msmtp',
        'offlineimap',
    ]: }

    if $systemd == 1 {
        file { "${homedir}/Mail":
            ensure => directory,
            owner  => $user,
            group  => $group,
            mode   => '0755',
        }

        file { "${homedir}/Mail/Gmail":
            ensure => directory,
            owner  => $user,
            group  => $group,
            mode   => '0755',
        }

        file { "${homedir}/.config":
            ensure => directory,
            owner  => $user,
            group  => $group,
            mode   => '0755',
        }

        file { "${homedir}/.config/systemd":
            ensure => directory,
            owner  => $user,
            group  => $group,
            mode   => '0755',
        }

        file { "${homedir}/.config/systemd/user":
            ensure => directory,
            owner  => $user,
            group  => $group,
            mode   => '0755',
        }

        file { "${homedir}/.config/systemd/user/offlineimap.service":
            ensure => present,
            owner  => $user,
            group  => $group,
            mode   => '0444',
            source => 'puppet:///modules/mail/offlineimap.service',
        }

        file { "${homedir}/.config/systemd/user/offlineimap.timer":
            ensure => present,
            owner  => $user,
            group  => $group,
            mode   => '0444',
            source => 'puppet:///modules/mail/offlineimap.timer',
        }
    }

    User <| |> -> Class['mail']
}
