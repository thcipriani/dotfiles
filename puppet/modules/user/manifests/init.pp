class user(
  $user,
  $group,
  $pass,
  $sshkey,
  $sudo = false,
  $sshkeytype = 'ssh-rsa',
) {
  $groups = $sudo ? {
      default => $group,
      true => [
        $group,
        'sudo',
      ]
  }

  user { "${user}":
    ensure     => present,
    shell      => '/bin/bash',
    home       => "/home/${user}",
    groups     => $groups,
    password   => $pass,
    managehome => true,
  }

  ssh_authorized_key { $user:
    user    => $user,
    type    => $sshkeytype,
    key     => $sshkey,
    require => User[$user],
  }

  exec { 'sshd_fixes':
    command => 'printf "PermitRootLogin no\nPasswordAuthentication no\n" >> /etc/ssh/sshd_config',
    unless  => 'grep -Fq "PermitRootLogin no" /etc/ssh/sshd_config',
    require => Package['openssh-server'],
  }
}
