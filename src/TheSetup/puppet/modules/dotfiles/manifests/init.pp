# == class dotfiles
#
# Cheks out your dotfiles into your home directory
#
class dotfiles(
  $dotfile_repo,
  $emacs_repo,
  $homedir,
  $owner,
  $group,
) {

  ::git::clone { 'dotfiles':
    directory => "${homedir}/.dotfiles",
    remote    => $dotfile_repo,
    owner     => $owner,
    group     => $group,
    require   => User[$owner],
  }

  ::git::clone { 'dotemacs':
    directory => "${homedir}/.emacs.d",
    remote    => $emacs_repo,
    owner     => $owner,
    group     => $group,
    require   => User[$owner],
  }

  exec { 'setup-dotfiles':
    command     => 'ruby bootstrap.rb',
    environment => [ "HOME=${homedir}" ],
    cwd         => "/home/${owner}/.dotfiles",
    unless      => "test -L ${homedir}/.bashrc",
    require     => Exec['git_clone_dotfiles'],
  }

  exec { "${owner}-owner":
    command => "chown -R ${owner}:${group} ${homedir}",
    unless  => "test $(stat -c '%U' \"${homedir}\") = ${owner}",
    require => [
      Exec['setup-dotfiles'],
    ],
  }
}
