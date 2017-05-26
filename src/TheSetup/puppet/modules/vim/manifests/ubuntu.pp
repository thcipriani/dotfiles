class vim {
  package {[
    'vim',
    'vim-runtime',
    'vim-gnome',
    'vim-tiny',
    'vim-common',
    'vim-gui-common']:
    ensure  => purged,
    require => Exec['apt-get update'],
  }

  package {[
    'liblua5.1-dev',
    'luajit',
    'libluajit-5.1',
    'python-dev',
    'ruby-dev',
    'libperl-dev',
    'mercurial',
    'libncurses5-dev',
    'libgnome2-dev',
    'libgnomeui-dev',
    'libgtk2.0-dev',
    'libatk1.0-dev',
    'libbonoboui2-dev',
    'libcairo2-dev',
    'libx11-dev',
    'libxpm-dev',
    'libxt-dev']:
    ensure  => installed,
    require => Exec['apt-update'],
  }

  exec { 'check-vim-source':
    command => '/usr/bin/env hg clone https://code.google.com/p/vim/ /usr/local/src/vim',
    creates => '/usr/local/src/vim',
    require => Package['mercurial'],
    before  => Exec['configure-vim'],
  }

  exec { 'get-lua-for-vim':
    command => 'mkdir /usr/include/lua5.1/include/ && \
      cp /usr/include/lua5.1/* /usr/include/lua5.1/include/ && \
      ln -s /usr/lib/x86_64-linux-gnu/liblua5.1.so /usr/local/lib/liblua.so',
    creates => '/usr/include/lua5.1/include',
    before  => Exec['configure-vim'],
  }

  exec { 'configure-vim':
    command => template('vim/configure.erb'),
    cwd     => '/usr/local/src/vim',
    creates => '/usr/local/bin/vim',
  }

}
