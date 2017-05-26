class xsession {
  file { '/usr/share/xsessions/custom.desktop':
    source => 'puppet:///modules/xsession/custom.desktop',
  }
}
