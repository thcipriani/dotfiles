Exec {
    path => [
        '/bin/',
        '/sbin/' ,
        '/usr/bin/',
        '/usr/sbin/',
        '/usr/local/bin',
    ], }
Package { ensure => present, }

include ::stdlib
hiera_include('classes', [])
