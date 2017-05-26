# == Class jenkins-job-builder
#
class jjb {
  ::git::clone { 'jjb':
    directory => '/usr/local/src/jjb',
    remote    => "https://gerrit.wikimedia.org/r/p/integration/jenkins-job-builder.git",
    ensure    => 'present',
    owner     => 'root',
    group     => 'root',
  }

  exec { 'pip-jjb':
    command => 'pip install -e .',
    cwd     => '/usr/local/src/jjb',
    unless  => 'pip list | grep -q jenkins',
    require   => [
      Exec['git_clone_jjb'],
      Package['python-pip'],
      Package['python-dev'],
    ],
  }

}
