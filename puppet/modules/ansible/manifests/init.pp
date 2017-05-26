# == class ansible
#
# That's right, a puppet class to setup ansible. Don't you dare fucking judge
# me!
class ansible {

    include python

    package { 'ansible':
        provider => 'pip',
        require  => Exec['easy_install pip'],
    }
}
