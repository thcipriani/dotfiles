# == class google
#
# setup google apt-key for repos
#
class google {
    exec { 'add-google-key':
        command => 'wget -q -O - https://dl.google.com/linux/linux_signing_key.pub | apt-key add -',
        unless  => 'apt-key list | grep -q google',
    }
}
