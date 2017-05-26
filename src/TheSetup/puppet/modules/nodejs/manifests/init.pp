# == class node
#
# Installs nodejs on my system
class nodejs {
    # Update this whole mess via
    #  sudo npm cache clean -f \
    #      && sudo npm install -g n \
    #      && sudo n stable \
    #      && npm install -g npm
    package { [
        'nodejs',
        'npm',
    ]: }
}