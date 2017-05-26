# == Define: git::clone
#
# Custom resource for cloning a remote git repository.
#
# === Parameters
#
# [*directory*]
#   Name for target directory for repository content. It should not
#   refer to an existing directory.
#
# [*branch*]
#   Name of branch to check out. Defaults to checking out the HEAD of the
#   remote repository.
#
# [*remote*]
#   Remote URL for the repository. If unspecified, the resource title
#   will be interpolated into $git::urlformat.
#
# [*owner*]
#   User that should own the checked out repository. Git commands will run as
#   this user so the user must have the ability to create the target
#   directory. Default 'vagrant'.
#
# [*group*]
#   Group that should own the checked out repostory. Default 'vagrant'.
#
# [*ensure*]
#   What state the clone should be in. Valid values are `present` and
#   `latest`. Default 'present'.
#
# [*depth*]
#   If specified, creates a shallow clone with history truncated to the
#   specified number of revisions. Default undef.
#
# [*recurse_submodules*]
#   After the clone is created, initialize all submodules within, using their
#   default settings. Default true.
#
# === Examples
#
#  Clone VisualEditor to MediaWiki extension path:
#
#  git::clone { 'extensions/VisualEditor':
#      directory => '/vagrant/mediawiki/extensions/VisualEditor',
#  }
#
# Stolen verbatim from: 
define git::clone(
    $directory,
    $branch             = undef,
    $remote             = undef,
    $owner              = $::user::name,
    $group              = $::user::group,
    $ensure             = 'present',
    $recurse_submodules = true,
) {
    require ::git

    if !($ensure in ['present', 'latest']) {
        fail('ensure parameter must be present or latest.')
    }

    $repository = $remote ? {
        undef   => sprintf($git::urlformat, $title),
        default => $remote,
    }

    $arg_branch = $branch ? {
        undef   => '',
        default => "--branch '${branch}'"
    }
    $arg_recurse = $recurse_submodules ? {
        true    => '--recurse-submodules',
        default => '',
    }

    exec { "git_clone_${title}":
        command     => "git clone ${arg_recurse} ${arg_branch} ${repository} ${directory}",
        cwd         => '/',
        creates     => "${directory}/.git",
        user        => $owner,
        group       => $group,
        require     => Package[$::git::package],
        timeout     => 0,
    }

    if $ensure == 'latest' {
        exec { "git_pull_${title}":
            command  => "git pull ${arg_recurse} ${arg_depth}",
            unless   => "git fetch ${arg_depth} && git diff --quiet @{upstream}",
            cwd      => $directory,
            user     => $owner,
            group    => $group,
            schedule => 'hourly',
            require  => Exec["git_clone_${title}"],
        }

        if $recurse_submodules {
            exec { "git_submodule_update_${title}":
                command     => 'git submodule update --init --recursive',
                cwd         => $directory,
                user        => $owner,
                group       => $group,
                refreshonly => true,
                subscribe   => Exec["git_pull_${title}"],
            }
        }
    }
}
