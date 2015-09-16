# Cloning

It's probably best to just do a shallow clone of this repo, there are
probably a few dozen jpgs in the history :)

# Filename conventions

* rfc-3339 date spec used for file/folder names
* `{edit,bin,raw}` for each photo project

        Pictures/
        ├── lib
        │    └── basic-bash-scripts.sh
        └── 2015
            └── 2015-08-14_Project-name
                ├── bin
                │   └── convert-and-resize.sh
                ├── edit
                │   ├── 2015-08-14_Project-name_00001.jpg
                │   └── 2015-08-14_Project-name_00002.jpg
                └── raw
                    └── 2015-08-14_Project-name_00001.NEF

It's human readable and `find` works nicely

    find ~/Pictures -type d -iname 'project-name' -print0 | xargs -0 -I{} touch {}/edit/test
    find ~/Pictures -type f -iname 'project-name*.jpg' -print0

# Git Annex Info

## All the info about this repo:

    git annex info

## Grab this repo

* Put creds out into the environment

        eval $(~/.amazonrc)
        gpg-agent

* cd into repo:

        cd /home/tyler/Pictures

* Enable remote and get files

        git annex enableremote tylercipriani-raw
        git annex get [filename]

## Setup new repo

* Put creds out into the environment

        eval $(~/.amazonrc)
        gpg-agent

* cd into repo:

        cd /home/tyler/Pictures

* init git:

        git init

* init git annex:

        git annex init [annex name]

* Add S3 Remote named public-s3

        git annex initremote public-s3 type=S3 encryption=none bucket=tyler.zone chunk=0

* Add files

        git annex add [big-file]
        git annex copy --to=public-s3

* Commit repo

        git -a -m 'initial commit'

* Make available to the public (after public s3 setup):

        git annex addurl --file [filename] "http://tyler.zone/$(git annex lookupkey [filename])"
