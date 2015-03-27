# All my photos on git annex/s3

## Grab this repo

* Put creds out into the environment

    eval (~/.amazonrc)
    gpg-agent

* cd into repo:

    cd /home/tyler/Pictures

* Enable remote and get files

    git annex enableremote tylercipriani-raw
    git annex get [filename]

## Setup new repo

* Put creds out into the environment

    eval (~/.amazonrc)
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

    git annex [big-file] --copy-to public-s3
    # -- OR --
    git annex copy --to public-s3

* Commit repo

    git -a -m 'initial commit'

* Make available to the public (after public s3 setup):

    git annex addurl --file [filename] "http://tyler.zone/$(git annex lookupkey [filename])"


