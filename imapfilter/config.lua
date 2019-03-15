options.timeout = 120
options.create = true
options.subscribe = true
options.expunge = true

-- Used to grab values from my ~/.netrc
function getnetrc (key)
    if key ~= 'login' then
        key = ''
    end
    status, value = pipe_from('~/bin/getnetrc offlineimap ' .. key)
    value = string.gsub(value, ' ', '')
    value = string.gsub(value, '\n', '')
    return value
end

-- Check if mailbox exists
-- <https://github.com/lefcha/imapfilter/issues/37>
function mailbox_exists(account, mailbox)
    local result = account:list_all('', mailbox)
    if #result > 0 then return true else return false end
end

-- Create mailbox if it doesn't exist
function mailbox_maybe(account, mailbox)
    if mailbox_exists(account, mailbox) then
        print(string.format('Mailbox %q already created...', mailbox))
    else
        print(string.format('Creating mailbox %q', mailbox))
        account:create_mailbox(mailbox)
    end
end

account = IMAP {
    server = 'imap.gmail.com',
    username = getnetrc('login'),
    password = getnetrc(),
    ssl = 'tls1',
}

inbox = account['INBOX']

mailbox_maybe(account, 'ML')
mailbox_maybe(account, 'ML/wmfall')
mailbox_maybe(account, 'ML/wikitech-l')
mailbox_maybe(account, 'ML/mediawiki-l')
mailbox_maybe(account, 'ML/ops')
mailbox_maybe(account, 'ML/debian-user')
mailbox_maybe(account, 'ML/debian-devel')
mailbox_maybe(account, 'ML/debian-security')
mailbox_maybe(account, 'ML/debian-bugs')
mailbox_maybe(account, 'ML/coreboot')
mailbox_maybe(account, 'ML/github')
mailbox_maybe(account, 'ML/unicode')
mailbox_maybe(account, 'CR')
mailbox_maybe(account, 'auto')
mailbox_maybe(account, 'pipelinebot')
mailbox_maybe(account, 'task')

-- results = inbox:contain_to('wmfall@lists.wikimedia.org')
-- results:move_messages(account['ML/wmfall'])

results = inbox:contain_to('foundation-optional@wikimedia.org')
results:move_messages(account['ML/wmfall'])

results = inbox:contain_to('wikitech-l@lists.wikimedia.org')
results:move_messages(account['ML/wikitech-l'])

results = inbox:contain_to('mediawiki-l@lists.wikimedia.org')
results:move_messages(account['ML/mediawiki-l'])

-- Ops is fairly low traffic, let it hit the inbox for a bit
-- results = inbox:contain_to('ops@lists.wikimedia.org')
-- results:move_messages(account['ML/ops'])

results = inbox:contain_to('debian-user@lists.debian.org')
results:move_messages(account['ML/debian-user'])

results = inbox:contain_to('*@bugs.debian.org')
results:move_messages(account['ML/debian-bugs'])

results = inbox:contain_to('debian-devel@lists.debian.org')
results:move_messages(account['ML/debian-devel'])

results = inbox:contain_to('debian-security-announce@lists.debian.org')
results:move_messages(account['ML/debian-security'])

results = inbox:contain_to('coreboot@coreboot.org')
results:move_messages(account['ML/coreboot'])

results = inbox:contain_to('unicode@unicode.org')
results:move_messages(account['ML/unicode'])

-- Gerrit
results = inbox:contain_from('gerrit@wikimedia.org')
-- results:mark_flagged()
-- results:move_messages(account['CR'])

-- Github
results = inbox:contain_cc('subscribed@noreply.github.com')
results = inbox:contain_cc('push@noreply.github.com')
results:move_messages(account['ML/github'])

-- Phabricator-mail-tags that contain differential
-- X-Phabricator-Mail-Tags: <differential-review-request>, <differential-other>, <differential-reviewers>
-- results = inbox:contain_field('X-Phabricator-Mail-Tags', '<differential-review-request>')
-- results = inbox:match_header('.*X-Phabricator-Mail-Tags: <differential-review-request>.*')
-- results = inbox:contain_subject('[Differential]')
-- results:mark_flagged()
-- results:move_messages(account['CR'])

results = inbox:contain_from('jenkins-bot@wikimedia.org')
results:move_messages(account['auto'])
results = inbox:contain_to('betacluster-alerts@lists.wikimedia.org')
results:move_messages(account['auto'])
results = inbox:contain_from('git@palladium.eqiad.wmnet')
results:move_messages(account['auto'])
results = inbox:contain_subject('customchannels@ccubuntu')
results:move_messages(account['auto'])

-- results = inbox:contain_subject('[Maniphest]')
-- results:mark_flagged()
-- results:move_messages(account['task'])

results = inbox:contain_to('tcipriani+pipelinebot@wikimedia.org')
results:move_messages(account['pipelinebot'])
