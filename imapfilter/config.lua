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

account = IMAP {
    server = 'imap.gmail.com',
    username = getnetrc('login'),
    password = getnetrc(),
    ssl = 'tls1',
}

inbox = account['INBOX']

-- account:create_mailbox('ML')
-- account:create_mailbox('ML/wmfall')
results = inbox:contain_to('wmfall@lists.wikimedia.org')
results:move_messages(account['ML/wmfall'])

-- account:create_mailbox('ML/wikitech-l')
results = inbox:contain_to('wikitech-l@lists.wikimedia.org')
results:move_messages(account['ML/wikitech-l'])

results = inbox:contain_to('ops@lists.wikimedia.org')
results:move_messages(account['ML/ops'])

-- account:create_mailbox('ML/debian-devel')
results = inbox:contain_to('debian-devel@lists.debian.org')
results:move_messages(account['ML/debian-devel'])

-- account:create_mailbox('CR')

-- Gerrit
results = inbox:contain_from('gerrit@wikimedia.org')
results:move_messages(account['CR'])

-- Phabricator-mail-tags that contain differential
-- X-Phabricator-Mail-Tags: <differential-review-request>, <differential-other>, <differential-reviewers>
-- results = inbox:contain_field('X-Phabricator-Mail-Tags', '<differential-review-request>')
-- results = inbox:match_header('.*X-Phabricator-Mail-Tags: <differential-review-request>.*')
results = inbox:contain_subject('[Differential]')
results:move_messages(account['CR'])

-- account:create_mailbox('auto')
results = inbox:contain_from('jenkins-bot@wikimedia.org')
results:move_messages(account['auto'])
results = inbox:contain_to('betacluster-alerts@lists.wikimedia.org')
results:move_messages(account['auto'])
results = inbox:contain_from('git@palladium.eqiad.wmnet')
results:move_messages(account['auto'])

-- account:create_mailbox('task')
results = inbox:contain_subject('[Maniphest]')
results:move_messages(account['task'])
