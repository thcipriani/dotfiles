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

account:create_mailbox('ML')
account:create_mailbox('ML/wmfall')
results = inbox:contain_to('wmfall@lists.wikimedia.org')
results:move_messages(account['ML/wmfall'])

account:create_mailbox('ML/wikitech-l')
results = inbox:contain_to('wikitech-l@lists.wikimedia.org')
results:move_messages(account['ML/wikitech-l'])

account:create_mailbox('CR')
results = inbox:contain_from('gerrit@wikimedia.org')
results:move_messages(account['CR'])

account:create_mailbox('CI')
results = inbox:contain_from('jenkins-bot@wikimedia.org')
results:move_messages(account['CI'])
