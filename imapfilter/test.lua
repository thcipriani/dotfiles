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

results = inbox:contain_to('tcipriani+pipelinebot@wikimedia.org')
-- results = inbox:contain_cc('subscribed@noreply.github.com')
-- results = results:match_field('X-GitHub-Reason', 'subscribed')

print(#results)
print(results.n)
print("Table:", table.unpack(results))

-- mailbox_maybe(account, 'ML')
-- mailbox_maybe(account, 'ML/debian-devel')
-- mailbox_maybe(account, 'ML/debian-user')
