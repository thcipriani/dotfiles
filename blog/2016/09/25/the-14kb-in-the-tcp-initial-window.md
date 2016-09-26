[[!meta title="The 14KB in the TCP Initial Window"]]
[[!meta date="2016-09-25T20:07:53,391867327-07:00"]]

[[!tag tcp computing til]]

Today I went to coffee with a buddy of mine from `$DAY_JOB - 2`. During coffee
we were discussing all the folk theories of "fast" websites. I made
some off-the-cuff remark that I didn't really understand – that you
should try to squeeze the most important bits of your website into the first
_X_ bytes (_X_ being a value I didn't remember) for an opaque reason that has
to do with TCP congestion algorithms that I've never understood.

We spent a good chunk of time looking at Wireshark captures of an HTTP
request. It got muddled and we got off-track because we were looking at
an HTTPS request and we started talking about TLS1.2, CLIENT-HELLO, and
the TLS handshake in general.

I eventually found the
[source](https://developers.google.com/speed/docs/insights/mobile) for what I
was yammering about.

> Due to how TCP estimates the capacity of a connection (i.e. TCP Slow
> Start), a new TCP connection cannot immediately use the full available
> bandwidth between the client and the server. Because of this, the
> server can send up to 10 TCP packets on a new connection (~14KB) in
> first roundtrip, and then it must wait for client to acknowledge this
> data before it can grow its congestion window and proceed to deliver
> more data.

After I got home and ate some ice cream, I did some more reading trying
to understand this stuff. I think a lot of what was confusing about the
Wireshark stuff we were looking at is it also had TLS1.2 mixed in there.
Requesting a plain-old HTTP, port 80 site made looking at packets a
bit more sane.

It seems like the size of a TCP packet is negotiated during the `SYN`,
`SYN-ACK` phase of the TCP [three-way handshake](http://www.tcpipguide.com/free/t_TCPConnectionEstablishmentProcessTheThreeWayHandsh-3.htm).
In the `Options` section of the TCP segment (for a segment that has the
`SYN`-flag set in the TCP segment header) a sender or receiver may indicate a Max
Segment Size (MSS). An MSS is derived from a machine's Maximum Transmission
Unit (MTU). In the case of my machine the MTU is set to 1500. I found this via
the `ip` command:

```{.bash .sourceCode}
tyler@magneto:~$ ip -4 link show dev wlp3s0
2: wlp3s0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc mq state UP mode DORMANT group default qlen 1000
```

The MSS is derived from the MTU and is (at most?) the MTU minus the 40
bytes used for the TCP header and the IP header, so my machine sent the
MSS size of 1460 to the server in the `Options` section of the SYN TCP
segment that it initially sent my webserver.

![Client MSS](/static/images/2016/mss.png)

So the data portion of a TCP segment sent in this particular connection
should never exceed 1460 bytes.

While we were looking at Wireshark packet captures we kept noticing a
TCP segment size of 1486 in the "length" column with a TCP segment data
length of 1432. I noticed that my webserver sent an MSS size of 1432,
which I suppose became the limiting factor in how much data could be
sent per segment (rather than the 1460 MSS sent by my client – the
browser):

![Server MSS](/static/images/2016/server-mss.png)

Each TCP segment had a header-size of 20 bytes, so that leaves 34
unexplained bytes which (I guess) were part of the IP header.

So why try to squeeze a website into ~14KB? Seems like you should be
trying to squeeze it into 1432 bytes.

I found my answer in [IETF RFC 6928](https://tools.ietf.org/html/rfc6928) – a proposal to increase the
TCP Initial Window to 10 segments. The [Initial Congestion Window](https://en.wikipedia.org/wiki/Congestion_window#Congestion_window)
(`cwnd`) is the maximum number of bytes that a server can send without
receiving any acknowledgement that the bytes were received via a client
ACK packet. This maximum number of bytes to send is calculated by
multiplying _some number_ by MSS. As of RFC 6928 _some number_ is equal
to 10 (initially).

I checked the value of `cwnd` on some established connections on my
server:

```{.bash .sourceCode}
tyler@magneto:~$ sudo ss -ti
    ...cwnd:10...
```

And it is indeed set to 10.

This means that my server can send `cwnd` * MSS bytes of information
before the client has to acknowledge that anything has been receieved.
So, in the case of the Wireshark connection I recorded, my webserver can
send 1432 * 10 bytes, or 14320 bytes:

```{.bash .sourceCode}
tyler@magneto:~$ units
Currency exchange rates from www.timegenie.com on 2016-06-22
202926 units, 109 prefixes, 88 nonlinear units

You have: 14320 bytes
You want: kilobytes
        * 14.32
        / 0.069832402
```

*OR* around 14KB of data can be sent to a browser before another round-trip
from the client to the webserver has to happen.

So, overall, a nice, relaxing coffee and catch-up session with an old coworker.
