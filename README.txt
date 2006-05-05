Jabberlang
==========

Jabberlang is an Erlang XMPP (Jabber) client library. It only supports for now basic XMPP packets.
The scope of this library is currently progressively being extended to support an increasing part of the XMPP (and thus Jabber) protocol.


Requirements
============

You need:
- Erlang/OTP
- The CVS version of ejabberd



Usage
=====

Compile with
erl -pa ebin -make


./launch
(You can modify the launch script to fix the paths to fit your needs)

Erlang (BEAM) emulator version 5.3.6.3 [source] [hipe] [threads:0]

Eshell V5.3.6.3  (abort with ^G)
1> {ok, XMPP} = xmpp:start().
{ok,<0.31.0>}
2> xmpp:set_login_information(XMPP, "mremond", {password,"mypassword"}).
2> ".
** 2: syntax error before: '.' **
2> xmpp:set_login_information(XMPP, "mremond", {password,"mypassword"}).
ok
3> xmpp:connect(XMPP).
ok
4>
=INFO REPORT==== 17-Oct-2004::22:51:04 ===
I(<0.31.0>:xmpp:174): Connected to localhost:5222


=INFO REPORT==== 17-Oct-2004::22:51:04 ===
I(<0.31.0>:xmpp:241): Authentication successfull. You are logged in as "mremond"


4>
=INFO REPORT==== 17-Oct-2004::22:51:29 ===
I(<0.31.0>:xmpp_callbacks:13): Presence <0.31.0>: available ([{"from",
                                                               "mremond5@localhost/tkabber"},
                                                              {"to",
                                                               "mremond@localhost/Jabberlang"},
                                                              {"xml:lang",
                                                               "fr-FR"}]) ([{xmlelement,
                                                                                "priority",
                                                                                [],
                                                                                [{xmlcdata,
                                                                                     "8"}]}])

=INFO REPORT==== 17-Oct-2004::22:51:30 ===
I(<0.31.0>:xmpp_callbacks:13): Presence <0.31.0>: available ([{"from",
                                                               "mremond5@localhost/tkabber"},
                                                              {"to",
                                                               "mremond@localhost/Jabberlang"},
                                                              {"xml:lang",
                                                               "fr-FR"}]) ([{xmlelement,
                                                                                "priority",
                                                                                [],
                                                                                [{xmlcdata,
                                                                                     "8"}]}])

