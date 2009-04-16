
# XMPP

A basic module for IM using the Jabber/XMPP protocol with PLT Scheme.

## Protocol Support

Should eventually implement XMPP-Core and XMPP-IM to conform with RFCs
3920 and 3921. Progress toward supporting the full protocol is
currently documented in the file 'xmpp.ss'

## Installation

    (require (planet zzkt/xmpp:1:0/xmpp))


## Session

It is necessary to establish a session with a Jabber server before
sending any messages or presence updates. This can be done manually,
or with the help of with-xmpp-session.


## Sending

Once a session is established, the 'send' function can be used to send
messages, presnece updates or queries.

    (with-xmpp-session jid pass 
      (send (message user@host "some random message")))    		       

Where 'jid' is the senders jid and 'pass' is the password


## Response Handlers 

A handler can be registered to repsond to 'message 'presence 'iq or
'other stanzas. Note that an 'iq handler will revive any error
messages from the server

    (set-xmpp-handler 'message print-message)
   

## Example Chat Client

    (require xmpp)

    (define (read-input prompt)
      (display prompt)
      (read-line (current-input-port)))

    (define (chat)
      (let ((jid  (read-input "jid: "))
            (pass (read-input "password: "))
            (to   (read-input "chat with: ")))
        (with-xmpp-session jid pass 
                           (set-xmpp-handler 'message print-message)
                           (let loop ()                         
                             (let ((msg (read-line (current-input-port))))
                               (send (message to msg))
                               (loop))))))


## possiby interesting extensions to implement. 

see http://xmpp.org/extensions/

* XEP-0047: In-Band Bytestreams
* XEP-0066: Out of Band Data
* XEP-0030: Service Discovery
* XEP-0060: Publish-Subscribe
* XEP-0045: Multi-User Chat
* XEP-0149: Time Periods
* XEP-0166: Jingle
* XEP-0174: Serverless Messaging
* XEP-0199: XMPP Ping
* XEP-0224: Attention
* XEP-0077: In-Band Registration

    
