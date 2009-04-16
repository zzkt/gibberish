#lang scribble/doc
@(require scribble/manual)

@title{XMPP}

A module for using the Jabber/XMPP protocol.

@table-of-contents[]

@section{Protocol Support}

A minimal subset of the XMPP protocols are supported, but not much
beyond sending and receiving messages and presence updates. This
module should eventually implement XMPP-Core and XMPP-IM to conform
with RFCs 3920 and 3921. Currently, the default connection uses 'old
style' SSL, which is deprecated and may cause problems with some
servers. Progress toward supporting the full protocol is documented in
the file 'xmpp.ss'


@section{Session}

It is necessary to establish a session with a Jabber server before
sending any messages or presence updates. This can be done manually,
or with the help of with-xmpp-session.

@defform[(with-xmpp-seesion [jid jid?] [password string?] body)]{
  
   Establishes an XMPP session using the id @scheme[jid] and password
   @scheme[pass] and evaluates the forms in @scheme[body] in the
   session's scope.}
                    
                 
@section{Sending}

Once a session is established, the 'send' function can be used to send
messages, presence updates or queries. 

@section{Messages}

To send a message containing @scheme[text] to a user with the
@scheme[jid] of @scheme[to]. 
                                                               
@schemeblock[
(with-xmpp-session jid pass
  (send (message to text)))
]

@section{Presence}

@schemeblock[					
(with-xmpp-session jid pass
  (send (presence)))
]

@schemeblock[
(with-xmpp-session jid pass
  (send (presence #:status "Available")))
]

@section{Response Handling}

@schemeblock[
(with-xmpp-session jid pass 
     (set-xmpp-handler 'message print-message))
]
 
@section{Example Chat Client}

@schemeblock[
(define (read-input prompt)
  (display prompt)
  (read-line (current-input-port)))

(define (chat)
  (let ((jid  (read-input "jid: "))
        (pass (read-input "password: "))
        (to   (read-input "chat with: ")))
    (with-xmpp-session
     jid pass 
     (set-xmpp-handler 'message print-message)
     (let loop ()                         
       (let ((msg (read-line (current-input-port))))
         (send (message to msg))
         (loop))))))

             ]

and chat away...
