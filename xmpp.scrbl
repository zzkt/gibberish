#lang scribble/doc
@(require scribble/manual)

@title{XMPP}

A module for using the Jabber/XMPP protocol.

@table-of-contents[]

@section{Protocol Support}

It should eventually implement XMPP-Core and XMPP-IM to conform with
RFCs 3920 and 3921. Progress toward supporting the full protocol is
currently documented in the file 'xmpp.ss'


@section{Installation}

@schemeblock[(require (planet zzkt/xmpp:1:0/xmpp))]

@section{Session}

@schemeblock[
(with-xmpp-session jid pass body)
]

@section{Mesaging}

@schemeblock[
(with-xmpp-session jid pass
  (send (message to text)))
]

@section{Presence}

@schemeblock[
(with-xmpp-session jid pass
  (send (presence)))
]
		   
@section{Registration}

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
