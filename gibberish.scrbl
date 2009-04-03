#lang scribble/doc
@(require scribble/manual)

@title{Gibberish}

Gibberish is a module for using the Jabber/XMPP protocol.

@table-of-contents[]

@section{Protocol Support}

It should eventually implement XMPP-Core and XMPP-IM to conform with
RFCs 3920 and 3921. Progress toward supporting the full protocol is
currently documented in the file 'xmpp.ss'


@section{Installation}

(require (planet zzkt/gibberish:1:0/xmpp))


@section{Example Chat Client}

@schemeblock[
             
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
             ]

and chat away...
