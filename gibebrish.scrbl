#lang scribble/doc
@(require scribble/manual)

@title{Gibberish}

@title{Example chat client}

@schemeblock[
             
             (require xmpp)
             
             (define (read-input prompt)
               (display prompt)
               (read-line (current-input-port)))
             
             (define (chat2)
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


