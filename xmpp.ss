;;; A basic XMPP library which should conform to RFCs 3920 and 3921
;;;
;;; Copyright (C) 2009 FoAM vzw. 
;;;
;;;  This package is free software: you can redistribute it and/or
;;;  modify it under the terms of the GNU Lesser General Public
;;;  License as published by the Free Software Foundation, either
;;;  version 3 of the License, or (at your option) any later version.
;;;
;;;  This program is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;;  Lesser General Public License for more details.
;;;
;;;  You can find a copy of the GNU Lesser General Public License at
;;;  http://www.gnu.org/licenses/lgpl-3.0.html.
;;;
;;; Authors 
;;;
;;;  nik gaffney <nik@fo.am>
;;;
;;; Requirements
;;;
;;;  PLT for now. TLS requires a version of PLT > 4.1.5.3
;;;
;;; Commentary
;;;
;;;  Still a long way from implementing even a minimal subset of XMPP
;;; 
;;;  features implemented
;;;   - plaintext sessions on port 5222 
;;;   - "old sytle" ssl sessions on port 5223 (default)
;;;   - authenticate using an existing account
;;;   - send messages (rfc 3921 sec.4)
;;;   - send presence (rfc 3921 sec.5)
;;;   - parse (some) xml reponses from server
;;;   - handlers for responses
;;;   - basic roster handling (rfc 3921 sec.7)
;;;
;;;  features to implement
;;;   - account creation
;;;   - managing subscriptions & rosters (rfc 3921 sec.6 & 8)
;;;   - error handling for rosters (rfc 3921 sec.7)
;;;   - plaintext/tls/sasl negotiation (rfc 3920 sec.5 & 6) 
;;;   - encrypted connections using tls on port 5222
;;;   - correct namespaces in sxml
;;;   - message types
;;;   - maintain session ids
;;;   - maintain threads
;;;   - error handling
;;;   - events
;;;   - [...]
;;;   - rfc 3920 
;;;   - rfc 3921
;;;
;;;  bugs and/or improvements
;;;   - start & stop functions for multiple sessions
;;;   - pubsub (XEP-0060) & group chats (XEP-0045)
;;;   - 'send' using call/cc & parameterize'd i/o ports
;;;   - coroutines for sasl negotiation
;;;   - read-async & repsonse-handler
;;;   - ssax:xml->sxml or lazy:xml->sxml
;;;   - default handlers
;;;   - syntax for defining sxpath based handlers
;;;   - improve parsing
;;;   - chatbot exmples
;;;   

(module xmpp scheme
  
  (require (planet lizorkin/sxml:2:1/sxml))   ;; encoding xml
  (require (planet lizorkin/ssax:2:0/ssax))   ;; decoding xml
  (require mzlib/os)                          ;; hostname
  (require scheme/tcp)                        ;; networking
  (require openssl)                           ;; ssl/tls
  (require srfi/13)                           ;; jid decoding
  
  (provide (all-defined-out))
  
  ;;;; ;  ;;  ;
  ;; 
  ;; debugging
  ;;
  ;;;;  ;  ;
  
  (define debug? #t)
  
  (define debugf
    (case-lambda 
      ((str) (when debug? (printf str)))
      ((str . dir) (when debug? (apply printf (cons str dir))))))
  
  ;;;;;;;;;;; ; ;;;;  ;   ;;; ;    ; ;;     ;
  ;;
  ;; networking
  ;;
  ;;;;;; ;;  ;;  ;  ; ;   ;
  
  (define port 5222)
  (define ssl-port 5223)
  
  (define (open-connection machine port handler)
    (let-values (((in out)
                  (tcp-connect machine port))) 
      (handler in out)
      (close-output-port out)
      (close-input-port in)))
  
  (define (open-ssl-connection machine port handler)
    (let-values (((in out)
                  (ssl-connect machine port 'tls)))
      (handler in out)
      (close-output-port out)
      (close-input-port in)))
  
  (define (read-async in)
    (bytes->string/utf-8 (list->bytes (read-async-bytes in))))
  
  (define (read-async-bytes in)
    (let ((bstr '()))
      (when (sync/timeout 0 in)
        (set! bstr (cons (read-byte in) (read-async-bytes in)))) bstr))
  
  (define ssxml srl:sxml->xml-noindent) 
  
  ;;;;;; ; ; ;      ;   ;; ;;;;;; ;
  ;;
  ;; XMPP stanzas
  ;;
  ;;;;;;;;;; ;;;  ;  ;;   ;  ;
  
  ;; intialization
  (define (xmpp-stream host) 
    (string-append "<?xml version='1.0'?>" ;; version='1.0' is a MUST for SASL on 5222 but NOT for ssl on 5223
                   "<stream:stream xmlns:stream='http://etherx.jabber.org/streams' to='" 
                   host 
                   "' xmlns='jabber:client' >")) 
  
  ;; authentication
  (define (xmpp-auth username password resource)
    (ssxml `(iq (@ (type "set") (id "auth")) 
                (query (@ (xmlns "jabber:iq:auth")) 
                       (username ,username) 
                       (password ,password)
                       (resource ,resource)))))
  
  (define (xmpp-session host)
    (ssxml `(iq (@ (to ,host) (type "set") (id "session")) 
                (session (@ (xmlns "urn:ietf:params:xml:ns:xmpp-session")))))) 
  
  ;; messages
  (define (message to body)
    (ssxml `(message (@ (to ,to)) (body ,body))))
  
  ;; presence
  (define (presence #:from (from "")
                    #:to (to "") 
                    #:type (type "") 
                    #:show (show "") 
                    #:status (status ""))
    (cond ((not (string=? status ""))
           (ssxml `(presence (@ (type "probe")) (status ,status))))
          ((string=? type "") "<presence/>")
          (else (ssxml `(presence (@ (type ,type)))))))
  
  ;; queries
  (define (iq body 
              #:from (from "")
              #:to (to "") 
              #:type (type "") 
              #:id (id ""))
    (ssxml `(iq (@ (to ,to) (type ,type) ,body))))
  
  ;; curried stanza disection (sxml stanza -> string)
  (define ((sxpath-element xpath (ns "")) stanza) 
    (let ((node ((sxpath xpath (list (cons 'ns ns))) stanza)))
      (if (empty? node) "" (car node))))
  
  ;; message 
  (define message-from (sxpath-element "message/@from/text()"))
  (define message-to (sxpath-element "message/@to/text()"))
  (define message-id (sxpath-element "message/@id/text()"))
  (define message-type (sxpath-element "message/@type/text()"))
  (define message-body (sxpath-element "message/body/text()"))
  (define message-subject (sxpath-element "message/subject/text()"))
  
  ;; info/query
  (define iq-type (sxpath-element "iq/@type/text()"))
  (define iq-id (sxpath-element "iq/@id/text()"))
  (define iq-error-type (sxpath-element "iq/error/@type/text()"))
  (define iq-error-text (sxpath-element "iq/error/text()"))
  (define iq-error (sxpath-element "iq/error"))
  
  ;; presence
  (define presence-show (sxpath-element "presence/show/text()"))
  (define presence-from (sxpath-element "presence/@from/text()"))
  (define presence-status (sxpath-element "presence/status/text()"))
  
  
  ;;;;;;;;;; ; ;     ;  ;;  ;
  ;;
  ;; rosters
  ;;
  ;;;;;; ; ;;  ;
  
  ;; request the roster from server
  (define (request-roster from)
    (ssxml `(iq (@ (from ,from) (type "get") (id "roster_1")) 
                (query (@ (xmlns "jabber:iq:roster"))))))
  
  ;; add an item to the roster 
  (define (add-to-roster from jid name group)
    (ssxml `(iq (@ (from ,from) (type "set") (id "roster_2")) 
                (query (@ (xmlns "jabber:iq:roster"))
                       (item (@ (jid ,jid) (name ,name))
                             (group ,group)))))) 
  
  ;; update an item in the roster 
  (define (update-roster from jid name group)
    (ssxml `(iq (@ (from ,from) (type "set") (id "roster_3")) 
                (query (@ (xmlns "jabber:iq:roster"))
                       (item (@ (jid ,jid) (name ,name))
                             (group ,group)))))) 
  
  ;; remove an item from the roster
  (define (remove-from-roster from jid)
    (ssxml `(iq (@ (from ,from) (type "set") (id "roster_4")) 
                (query (@ (xmlns "jabber:iq:roster"))
                       (item (@ (jid ,jid) (subscription "remove"))))))) 
  
  
  ;;;;; ;   ; ;;  ;   ;
  ;;
  ;; in-band registration
  ;;
  ;;;;;; ;;     ;; ;
  
  (define (reg1)
    (ssxml `(iq (@ (type "get") (id "reg1"))
                (query (@ (xmlns "jabber:iq:register"))))))
  
  ;;;; ;; ;  ;;;  ;
  ;;
  ;; tls & sasl
  ;;  - http://xmpp.org/rfcs/rfc3920.html#tls
  ;;  - http://xmpp.org/rfcs/rfc3920.html#sasl
  ;;
  ;;;; ;;
  
  (define session->tls? #f) ;; changes state when a tls proceed is recived
  
  ;; moved to xmpp-sasl until it 'works'
  
  
  ;;;;;;;;; ; ;; ; ; ;;  ;;    ;  ;
  ;;
  ;; parsing & message/iq/error handlers
  ;;  - minimal parsing
  ;;  - handlers match on a tag (eg. 'message)
  ;;  - handlers are called with a single relevant xmpp stanza 
  ;;
  ;;;;;; ;;  ; ; ;;  ;
  
  (define xmpp-handlers (make-hash)) ;; a hash of tags and functions (possibly extend to using sxpaths and multiple handlers)
  
  (define (set-xmpp-handler type fcn)
    (dict-set! xmpp-handlers type fcn))
  
  (define (remove-xmpp-handler type fcn)
    (dict-remove! xmpp-handlers type fcn))
  
  (define (run-xmpp-handler type sz)
    (let ((fcn (dict-ref xmpp-handlers type #f))) 
      (when fcn (begin
                  (debugf "attempting to run handler ~a.~%" fcn)
                  (fcn sz)))))
  
  ;; no real parsing yet. dispatches any received xml stanzas as sxml
  
  (define (parse-xmpp-response str)
    (when (> (string-length str) 0)
      (let ((sz (ssax:xml->sxml (open-input-string (clean str)) '())))
        ;;(let ((sz (lazy:xml->sxml (open-input-string str) '())))
        (cond
          ((equal? '(null) (cadr sz)) 
           (newline))
          ((equal? 'message (caadr sz))
           (run-xmpp-handler 'message sz))
          ((equal? 'iq (caadr sz)) 
           (run-xmpp-handler 'iq sz))
          ((equal? 'presence (caadr sz)) 
           (run-xmpp-handler 'presence sz))
          (else (run-xmpp-handler 'other sz))))))
  
  ;; example handlers to print stanzas or their contents
  (define (print-message sz)
    (printf "a ~a message from ~a which says '~a.'~%" (message-type sz) (message-from sz) (message-body sz)))
  
  (define (print-iq sz)
    (printf "an iq response of type '~a' with id '~a.'~%" (iq-type sz) (iq-id sz)))
  
  (define (print-presence sz)
    (printf " p-r-e-s-e-n-e-c--> ~a is ~a" (presence-from sz) (presence-status)))
  
  (define (print-stanza sz)
    (printf "? ?? -> ~%~a~%" sz))
  
  ;; handler to print roster
  
  (define (roster-jids sz) 
    ((sxpath "iq/ns:query/ns:item/@jid/text()" '(( ns . "jabber:iq:roster"))) sz))

  (define (roster-items sz)
    ((sxpath-element "iq/ns:query/ns:item"  '(( ns . "jabber:iq:roster"))) sz))
  
  (define (print-roster sz)
    (when (and (string=? (iq-type sz) "result")
               (string=? (iq-id sz) "roster_1"))
      (printf "~a~%" (roster-jids sz))))
  
  ;; QND hack to filter out anything not a message, iq or presence
  (define (clean str)
    (let ((test (substring str 0 3)))
      (cond ((string-ci=? test "<me") str)
            ((string-ci=? test "<iq") str)
            ((string-ci=? test "<pr") str)
            ((string-ci=? test "<ur") str)
            (else 
             (debugf "~%recieved: ~a ~%parsed as <null/>~%~%" str)
             "<null/>"))))
  
  
  ;; response handler
  (define (xmpp-response-handler in)
    (thread (lambda () 
              (let loop () 
                (parse-xmpp-response (read-async in))
                (sleep 0.1) ;; slight delay to avoid a tight loop
                (loop)))))
  
  ;; jid splicing (assuming the jid is in the format user@host/resource)
  (define (jid-user jid)
    (string-take jid (string-index jid #\@)))
  
  (define (jid-host jid)
    (let* ((s (string-take-right jid (- (string-length jid) (string-index jid #\@) 1)))
           (v (string-index s #\/)))
      (if v (string-take s v) s )))
  
  (define (jid-resource jid)
    (let ((r (jid-resource-0 jid)))
      (if (void? r) (gethostname) r)))
  
  (define (jid-resource-0 jid)
    (let ((v (string-index jid #\/)))
      (when v (string-take-right jid (- (string-length jid) v 1)))))    
  
  
  ;;;;  ;; ; ; ;; ;;   ;;;; ;
  ;;
  ;; interfaces
  ;;
  ;;;;; ;; ;;;;   ;   ;;  ;
  
  (define xmpp-in-port (make-parameter #f))
  (define xmpp-out-port (make-parameter #F))
  
  (define (send str)
    (debugf "sending: ~a ~%~%" str)
    (let* ((p-out  (xmpp-out-port))
           (out (if p-out p-out xmpp-out-port-v))) 
      (fprintf out "~A~%" str) (flush-output out)))
  
  (define-syntax with-xmpp-session
    (syntax-rules ()
      ((_ jid pass form . forms)
       (let ((host (jid-host jid))
             (user (jid-user jid))
             (resource (jid-resource jid)))
         (let-values (((in out)
                       (ssl-connect host ssl-port 'tls)))
           ;;(tcp-connect host port)))
           (parameterize ((xmpp-in-port in)
                          (xmpp-out-port out))
             (file-stream-buffer-mode out 'line)
             (xmpp-response-handler in) 
             (send (xmpp-stream host))
             (send (xmpp-session host))           
             ;(starttls in out)
             (send (xmpp-auth user pass resource))
             (send (presence))
             (begin form . forms)
             (close-output-port out)
             (close-input-port in)))))))
  
  ;; NOTE: this will only work with a single connection to a host, however multiple sessions to that host may be possible
  (define xmpp-in-port-v (current-input-port))
  (define xmpp-out-port-v (current-output-port))
  
  (define (start-xmpp-session jid pass)
    (let ((host (jid-host jid))
          (user (jid-user jid))
          (resource (jid-resource jid)))
      (let-values (((in out)
                    (ssl-connect host ssl-port 'tls)))
        ;;(tcp-connect host port)))
        (set! xmpp-in-port-v in)
        (set! xmpp-out-port-v out)
        (file-stream-buffer-mode out 'line)
        (xmpp-response-handler in) 
        (send (xmpp-stream host))
        (send (xmpp-session host))           
        ;;(starttls in out)
        (send (xmpp-auth user pass resource))
        (send (presence)))))
  
  (define (close-xmpp-session)
    (close-output-port xmpp-out-port-v)
    (close-input-port xmpp-in-port-v))
  
  ) ;; end module

