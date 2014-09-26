(use-modules (system repl coop-server)
             (system repl server)
             (tox))

(define tox (make-tox))

(define tox-bootstrap-nodes '())
(define tox-repl-port 37146)

(define (bootstrap!)
  (if (null? tox-bootstrap-nodes)
      (error "No bootstrap nodes!  Please configure tox-bootstrap-nodes.")
      (for-each (lambda (node)
                  (tox-bootstrap-from-address
                   tox
                   (assq-ref node 'ip)
                   (or (assq-ref node 'port) 33445)
                   (tox-client-id (assq-ref node 'id))))
                tox-bootstrap-nodes)))

(define (quit!)
  (abort-to-prompt 'tox-client))

(define (save!)
  (display "Saving to tox_save\n")
  (with-output-to-file "tox_save"
    (lambda ()
      (format #t "~a" (tox-save tox)))))

(define (load!)
  (when (file-exists? "tox_save")
    (display "Loading tox_save\n")
    (tox-load tox (with-input-from-file "tox_save" read))))

(define connected? #f)
(define last-connection-attempt (current-time))
(define connection-retry-interval 3)

;; Load user init file
(let ((config-file (string-append (getenv "HOME") "/.guile-tox")))
  (if (file-exists? config-file)
      (load config-file)
      (error (format #f "No .guile-tox file found in ~a" (getenv "HOME")))))

;; Boot REPL server
(define repl
  (spawn-coop-repl-server
   (make-tcp-server-socket #:port tox-repl-port)))

(format #t "Welcome to Guile-Tox!
This client acts a server to send commands to via the Guile REPL.
Connect to the REPL server via telnet or Emacs on port ~a
Once you've connected to the REPL, try setting your nickname by typing:
  (set-tox-name \"Tox User\")
To discover other Tox procedures, type:
  ,a tox
To see the documentation for a procedure, type:
  ,d <procedure-name-goes-here>
Happy hacking!\n\n"
        tox-repl-port)
(load!)
(bootstrap!)

(call-with-prompt
 'tox-client
 (lambda ()
   (while #t
     (when (not connected?)
       (if (tox-connected? tox)
           (begin
             (display "Connected to DHT\n")
             (set! connected? #t))
           (let ((time (current-time)))
             (when (> time
                      (+ last-connection-attempt
                         connection-retry-interval))
               (display "Re-trying connection...\n")
               (set! last-connection-attempt time)
               (bootstrap!)))))

     (poll-coop-repl-server repl)
     (tox-do tox)
     (usleep (* (tox-do-interval tox) 1000))))
 (lambda args
   (display "Bye!\n")
   (save!)))

;; Local Variables:
;; compile-command: "guile -L .. client.scm"
;; End:
