;; talk to 127.0.0.1:5601

;; (require 'json-rpc)

;; (process-live-p my-conn)

(defvar my-conn nil)

(defun my-rpc-connect (port)
  (when my-conn
    (delete-process my-conn))
  (setq my-conn (make-network-process :name "my-rpc"
                                      :buffer (get-buffer-create "*my-rpc*")
                                      ;; :server t
                                      :service port
                                      :filter 'my-listen-filter
                                      :log 'my-log
                                      )))
(defun my-rpc-send (what)
  (when (not my-conn)
    (my-rpc-connect 5601))
  (with-temp-buffer
    (insert (json-encode what))
    (message "Sending %s" (buffer-substring-no-properties (point-min) (point-max)))
    (process-send-region my-conn (point-min) (point-max))
    ;; TODO synchronized send and read i.e. wait for results
    ))

(defun my-log (server client message)
  (message "logging %s %s %s" server client message))


(defun my-listen-filter (proc string)
  (let ((jobj (json-read-from-string string)))
    (message "received: %s" jobj)
    ;; (setq tmpvar jobj)
    ;; execute callback
    ;; (message jobj)
    (let ((name (elt jobj 0))
          (id (elt jobj 1)))
      ;; assert name is "cb"
      (when (string-equal name "cb")
        (message "calling function handler for %s" id)
        (if (assoc id handlers)
            (funcall (cadr (assoc id handlers)) jobj)
          (warn "Handler %s not registered" id))))
    nil))

;; magic numbers for callback IDs
(defvar handlers `((6 gotosymbol-handler)
                   (1 echo-handler)))

(defun gotosymbol-handler (jobj)
  (message "Inside gotosymbol handler")
  (let ((error-p (cdr (assoc 'error (elt jobj 2))))
        (items (cdr (assoc 'items (elt jobj 2)))))
    ;; this is ':json-false
    (message "Error? %s" error-p)
    ;; (message "Items: %s" items)
    (cl-loop for item being the elements of items
             do
             (let ((file (cdr (assoc 'file item)))
                   (line (cdr (assoc 'line item)))
                   (text (cdr (assoc 'text item))))
               ;; TODO do something for these results
               (message "%s:%s (%s)" file line text)))))
(defun echo-handler (jobj)
  (message "echo %s" jobj))

(defun my-rpc-send-ping ()
  (my-rpc-send '(((:type . "ping") (:callback . 1)))))

(defun my-rpc-send-goto (symbol)
  (let ((query `((("type" . "gotosymbol")
                  (:callback . 6))
                 ((:word . ,symbol)
                  (:path . "/home/hebi/git/AdvAE/julia/test.jl")
                  ("column" . 2)
                  ("row" . 2)))))
    (my-rpc-send query)))

(defun my-test ()
  (my-rpc-send-goto "reshape"))



;; utilities
(defun json-pp (jobj)
  (with-temp-buffer
    (insert jobj)
    (json-pretty-print (point-min) (point-max))
    (message (buffer-substring-no-properties (point-min) (point-max)))
    nil))

;; testing
(defun my-test ()

  (defvar gotosymbol-query '((("type" . "gotosymbol") (:callback . 6))
                             ((:word . "reshape")
                              (:path . "/home/hebi/git/AdvAE/julia/test.jl")
                              ("column" . 2)
                              ("row" . 2))
                             ))

  ;; (json-pp (json-encode gotosymbol-query))

  ;; (funcall (cadr (assoc 6 handlers)))

  ;; (apply 'gotosymbol-handler '())

  (cl-loop for i being the elements of (vector 1 2 3)
           do (prin1 i))

  (let ((items (cdr (assoc 'items (elt tmpvar 2)))))
    (cl-loop for item being the elements of items
             do
             (let ((file (cdr (assoc 'file item)))
                   (line (cdr (assoc 'line item)))
                   (text (cdr (assoc 'text item))))
               (message "%s:%s (%s)" file line text))))


  (my-rpc-send gotosymbol-query))

;; julia-repl

(defun julia-repl--symbol-at-point-or-prompt (prompt)
  (let ((sap (thing-at-point 'symbol t)))
    (if sap sap
      (let ((s (read-from-minibuffer prompt sap)))
          (if (equal "" (s-trim s))
              nil
            s)))))

(defun julia-repl-goto-symbol (symbol)
  "Goto symbol under cursor."
  (interactive
   (list
    ;; get current word, use as default
    ;; prompt minibuffer
    (let ((cur (thing-at-point 'symbol t)))
      (if cur cur
        (read-string "Symbol: ")))))
  (when symbol
    ;; do visit def
    ;; (julia-repl--do-goto-symbol symbol)
    (my-rpc-send-goto symbol)
    ;; I'll goto symbol in the handler
    ))


