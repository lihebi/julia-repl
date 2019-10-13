(defsubst julia--safe-forward-char ()
  "Move back one character, but don't error if we're at the
beginning of the buffer."
  (unless (eq (point) (point-max))
    (forward-char)))

(defun julia-safe-forward-sexp ()
  (if (condition-case nil (forward-sexp) (error t))
      (ignore-errors (forward-char))))

(defun julia-current-line-block-start-p ()
  "If the current line is a block start, send the region until
  the block end"
  (> (julia-current-line-block-level) 0))

(defun julia-current-line-block-level ()
  "Return the current line block level. If have open block, count
  as 1, if have end, count as -1."
  (save-excursion
    (back-to-indentation)
    (let ((count
           ;; FIXME need to count the () in comments
           (- (count-matches "(" (line-beginning-position) (line-end-position))
              (count-matches ")" (line-beginning-position) (line-end-position))))
          (end (line-end-position)))
      ;; I need an initial forward, otherwise it is counted twice
      (julia-safe-forward-sexp)
      (while (<= (point) end)
        (setq count (cond ((julia-at-keyword julia-block-start-keywords)
                           (+ count 1))
                          ((equal (current-word t) "end")
                           (- count 1))
                          ;; I cannot add it here, because I'm using
                          ;; forward-sexp, it will behave differently
                          ;; for reaching ()
                          ;;
                          ;; ((looking-at "(") (+ count 1))
                          ;; ((looking-at ")") (- count 1))
                          (t count)))
        (julia-safe-forward-sexp))
      count)))

(defun julia-block-end-pos ()
  "Go to the block end."
  (when (> (julia-current-line-block-level) 0)
    (save-excursion
      (let ((count (julia-current-line-block-level)))
        (while (and (> count 0)
                    (< (point) (point-max))
                    (forward-line 1))
          (message "Count: %s" count)
          (setq count (+ count (julia-current-line-block-level))))
        (line-end-position)))))

(defun julia-mark-block ()
  "Mark the block."
  (interactive)
  (set-mark (line-beginning-position))
  (goto-char (julia-block-end-pos)))

(define-derived-mode julia-true-repl-mode comint-mode "Julia-REPL"
  "Major mode for Julia REPL."
  (setq-local comint-use-prompt-regexp nil)
  (setq-local comint-prompt-read-only t)
  (setq-local comint-scroll-show-maximum-output nil) ;t slow for big outputs
  (setq-local mode-line-process nil)
  (compilation-setup t))

(provide 'julia-utils)
