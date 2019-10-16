;;; julia-utils.el --- -*- lexical-binding: t -*-

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

(defun julia-repl--replace-images ()
  "Replace all image patterns with actual images"
  ;; Adapted from racket-mode repl
  (with-silent-modifications
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward  "#<Image: \\(.+\\)>" nil t)
        (message "Found match")
        (let* ((file (match-string 1))
               (begin (match-beginning 0))
               (end (match-end 0)))
          (delete-region begin end)
          (goto-char begin)
          (if (display-images-p)
              (progn (message "inserting image")
                     (insert-image (create-image file
                                                 'imagemagick
                                                 ;; 'png
                                                 nil
                                                 :scale 10
                                                 ;; :height 100
                                                 ) "[image]"))
            (goto-char begin)
            (insert "[image] ; use M-x racket-view-last-image to view")))))))

(defun julia-repl--output-filter (_)
  (julia-repl--replace-images))

(defvar julia-true-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'completion-at-point)
    map)
  "Keymap used in hn buffer.")

(defun julia-repl--completion-get-completions (input)
  ;; Reflections
  ;; - Base.names(mod)
  ;; - TODO fieldnames(struct)
  ;;
  ;; TODO Documents:
  ;; - Docs.getdoc(x): get document on instance of a Type, showing the
  ;;   doc for the type
  ;; - @doc reshape: access the doc
  (let* ((base
          ;; remove until the last .
          (replace-regexp-in-string "\\.[^\\.]*$" "" input))
         ;; Send to comint and get output . Looks like I cannot match
         ;; the whole thing (".*"), I probably just implement getting
         ;; the completions directly here
         (res (comint-redirect-results-list-from-process
               (get-buffer-process (julia-repl-inferior-buffer))
               (format "println(Base.names(%s))" base)
               "ERROR:\\|:[[:alpha:]][[:word:]]+" 0))
         ;; detect error
         (newres (if (member "ERROR:" res) nil
                   (mapcar (lambda (s) (substring s 1)) res)))
         ;; Add base
         (base-res (mapcar (lambda (s) (concat base "." s))
                           newres)))
    base-res))

(defun julia-repl--completion-at-point ()
  (let* ((inferior-buffer (julia-repl-inferior-buffer))
         (proc (get-buffer-process inferior-buffer)))
    ;; get the thing at point
    (let* ((line-start (cdr comint-last-prompt))
           ;; TODO python has completion for import, I would probably
           ;; implement for `using'
           ;;
           ;; TODO I should search from (point) backward to get a
           ;; symbol name, and use that as start
           (start line-start)
           (end (point))
           (completion-fn
            #'julia-repl--completion-get-completions))
      ;; to form a valid completion list, it seems that I need to
      ;; return something like this
      (list start end
            ;; input will be fed into completion-fn by
            ;; completion-table-dynamic
            (completion-table-dynamic
             completion-fn)))))

(define-derived-mode julia-true-repl-mode comint-mode "Julia-REPL"
  "Major mode for Julia REPL."
  (setq-local comint-prompt-read-only t)
  (setq-local comint-scroll-show-maximum-output nil) ;t slow for big outputs
  (setq-local mode-line-process nil)

  ;; Detect special julia prompt.
  ;;
  ;; I'm trying this for fixing the comint-redirect-xxx not getting
  ;; all output problem. Yes, it did fix it! The default method is to
  ;; use text property to determine the prompt, so not that point!!
  ;;
  ;; This has to be nil, to make C-a respect the prompt
  (setq-local comint-use-prompt-regexp nil)
  ;; Although not used for detecting prompt, it is used by comint-redirect!!
  (setq-local comint-prompt-regexp "^julia> *")

  ;; replace images
  (add-hook 'comint-output-filter-functions #'julia-repl--output-filter nil t)
  ;; auto completion
  (add-hook 'completion-at-point-functions
            ;; #'python-shell-completion-at-point
            #'julia-repl--completion-at-point
            nil 'local)
  (compilation-setup t))

(provide 'julia-utils)
