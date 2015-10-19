;;; package --- emacs configuration
;;; Commentary:

;;; Code:

;; https://github.com/magnars/.emacs.d/blob/master/init.el
(setq inhibit-startup-message t)

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(if (eq system-type 'darwin)
  (setq --homebrew-prefix "/usr/local/")
  (setq --homebrew-prefix "~/.linuxbrew/"))

;; Homebrew executables and lisp files
(add-to-list 'load-path (concat --homebrew-prefix "bin/"))
(let ((default-directory (concat --homebrew-prefix "share/emacs/site-lisp/")))
  (normal-top-level-add-subdirs-to-load-path))

(blink-cursor-mode 0)

;; http://ergoemacs.org/emacs/emacs_make_modern.html
(global-linum-mode 1)
(column-number-mode 1)

;; Kill whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Add new lines automatically
(setq next-line-add-newlines t)

;; Faster redrawing
(setq redisplay-dont-pause nil)

;; Tabs (no)
(setq-default indent-tabs-mode nil)

;; case handling
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; allow 'y' or 'n' instead of 'yes' or 'no'
;; http://www.cs.berkeley.edu/~prmohan/emacs/
(fset 'yes-or-no-p 'y-or-n-p)

;; tramp is always fucking up
(setq tramp-verbose 6)

;; fix emoji support in cocoa-mode
;; https://github.com/dunn/company-emoji/issues/2#issue-99494790
(defun --set-emoji-font (frame)
"Adjust the font settings of FRAME so Emacs NS/Cocoa can display emoji properly."
  (if (eq system-type 'darwin)
    (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
    (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))
;; For when emacs is started with Emacs.app
(--set-emoji-font nil)
;; Hook for when a cocoa frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions '--set-emoji-font)

;;;;;;;;;;;;;;;;
;; KEYBINDINGS
;;;;;;;;;;;;;;;;
;; some inspiration from https://masteringemacs.org/article/my-emacs-keybindings

(setq mac-option-modifier 'meta)

;; mimic my tmux bindings, sort of
(define-key key-translation-map "\C-j" "\C-x")
(global-set-key "\M-o" 'other-window)
(global-set-key "\C-xj" 'other-window)
(global-set-key "\C-x;" 'other-window)

(global-set-key "\C-x\C-a" 'mark-whole-buffer)
(global-unset-key "\C-xh")

(global-set-key "\C-xk" 'kill-this-buffer)
(global-set-key "\C-x\C-k" 'kill-buffer)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key "\C-c;" 'comment-region)
(global-set-key "\C-c:" 'uncomment-region)

(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-co" 'browse-url-at-point)

(global-set-key "\C-cp" 'get-pbpaste)
(global-set-key "\C-ce" 'shell-command-replace-region)

;; I never want `ido-list-directory'
(global-set-key "\C-x\C-d" 'ido-dired)

;; see below
(global-set-key "\C-xm" 'company-complete)
(global-set-key "\C-cs" 'shruggie)
(global-set-key "\C-ck" 'insert-kbd)
(global-set-key "\C-cz" 'new-shell)
(define-key global-map "\M-Q" 'unfill-paragraph)

;; mimic native Mac OS behavior
(global-set-key "\M-_" 'mdash)

;; I also accidentally set column instead of opening a file
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/Keybindings.html#Keybindings
(global-unset-key "\C-xf")
(define-key key-translation-map "\C-xf" "\C-x\C-f")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODES ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ido)
(ido-mode t)

;; Auto fill mode
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'tex-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

;; word count
(autoload 'word-count-mode "word-count"
  "Minor mode to count words."
  t nil)
(global-set-key "\M-+" 'word-count-mode)

;; table mode
(require 'table)
(add-hook 'text-mode-hook 'table-recognize)
(add-hook 'markdown-mode-hook 'table-recognize)

;; Mail mode
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-hook 'mail-mode-hook 'turn-on-auto-fill)

;; Makefiles
(add-to-list 'auto-mode-alist '("\\.mak$" . makefile-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HOMEBREW FORMULAE
;; brew tap homebrew/bundle
;; brew tap dunn/emacs
;; brew bundle --file=emacs/Brewfile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default ispell-program-name (concat --homebrew-prefix "bin/aspell"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
(autoload 'tex-mode-flyspell-verify "flyspell" "" t)

;; editorconfig needs its hand held with a special exec-path and load-path
(setq exec-path (append exec-path '((concat --homebrew-prefix "opt/editorconfig/bin"))))
(add-to-list 'load-path (concat --homebrew-prefix "opt/editorconfig-emacs/share/emacs/site-lisp/editorconfig"))
(load "editorconfig")

;; installed --with-toc
(require 'markdown-mode)
(require 'markdown-toc)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; requires a newer version of Emacs than is provided by Debian
(if (eq system-type 'darwin)
  (progn
    (require 'magit)
    (global-set-key (kbd "C-x g") 'magit-status)
    (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
    (setq magit-last-seen-setup-instructions "1.4.0")))

(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

(require 'gitattributes-mode)
(require 'gitconfig-mode)
(require 'gitignore-mode)
(add-to-list 'auto-mode-alist '("^\\.gitattributes$" . gitattributes-mode))
(add-to-list 'auto-mode-alist '("^\\.gitconfig$" . gitconfig-mode))
(add-to-list 'auto-mode-alist '("^\\.gitignore$" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.git\/info\/attributes$" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.git\/config$" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.git\/info\/exclude$" . gitignore-mode))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(require 'fountain-mode)
(add-to-list 'auto-mode-alist '("\\.fountain$" . fountain-mode))

(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . scss-mode))

(require 'rainbow-mode)
(add-hook 'scss-mode-hook 'scss-rainbow-hook)
(defun scss-rainbow-hook ()
  "Colorize color strings."
  (rainbow-mode 1))

(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.drush\\.inc$" . php-mode))

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'interpreter-mode-alist '("iojs" . js2-mode))

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flycheck-package)
(eval-after-load 'flycheck
  '(flycheck-package-setup))

(require 'browse-kill-ring)

(require 'anzu)
(global-anzu-mode +1)
(set-face-attribute 'anzu-mode-line nil
  :foreground "#586e75" :weight 'bold)

;; Having trouble compiling hg on Linux
(if (eq system-type 'darwin)
  (require 'achievements))

(require 'unkillable-scratch)
(unkillable-scratch 1)
(setq unkillable-scratch-behavior 'bury)

(require 'applescript-mode)
(add-to-list 'auto-mode-alist '("\.applescript$" . applescript-mode))
(add-to-list 'interpreter-mode-alist '("osascript" . applescript-mode))

(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)

(require 'debbugs-org)
(autoload 'debbugs-org "debbugs-org" "" 'interactive)
(autoload 'debbugs-org-search "debbugs-org" "" 'interactive)
(autoload 'debbugs-org-bugs "debbugs-org" "" 'interactive)

(require 'register-list)
(global-set-key "\C-cr" 'register-list)

(require 'typo)
(add-hook 'markdown-mode-hook 'typo-mode)
(add-hook 'mail-mode-hook 'typo-mode)
;; typo-mode turns backticks into single left quotes in Markdown, so
;; we need another way to quickly make code fences:
(global-set-key "\C-c`" 'code-fence)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\.yaml$" . yaml-mode))

(require 'circe)
(setq my-credentials-file "/Users/cat/.emacs.d/.circe")
(defun my-nickserv-password (_)
  "Keep password out of backtraces.
See https://github.com/jorgenschaefer/circe/wiki/Configuration"
  (with-temp-buffer
    (insert-file-contents-literally my-credentials-file)
    (plist-get (read (buffer-string)) :nickserv-password)))

(setq circe-network-options
  '(("Freenode"
      :nick "dunndunndunn"
      :channels ("#machomebrew")
      :nickserv-password my-nickserv-password)))

(if (eq system-type 'darwin)
  (require 'pandoc-mode)
  (add-hook 'markdown-mode-hook 'pandoc-mode))

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(if (eq system-type 'darwin)
  (add-to-list 'load-path "~/Dropbox/projects/lisp/emoji"))
(require 'company-emoji)
(add-to-list 'company-backends 'company-emoji)

(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

(if (eq system-type 'darwin)
  (add-to-list 'load-path "/Users/cat/Dropbox/projects/lisp/homebrew-mode"))
(require 'homebrew-mode)
(global-homebrew-mode)

;;;;;;;;;;;;;;;
;; FUNCTIONS
;;;;;;;;;;;;;;;

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Take a multi-line paragraph and make it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
    "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
    "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
    "aliquip ex ea commodo consequat. Duis aute irure dolor in "
    "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
    "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
    "culpa qui officia deserunt mollit anim id est laborum."))

(defun shruggie ()
  "Insert him."
  (interactive)
  (insert "¯\\_(ツ)_/¯"))

(defun mdash ()
  "Insert a dang mdash ok."
  (interactive)
  (insert "—"))

(defun code-fence ()
  "Insert backticks to create a Markdown code block, \
then set point to the end of the first set of backticks \
so the code type can be specified."
  (interactive)
  (push-mark)
  (insert "```\n```")
  (goto-char (- (point) 4)))

(defun insert-kbd ()
  "Insert <kbd></kbd> then set point in the middle."
  (interactive)
  (push-mark)
  (insert "<kbd></kbd>")
  (goto-char (- (point) 6)))

(defun new-shell ()
  "Open a shell window.  If there are no other windows, \
create one; otherwise use `other-window'."
  (interactive)
  (if (= 1 (length (window-list)))
      (select-window (split-window-sensibly))
    (other-window 1))
  (shell))

(defun pipe-to-pbcopy (text)
  "Execute ../bin/copy.sh on TEXT, which copies it to the Mac OS \
clipboard.  This function is only meant to be assigned to \
'interprogram-cut-function'"
  ;; http://www.emacswiki.org/emacs/ExecuteExternalCommand
  (start-process "copy-to-clipboard" "*Messages*" "~/bin/copy.sh" text))
(if (eq system-type 'darwin)
  (setq interprogram-cut-function 'pipe-to-pbcopy))

(defun get-pbpaste ()
  "Execute `pbpaste`."
  (interactive)
  (insert (shell-command-to-string "pbpaste")))

;; https://github.com/sellout/emacs-color-theme-solarized/issues/141#issuecomment-71862293
(add-to-list 'custom-theme-load-path (concat --homebrew-prefix "share/emacs/site-lisp/solarized"))

;; no, I don't know why
(if (eq system-type 'darwin)
  (progn
    ;; `t` is important: http://stackoverflow.com/a/8547861
    (load-theme 'solarized t)
    (setq solarized-termcolors 256)
    (setq frame-background-mode 'light))
  (progn
    (setq solarized-termcolors 256)
    (setq frame-background-mode 'light)
    (load-theme 'solarized t)))

(defun fuck-you ()
  "Because this just doesn't work in Debian Terminal."
  (interactive)
  (setq solarized-termcolors 256)
  (setq frame-background-mode 'light)
  ;; `t` is important: http://stackoverflow.com/a/8547861
  (load-theme 'solarized t))

(defun shell-command-replace-region (start end command
				      &optional output-buffer replace
				      error-buffer display-error-buffer)
  "Slightly modified version of `shell-command-on-region'.
Only difference is it always replaces."
  (interactive (let (string)
		 (unless (mark)
		   (user-error "The mark is not set now, so there is no region"))
		 ;; Do this before calling region-beginning
		 ;; and region-end, in case subprocess output
		 ;; relocates them while we are in the minibuffer.
		 (setq string (read-shell-command "Shell command on region: "))
		 ;; call-interactively recognizes region-beginning and
		 ;; region-end specially, leaving them in the history.
		 (list (region-beginning) (region-end)
		       string
		       current-prefix-arg
		       t
		       shell-command-default-error-buffer
		       t)))
  (let ((error-file
	 (if error-buffer
	     (make-temp-file
	      (expand-file-name "scor"
				(or small-temporary-file-directory
				    temporary-file-directory)))
	   nil))
	exit-status)
    (if (or replace
	    (and output-buffer
		 (not (or (bufferp output-buffer) (stringp output-buffer)))))
	;; Replace specified region with output from command.
	(let ((swap (and replace (< start end))))
	  ;; Don't muck with mark unless REPLACE says we should.
	  (goto-char start)
	  (and replace (push-mark (point) 'nomsg))
	  (setq exit-status
		(call-process-region start end shell-file-name replace
				     (if error-file
					 (list t error-file)
				       t)
				     nil shell-command-switch command))
	  ;; It is rude to delete a buffer which the command is not using.
	  ;; (let ((shell-buffer (get-buffer "*Shell Command Output*")))
	  ;;   (and shell-buffer (not (eq shell-buffer (current-buffer)))
	  ;; 	 (kill-buffer shell-buffer)))
	  ;; Don't muck with mark unless REPLACE says we should.
	  (and replace swap (exchange-point-and-mark)))
      ;; No prefix argument: put the output in a temp buffer,
      ;; replacing its entire contents.
      (let ((buffer (get-buffer-create
		     (or output-buffer "*Shell Command Output*"))))
	(unwind-protect
	    (if (eq buffer (current-buffer))
		;; If the input is the same buffer as the output,
		;; delete everything but the specified region,
		;; then replace that region with the output.
		(progn (setq buffer-read-only nil)
		       (delete-region (max start end) (point-max))
		       (delete-region (point-min) (min start end))
		       (setq exit-status
			     (call-process-region (point-min) (point-max)
						  shell-file-name t
						  (if error-file
						      (list t error-file)
						    t)
						  nil shell-command-switch
						  command)))
	      ;; Clear the output buffer, then run the command with
	      ;; output there.
	      (let ((directory default-directory))
		(with-current-buffer buffer
		  (setq buffer-read-only nil)
		  (if (not output-buffer)
		      (setq default-directory directory))
		  (erase-buffer)))
	      (setq exit-status
		    (call-process-region start end shell-file-name nil
					 (if error-file
					     (list buffer error-file)
					   buffer)
					 nil shell-command-switch command)))
	  ;; Report the output.
	  (with-current-buffer buffer
	    (setq mode-line-process
		  (cond ((null exit-status)
			 " - Error")
			((stringp exit-status)
			 (format " - Signal [%s]" exit-status))
			((not (equal 0 exit-status))
			 (format " - Exit [%d]" exit-status)))))
	  (if (with-current-buffer buffer (> (point-max) (point-min)))
	      ;; There's some output, display it
	      (display-message-or-buffer buffer)
	    ;; No output; error?
	    (let ((output
		   (if (and error-file
			    (< 0 (nth 7 (file-attributes error-file))))
		       (format "some error output%s"
			       (if shell-command-default-error-buffer
				   (format " to the \"%s\" buffer"
					   shell-command-default-error-buffer)
				 ""))
		     "no output")))
	      (cond ((null exit-status)
		     (message "(Shell command failed with error)"))
		    ((equal 0 exit-status)
		     (message "(Shell command succeeded with %s)"
			      output))
		    ((stringp exit-status)
		     (message "(Shell command killed by signal %s)"
			      exit-status))
		    (t
		     (message "(Shell command failed with code %d and %s)"
			      exit-status output))))
	    ;; Don't kill: there might be useful info in the undo-log.
	    ;; (kill-buffer buffer)
	    ))))

    (when (and error-file (file-exists-p error-file))
      (if (< 0 (nth 7 (file-attributes error-file)))
	  (with-current-buffer (get-buffer-create error-buffer)
	    (let ((pos-from-end (- (point-max) (point))))
	      (or (bobp)
		  (insert "\f\n"))
	      ;; Do no formatting while reading error file,
	      ;; because that can run a shell command, and we
	      ;; don't want that to cause an infinite recursion.
	      (format-insert-file error-file nil)
	      ;; Put point after the inserted errors.
	      (goto-char (- (point-max) pos-from-end)))
	    (and display-error-buffer
		 (display-buffer (current-buffer)))))
      (delete-file error-file))
    exit-status))

;;; .emacs.el ends here
