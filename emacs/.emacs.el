;;; package --- emacs configuration
;;; Commentary:

;;; Code:

;; https://github.com/magnars/.emacs.d/blob/master/init.el
(setq inhibit-startup-message t)

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;;
;; Homebrew executables and lisp files
;;
;; brew tap homebrew/bundle
;; brew bundle --file=../Brewfile-darwin (or Brewfile-linux as the case may be)
(if (eq system-type 'darwin)
    (setq --homebrew-prefix "/usr/local/")
  (setq --homebrew-prefix "/home/linuxbrew/.linuxbrew/"))
(add-to-list 'load-path (concat --homebrew-prefix "bin/"))

;; Add development packages
(let ((default-directory "~/elisp/"))
   (normal-top-level-add-subdirs-to-load-path))

;;
;; Package manager
;;
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-refresh-contents)

;;
(defun require-package (package)
  "Install PACKAGE with package.el and require it."
  (unless (package-installed-p package)
    (package-install package))
  (require package))

;;
;; Customizations
;;
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;;
;; Color scheme
;;
;; https://github.com/sellout/emacs-color-theme-solarized/issues/141#issuecomment-71862293
(add-to-list 'custom-theme-load-path "~/elisp/solarized")
(require-package 'exec-path-from-shell)
(if (equal "winter" (exec-path-from-shell-getenv "SEASON"))
    (setq frame-background-mode 'dark)
  (setq frame-background-mode 'light))
;; `t` is important: http://stackoverflow.com/a/8547861
(load-theme 'solarized t)

;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;
;; some inspiration from https://masteringemacs.org/article/my-emacs-keybindings
;;
;; Requires matching change in iTerm key profile
(setq mac-option-modifier 'meta)
;;
;; Mimic native Mac OS behavior
(global-set-key "\M-_" '--mdash)
;;
(global-set-key "\M-o" 'other-window)
(global-set-key "\C-xo" (lambda () (interactive) (switch-to-buffer (other-buffer))))
;;
(global-set-key "\C-xk" 'kill-this-buffer)
(global-set-key "\C-x\C-k" 'kill-buffer)
;;
(global-set-key "\C-x;" 'comment-region)
(global-set-key "\C-x:" 'uncomment-region)
;;
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-co" 'browse-url-at-point)
(global-set-key "\C-cp" '--pbpaste)
(global-set-key "\C-cs" '--shell-command-replace-region)
;;
;; bindings for custom functions defined below
(global-set-key "\C-ck" '--insert-kbd)
(global-set-key "\C-cy" '--shruggie)
(global-set-key "\C-cz" '--new-shell)
(global-set-key "\C-c\C-l" '--solarized-light)
(global-set-key "\C-c\C-d" '--solarized-dark)
;;
;; window resizing
;; https://www.emacswiki.org/emacs/WindowResize
(global-set-key "\C-cJ" '--resize-window-left)
(global-set-key "\C-c:" '--resize-window-right)
(global-set-key "\C-cK" '--resize-window-down)
(global-set-key "\C-cL" '--resize-window-up)

;;;;;;;;;;;;;;;;;;;
;; General settings
;;;;;;;;;;;;;;;;;;;
;;
(blink-cursor-mode 0)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;;
;; http://ergoemacs.org/emacs/emacs_make_modern.html
(column-number-mode 1)
;;
;; Kill whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;;
;; Add new lines automatically
(setq next-line-add-newlines t)
;;
;; Tabs (no)
(setq-default indent-tabs-mode nil)
;;
;; allow 'y' or 'n' instead of 'yes' or 'no'
;; http://www.cs.berkeley.edu/~prmohan/emacs/
(fset 'yes-or-no-p 'y-or-n-p)
;;
(require-package 'unkillable-scratch)
(unkillable-scratch 1)
(setq unkillable-scratch-behavior 'bury)

;;
;; Navigation and search
;;
(require-package 'swiper)
(global-set-key "\C-s" 'swiper)
(global-set-key "\C-r" 'swiper)
(global-set-key (kbd "C-c u") 'swiper-all)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(put 'ivy-done 'disabled nil)
;;
(require-package 'counsel)
(global-set-key "\M-x" 'counsel-M-x)
(global-set-key (kbd "C-x f") 'counsel-find-file)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key "\C-cl" 'counsel-locate)
;;
(require-package 'neotree)
(global-set-key "\M-t" 'neotree-toggle)
;;
(require-package 'ag)
(global-set-key "\C-cf" 'ag)
(global-set-key "\C-c\C-f" 'ag-regexp)
;;
(require-package 'beacon)
(beacon-mode 1)
;;
(require-package 'nlinum)
(global-nlinum-mode 1)

;;
;; Auto-completion
;;
(require-package 'company)
(require-package 'company-ansible)
(require-package 'company-emoji)
(require-package 'company-web)
(add-to-list 'company-backends 'company-ansible)
(add-to-list 'company-backends 'company-emoji)
(add-to-list 'company-backends 'company-web-html)
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key "\C-xm" 'company-complete)

;;
;; Text and fonts
;;
(require-package 'list-unicode-display)
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

;;
;; Spellchecking
;;
;; brew install aspell
(setq-default ispell-program-name (concat --homebrew-prefix "bin/aspell"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
(autoload 'tex-mode-flyspell-verify "flyspell" "" t)

;;
;; Code style
;;
(require-package 'editorconfig)
;;
(require-package 'pkg-info)
(require-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
;;
(require-package 'flycheck-package)
(eval-after-load 'flycheck
  '(flycheck-package-setup))

;;
;; Debugging
;;
;; (require-package 'realgud)
(require-package 'suggest)

;;
;; Git
;;
(require-package 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(global-set-key (kbd "C-c b") 'magit-blame)
(global-set-key (kbd "C-c h") 'magit-log-buffer-file)
(setq magit-last-seen-setup-instructions "1.4.0")
(add-to-list 'git-commit-style-convention-checks
             'overlong-summary-line)

;;
(global-set-key "\C-cr" 'vc-resolve-conflicts)
(require-package 'gitattributes-mode)
(require-package 'gitconfig-mode)
(require-package 'gitignore-mode)
(add-to-list 'auto-mode-alist '("^\\.gitattributes$" . gitattributes-mode))
(add-to-list 'auto-mode-alist '("^\\.gitconfig$" . gitconfig-mode))
(add-to-list 'auto-mode-alist '("^\\.gitignore$" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.git\/info\/attributes$" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.git\/config$" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.git\/info\/exclude$" . gitignore-mode))
;;
(require-package 'diff-hl)
(global-diff-hl-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t)
;;
(require-package 'diffview)
(add-hook 'diff-mode-hook 'diffview-current)

;;
;; pinentry
;;
(require-package 'pinentry)
(setq epa-pinentry-mode 'loopback)
(pinentry-start)

;;
;; RSS
;;
(require-package 'elfeed)
(global-set-key (kbd "C-c w") 'elfeed)
(load "~/.emacs.d/elfeeds.el")

;;
;; IRC
;;
(require-package 'circe)
(load "~/.emacs.d/circe.el")

;;
;; password-store
;;
(require-package 'pass)

;;;;;;;;;;;;;;;;;;;
;; Language support
;;;;;;;;;;;;;;;;;;;
;;
;; LSP
;;
(require-package 'lsp-mode)
(require-package 'lsp-ivy)
(require-package 'lsp-ui)
(require-package 'company-lsp)

;;
;; Plain text, Markdown, LaTeX, Fountain
;;
;; brew install markdown-mode
(require-package 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
;;
(require-package 'typo)
(add-hook 'markdown-mode-hook 'typo-mode)
;; typo-mode turns backticks into single left quotes in Markdown, so
;; we need another way to quickly make code fences:
(global-set-key "\C-c`" '--code-fence)
;;
(add-hook 'tex-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)
;;
(require 'table)
(add-hook 'text-mode-hook 'table-recognize)
(add-hook 'markdown-mode-hook 'table-recognize)
;;
(require-package 'fountain-mode)
(add-to-list 'auto-mode-alist '("\\.fountain$" . fountain-mode))
(add-hook 'fountain-mode-hook 'typo-mode)

;;
;; Org
;;
(require-package 'org)
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)
(global-set-key "\C-c\C-l" 'org-store-link)
(global-set-key "\C-c\C-a" 'org-agenda)
(global-set-key "\C-c\C-c" 'org-capture)
(global-set-key "\C-c\C-b" 'org-iswitchb)
(setq org-log-done 'time)

;;
;; HTML, CSS/SASS
;;
(require-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
;;
(require-package 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . scss-mode))
;;
(require-package 'rainbow-mode)
(add-hook 'scss-mode-hook (lambda () (rainbow-mode 1)))
;;
(require-package 'htmlize)

;;
;; PHP
;;
(require-package 'php-mode)

;;
;; JavaScript
;;
(require-package 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'interpreter-mode-alist '("iojs" . js2-mode))
;;
(require-package 'mocha)
(define-key js2-mode-map "\C-c,s" 'mocha-test-at-point)
;;
(require-package 'json-mode)

;;
;; Ruby
;;
(require-package 'rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)
;;
(require-package 'ruby-tools)
(require-package 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'web-mode-hook (lambda ()
                           (when (and buffer-file-name
                                      (string-match "\\.erb$" buffer-file-name))
                             (robe-mode))))
(eval-after-load 'company
  '(push 'company-robe company-backends))
;;
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
;;
(require-package 'rspec-mode)
(require-package 'minitest)
;;
(require-package 'homebrew-mode)
(global-homebrew-mode)

;;
;; Emacs lisp
;;
(require-package 'slime)
(setq inferior-lisp-program "sbcl")
(require-package 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))
(global-set-key "\C-ci" 'edebug-eval-top-level-form)

;;
;; Clojure
;;
(require-package 'cider)

;;
;; Nix
;;
(require-package 'nix-mode)

;;
;; C
;;
(global-set-key "\C-cc" 'compile)

;;
;; Go
;;
(require-package 'go-mode)

;;
;; Rust
;;
(require-package 'racer)
(require-package 'flycheck-rust)
(require 'lsp-rust)
;; https://github.com/flycheck/flycheck-rust#setup
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;;
;; Make, CMake
;;
(add-to-list 'auto-mode-alist '("\\.mak$" . makefile-mode))
(require-package 'cmake-mode)

;;
;; YAML
;;
(require-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\.yaml$" . yaml-mode))
(require-package 'ansible-doc)
(add-hook 'yaml-mode-hook #'ansible-doc-mode)

;;
;; RDF
;;
(require-package 'omn-mode)
(require-package 'rdf-prefix)
(require-package 'tawny-mode)

;;
;; PDF
;;
(require-package 'pdf-tools)
(add-to-list 'auto-mode-alist '("\.pdf$" . pdf-view-mode))

;;;;;;;;;;;;;;;
;; FUNCTIONS
;;;;;;;;;;;;;;;

(defun --win-to-right ()
  "True if there's a window to the right of the active window, false otherwise."
  (> (frame-width) (nth 2 (window-edges))))

(defun --win-below ()
  "True if there's a window below the active window, false otherwise."
  (> (frame-height) (+ 1 (nth 3 (window-edges)))))

(defvar --window-increment 10)

(defun --resize-window-right ()
  "Move the frame barrier rightwards."
  (interactive)
  (if (--win-to-right)
      (enlarge-window --window-increment t)
    (shrink-window --window-increment t)))

(defun --resize-window-left ()
  "Move the frame barrier leftwards."
  (interactive)
  (if (--win-to-right)
      (shrink-window --window-increment t)
    (enlarge-window --window-increment t)))

(defun --resize-window-up ()
  "Move the frame barrier leftwards."
  (interactive)
  (if (--win-below)
      (shrink-window --window-increment)
    (enlarge-window --window-increment)))

(defun --resize-window-down ()
  "Move the frame barrier leftwards."
  (interactive)
  (if (--win-below)
      (enlarge-window --window-increment)
    (shrink-window --window-increment)))

;; https://notmuchmail.org/emacstips/#index5h2
(defun --notmuch-search-flag ()
  "Toggle flag on message under point."
  (interactive)
  (if (member "flagged" (notmuch-search-get-tags))
      (notmuch-search-tag '("-flagged"))
    (notmuch-search-tag '("+flagged")))
  (notmuch-search-next-thread))
;;
(defun --notmuch-show-flag ()
  "Toggle flag on message under point."
  (interactive)
  (if (member "flagged" (notmuch-show-get-tags))
      (notmuch-show-tag '("-flagged"))
    (notmuch-show-tag '("+flagged"))))
;;
(defun --notmuch-search-read (&optional beg end)
  "Toggle read status of message(s) from BEG to END."
  (interactive (notmuch-search-interactive-region))
  (if (member "unread" (notmuch-search-get-tags))
      (notmuch-search-tag '("-unread") beg end)
    (notmuch-search-tag '("+unread") beg end))
  (notmuch-search-next-thread))
;;
(defun --notmuch-show-read (&optional beg end)
  "Toggle flag on message(s) from BEG to END."
  (interactive (notmuch-search-interactive-region))
  (if (member "unread" (notmuch-show-get-tags))
      (notmuch-show-tag '("-unread") beg end)
    (notmuch-show-tag '("+unread") beg end)))
;;
(defun --notmuch-search-spam (&optional beg end)
  "Toggle read status of message(s) from BEG to END."
  (interactive (notmuch-search-interactive-region))
  (if (member "spam" (notmuch-search-get-tags))
      (progn
        (notmuch-search-tag '("+inbox") beg end)
        (notmuch-search-tag '("-spam") beg end))
    (progn
      (notmuch-search-tag '("+spam") beg end)
      (notmuch-search-tag '("-unread") beg end)
      (notmuch-search-tag '("-inbox") beg end)))
  (notmuch-search-next-thread))
;;
(defun --notmuch-show-spam (&optional beg end)
  "Toggle flag on message(s) from BEG to END."
  (interactive (notmuch-search-interactive-region))
  (if (member "spam" (notmuch-show-get-tags))
      (progn
        (notmuch-search-tag '("+inbox") beg end)
        (notmuch-search-tag '("-spam") beg end))
    (progn
      (notmuch-search-tag '("+spam") beg end)
      (notmuch-search-tag '("-unread") beg end)
      (notmuch-search-tag '("-inbox") beg end))))

(defun --lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun --shruggie ()
  "Insert him."
  (interactive)
  (insert "¯\\_(ツ)_/¯"))

(defun --mdash ()
  "Insert a dang mdash ok."
  (interactive)
  (insert "—"))

(defun --code-fence ()
  "Insert backticks to create a Markdown code block, \
then set point to the end of the first set of backticks \
so the code type can be specified."
  (interactive)
  (push-mark)
  (insert "```\n```")
  (goto-char (- (point) 4)))

(defun --insert-kbd ()
  "Insert <kbd></kbd> then set point in the middle."
  (interactive)
  (push-mark)
  (insert "<kbd></kbd>")
  (goto-char (- (point) 6)))

(defun --pipe-to-pbcopy (text)
  "Execute ../bin/copy.sh on TEXT, which copies it to the Mac OS \
clipboard.  This function is only meant to be assigned to \
'interprogram-cut-function'"
  ;; http://www.emacswiki.org/emacs/ExecuteExternalCommand
  (start-process "copy-to-clipboard" "*Messages*" "~/bin/copy.sh" text))
(if (eq system-type 'darwin)
    (setq interprogram-cut-function '--pipe-to-pbcopy))

(defun --pbpaste ()
  "Insert the contents of the clipboard."
  (interactive)
  (insert (shell-command-to-string
           (if (eq system-type 'darwin)
               "pbpaste"
             "xclip -o -selection clipboard"))))

(require 'ibuffer)
(defun --new-shell ()
  "Open a shell window in the directory of the buffered file.
If there are no other windows, create one; otherwise use `other-window'."
  (interactive)
  (let ((bufname (or (ibuffer-buffer-file-name) "~/")))
    (if (= 1 (length (window-list)))
        (select-window (split-window-sensibly))
      (other-window 1))
    (let ((default-directory (file-name-directory bufname)))
      (shell))))

(defun --solarized-light ()
  "Switch to the light version of Solarized."
  (interactive)
  (setq frame-background-mode 'light)
  (load-theme 'solarized t))

(defun --solarized-dark ()
  "Switch to the dark version of Solarized."
  (interactive)
  (setq frame-background-mode 'dark)
  (load-theme 'solarized t))

(defun --fuck-you ()
  "Because it won't work on init with Debian Terminal."
  (interactive)
  (setq solarized-termcolors 256)
  (setq frame-background-mode 'light)
  (load-theme 'solarized t))

(defun --firefox (url &optional new-window)
  "Open the URL in firefox.exe, optionally in a NEW-WINDOW."
  (let ((ff-path (if (getenv "WSL_DISTRO_NAME")
                    "/mnt/c/Program\\ Files/Mozilla\\ Firefox/firefox.exe "
                  "firefox")))
    (shell-command (concat ff-path url))))

(defun --shell-command-replace-region (start end command
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
