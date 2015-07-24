;;; package --- emacs configuration
;;; Commentary:

;;; Code:
;; import PATH etc; necessary since emacs starts as a daemon before
;; .bash_profile is run
(setenv "PATH" "/Users/cat/bin:/usr/local/opt/go/libexec/bin:/usr/local/sbin:/usr/local/bin:~/.cabal/bin::/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/usr/texbin")
(setq exec-path (append exec-path '("/Users/cat/bin:/usr/local/opt/go/libexec/bin:/usr/local/sbin:/usr/local/bin:~/.cabal/bin::/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/usr/texbin")))

;; https://github.com/magnars/.emacs.d/blob/master/init.el
(setq inhibit-startup-message t)

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Homebrew executables and lisp files
(add-to-list 'load-path "/usr/local/bin/")
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Solarized theme
;; https://github.com/sellout/emacs-color-theme-solarized/issues/141#issuecomment-71862293
(add-to-list 'custom-theme-load-path "/usr/local/share/emacs/site-lisp/solarized")
;; `t` is important: http://stackoverflow.com/a/8547861
(load-theme 'solarized t)
(setq solarized-termcolors 256)
(add-to-list 'default-frame-alist '(background-mode . dark))

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(blink-cursor-mode 0)

;; http://ergoemacs.org/emacs/emacs_make_modern.html
(global-linum-mode 1)
(column-number-mode 1)

;; Commenting
(global-set-key "\C-c;" 'comment-region)
(global-set-key "\C-c:" 'uncomment-region)

;; select all
(global-set-key "\C-x\C-a" 'mark-whole-buffer)
(global-unset-key "\C-xh")

;; Kill whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Add new lines automatically
(setq next-line-add-newlines t)

;; Faster redrawing
(setq redisplay-dont-pause nil)

;; Tabs (no)
(setq-default indent-tabs-mode nil)

(require 'ido)
(ido-mode t)

;; Auto fill mode
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'tex-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

;; magit
;; brew install magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(setq magit-last-seen-setup-instructions "1.4.0")

;; editorconfig
;; needs its hand held with a special load-path
(add-to-list 'load-path "/usr/local/opt/editorconfig-emacs/share/emacs/site-lisp/editorconfig-emacs")
(load "editorconfig")

;; Better Meta
(require 'smex)
(setq mac-option-modifier 'meta)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-x C-M") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(setq smex-show-unbound-commands t)

;; Spelling
(setq-default ispell-program-name "aspell")
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
(autoload 'tex-mode-flyspell-verify "flyspell" "" t)

;; case handling
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; word count
(autoload 'word-count-mode "word-count"
  "Minor mode to count words."
  t nil)
(global-set-key "\M-+" 'word-count-mode)

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Take a multi-line paragraph and make it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

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

(defun autotools ()
  "For Homebrew HEAD builds."
  (interactive)
  (insert "depends_on \"automake\" => :build\n"
          "    depends_on \"autoconf\" => :build\n"
          "    depends_on \"libtool\" => :build"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODES ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org-mode (installed from dunn/emacs homebrew tap)
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

;; table mode
(require 'table)
(add-hook 'text-mode-hook 'table-recognize)
(add-hook 'markdown-mode-hook 'table-recognize)

;; Mail mode
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-hook 'mail-mode-hook 'turn-on-auto-fill)

;; git-modes
(require 'gitattributes-mode)
(require 'gitconfig-mode)
(require 'gitignore-mode)
(add-to-list 'auto-mode-alist '("^\\.gitattributes$" . gitattributes-mode))
(add-to-list 'auto-mode-alist '("^\\.gitconfig$" . gitconfig-mode))
(add-to-list 'auto-mode-alist '("^\\.gitignore$" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.git\/info\/attributes$" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.git\/config$" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.git\/info\/exclude$" . gitignore-mode))

;; Web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; HTML autocomplete
(require 'ac-html)
(add-hook 'html-mode-hook 'ac-html-enable)
(add-to-list 'web-mode-ac-sources-alist
  '("html" . (
               ;; attribute-value better to be first
               ac-source-html-attribute-value
               ac-source-html-tag
               ac-source-html-attribute)))

;; Markdown mode
;; installed from dunn/emacs --with-markdown-plus
(require 'markdown-mode)
(require 'markdown-mode+)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Fountain mode
(require 'fountain-mode)
(add-to-list 'auto-mode-alist '("\\.fountain$" . fountain-mode))

;; SCSS mode
(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . scss-mode))

;; Rainbow mode
(require 'rainbow-mode)
(add-hook 'scss-mode-hook 'scss-rainbow-hook)
(defun scss-rainbow-hook ()
  "Colorize color strings."
  (rainbow-mode 1))

;; PHP mode
(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;; JS mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'interpreter-mode-alist '("iojs" . js2-mode))

;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'browse-kill-ring)

(require 'anzu)
(global-anzu-mode +1)
(set-face-attribute 'anzu-mode-line nil
  :foreground "#586e75" :weight 'bold)

(require 'achievements)

(require 'unkillable-scratch)
(unkillable-scratch 1)
(add-to-list 'unkillable-scratch "\*.*\*")
(setq unkillable-scratch-behavior 'bury)

(require 'which-key)
(which-key-mode)

(require 'applescript-mode)
(add-to-list 'auto-mode-alist '("\.applescript$" . applescript-mode))
(add-to-list 'interpreter-mode-alist '("osascript" . applescript-mode))

(require 'aggressive-indent)
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

;;; .emacs.el ends here
