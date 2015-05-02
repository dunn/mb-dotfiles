;;; package --- emacs configuration
;;; Commentary:
;; Have emacs --daemon run at startup; otherwise the package
;; initialization will slow every launch

;;; Code:
;; import PATH etc; necessary since emacs starts as a daemon before
;; .bash_profile is run
(setenv "PATH" "/Users/cat/bin:/usr/local/opt/go/libexec/bin:/usr/local/opt/php56/bin:/usr/local/bin:~/.cabal/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/usr/texbin:/Users/cat/bin:/usr/local/opt/go/libexec/bin:/usr/local/opt/php56/bin:~/.cabal/bin")
(setq exec-path (append exec-path '("/Users/cat/bin:/usr/local/opt/go/libexec/bin:/usr/local/opt/php56/bin:/usr/local/bin:~/.cabal/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/usr/texbin:/Users/cat/bin:/usr/local/opt/go/libexec/bin:/usr/local/opt/php56/bin:~/.cabal/bin")))

;; https://github.com/magnars/.emacs.d/blob/master/init.el
(setq inhibit-startup-message t)

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

;; shell access
(global-set-key (kbd "C-z") 'shell)

;; Do the packaging dance
(require 'package)
(package-initialize)
(add-to-list 'package-archives
  '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
;; This makes starting emacs slow as ass unless you do emacs --daemon
;; at startup
(package-refresh-contents)

;; Homebrew packages
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")

;; Solarized theme
(if (not (package-installed-p 'solarized-theme))
    (progn
      (package-install 'solarized-theme)))
(load-theme 'solarized-dark t)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(blink-cursor-mode 0)

;; http://ergoemacs.org/emacs/emacs_make_modern.html
(global-linum-mode 1)
(column-number-mode 1)

;; Commenting
(global-set-key "\C-c;"      'comment-region)
(global-set-key "\C-c:"      'uncomment-region)

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

;; editorconfig
(if (not (package-installed-p 'editorconfig))
    (progn
      (package-install 'editorconfig)))
(load "editorconfig")

;; Flycheck
(if (not (package-installed-p 'flycheck))
    (progn
      (package-install 'flycheck)))
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Better Meta
(if (not (package-installed-p 'smex))
    (progn
      (package-install 'smex)))
(setq mac-option-modifier 'meta)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-x C-M") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(setq smex-show-unbound-commands t)

;; Multiple cursors
;; https://github.com/kjhealy/emacs-starter-kit/blob/master/starter-kit-bindings.org
;; When you have an active region that spans multiple lines, the
;; following will add a cursor to each line:
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)
;; When you want to add multiple cursors not based on continuous
;; lines, but based on keywords in the buffer, use:
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; Rectangular region mode
(global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)
;; Mark more like this
(global-set-key (kbd "H-a") 'mc/mark-all-like-this)
(global-set-key (kbd "H-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "H-n") 'mc/mark-next-like-this)
(global-set-key (kbd "H-S-n") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "H-S-a") 'mc/mark-all-in-region)

;; Spelling
(setq-default ispell-program-name "aspell")
(add-to-list 'load-path "/usr/local/bin")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODES ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; table mode
(require 'table)
(add-hook 'text-mode-hook 'table-recognize)
(add-hook 'markdown-mode-hook 'table-recognize)

;; Mail mode
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-hook 'mail-mode-hook 'turn-on-auto-fill)

;; Web mode
(if (not (package-installed-p 'web-mode))
    (progn
      (package-install 'web-mode)))
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; HTML autocomplete
(if (not (package-installed-p 'ac-html))
    (progn
      (package-install 'ac-html)))
(add-hook 'html-mode-hook 'ac-html-enable)
(add-to-list 'web-mode-ac-sources-alist
  '("html" . (
               ;; attribute-value better to be first
               ac-source-html-attribute-value
               ac-source-html-tag
               ac-source-html-attribute)))

;; Markdown mode
(if (not (package-installed-p 'markdown-mode))
    (progn
      (package-install 'markdown-mode)))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Fountain mode
(if (not (package-installed-p 'fountain-mode))
    (progn
      (package-install 'fountain-mode)))
(add-to-list 'auto-mode-alist '("\\.fountain$" . fountain-mode))

;; SCSS mode
(if (not (package-installed-p 'scss-mode))
    (progn
      (package-install 'scss-mode)))
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . scss-mode))

;; Rainbow mode
(if (not (package-installed-p 'rainbow-mode))
    (progn
      (package-install 'rainbow-mode)))
(add-hook 'scss-mode-hook 'scss-rainbow-hook)
(defun scss-rainbow-hook ()
  "Colorize color strings."
  (rainbow-mode 1))

;; PHP mode
(if (not (package-installed-p 'php-mode))
    (progn
      (package-install 'php-mode)))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;; JS mode
(if (not (package-installed-p 'js2-mode))
    (progn
      (package-install 'js2-mode)))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'interpreter-mode-alist '("iojs" . js2-mode))

;; Go mode
(if (not (package-installed-p 'go-mode))
    (progn
      (package-install 'go-mode)))
(require 'go-mode-autoloads)

; Olivetti
(if (not (package-installed-p 'olivetti))
    (progn
      (package-install 'olivetti)))

;; textmate minor mode
;; (add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
;; (require 'textmate)
;; (textmate-mode)

;; ;; Set to the location of your Org files on your local system
;; (setq org-directory "~/org")
;; ;; Set to the name of the file where new notes will be stored
;; (setq org-mobile-inbox-for-pull "~/org/flagged.org")
;; ;; Set to <your Dropbox root directory>/MobileOrg.
;; (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; org-mode
(if (not (package-installed-p 'org))
    (progn
      (package-install 'org)))
;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-indent-offset 2)
 '(frame-background-mode nil)
 '(js2-basic-offset 2)
 '(markdown-enable-math t)
 '(menu-bar-mode t)
 '(perl-indent-level 2)
 '(require-final-newline t)
 '(scss-compile-at-save nil)
 '(x-stretch-cursor t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "monotype" :family "Consolas"))))
 '(js2-error ((t (:foreground "#dc322f"))))
 '(js2-external-variable ((t (:foreground "#cb4b16"))))
 '(js2-function-call ((t (:inherit default :foreground "#6c71c4"))))
 '(js2-function-param ((t (:foreground "#d33682" :slant italic))))
 '(js2-instance-member ((t (:foreground "#d33682"))))
 '(js2-jsdoc-html-tag-delimiter ((t (:foreground "#859900"))))
 '(js2-jsdoc-html-tag-name ((t (:foreground "#b58900")))))

(provide 'emacs)
;;; .emacs.el ends here
