(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(ag-arguments
     (quote
       ("--line-number" "--smart-case" "--nogroup" "--column" "--stats" "--hidden" "--")))
 '(ag-highlight-search t)
 '(as-continuation-offset 2)
 '(as-indent-offset 2)
 '(beacon-size 30)
  '(company-emoji-aliases
     (quote
       ((:man-woman-boy: . ":family:")
         (:woman-kiss-man: . ":couplekiss:")
         (:woman_man_holding_hands: . ":couple:")
         (:woman-heart-man: . ":couple_with_heart:")
         (:oop: . ":speak_no_evil:")
         (:middle-finger: . ":reversed-hand-with-middle-finger-extended:"))))
 '(company-emoji-insert-unicode t)
 '(company-idle-delay 0.3)
 '(company-minimum-prefix-length 2)
 '(counsel-find-file-at-point t)
 '(css-indent-offset 2)
 '(global-homebrew-mode t)
 '(global-mark-ring-max 100)
 '(gnutls-trustfiles (quote ("/etc/ssl/certs/system.pem")))
 '(gnutls-verify-error t)
 '(homebrew-default-args (quote ("-v" "-s" "--sandbox")))
 '(homebrew-poet-executable "/usr/local/bin/poet")
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(magit-completing-read-function (quote ivy-completing-read))
 '(magit-use-overlays nil)
 '(mark-ring-max 100)
 '(markdown-enable-math t)
 '(menu-bar-mode t)
 '(message-send-mail-function (quote message-send-mail-with-sendmail))
 '(message-sendmail-envelope-from (quote header))
 '(notmuch-address-command "~/bin/lbdbq_no_err")
 '(notmuch-search-oldest-first nil)
  '(notmuch-tag-formats
     (quote
       (("unread"
          (notmuch-apply-face tag
            (quote
              (:foreground "dark cyan"))))
         ("flagged"
           (notmuch-tag-format-image-data tag
             (notmuch-tag-star-icon)))
         ("pay"
           (notmuch-apply-face tag
             (quote
               (:foreground "light green")))))))
  '(package-selected-packages
     (quote
       (pdf-tools yaml-mode elisp-slime-nav web-mode unkillable-scratch typo scss-mode robe rainbow-mode nlinum markdown-toc magit js2-mode homebrew-mode gitignore-mode gitconfig-mode gitattributes-mode gist fountain-mode flycheck-package flycheck-cask elfeed editorconfig diff-hl counsel company-emoji beacon ag)))
 '(pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
 '(perl-indent-level 2)
 '(require-final-newline t)
 '(robe-completing-read-func (quote ivy-completing-read))
 '(scss-compile-at-save nil)
 '(send-mail-function (quote sendmail-send-it))
 '(sendmail-program "/usr/local/bin/msmtp")
 '(sh-indentation 2)
 '(show-trailing-whitespace t)
 '(standard-indent 2)
 '(tab-width 2)
 '(tls-checktrust t)
  '(tls-program
     (quote
       ("gnutls-cli --x509cafile /etc/ssl/certs/system.pem -p %p %h")))
 '(tramp-connection-timeout 10)
 '(tramp-verbose 6)
 '(typo-language "English")
 '(url-queue-timeout 10)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-sql-indent-offset 2)
 '(x-stretch-cursor t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "monotype" :family "PT Mono"))))
 '(js2-error ((t (:foreground "#dc322f"))))
 '(js2-external-variable ((t (:foreground "#cb4b16"))))
 '(js2-function-call ((t (:inherit default :foreground "#6c71c4"))))
 '(js2-function-param ((t (:foreground "#d33682" :slant italic))))
 '(js2-instance-member ((t (:foreground "#d33682"))))
 '(js2-jsdoc-html-tag-delimiter ((t (:foreground "#859900"))))
 '(js2-jsdoc-html-tag-name ((t (:foreground "#b58900"))))
 '(notmuch-crypto-part-header ((t (:foreground "DodgerBlue1"))))
 '(web-mode-symbol-face ((t (:foreground "#d33682")))))
