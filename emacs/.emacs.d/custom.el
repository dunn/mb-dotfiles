(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-arguments
   (quote
    ("--line-number" "--smart-case" "--nogroup" "--column" "--stats" "--hidden" "--")))
 '(ag-highlight-search t)
 '(ag-ignore-list (quote (".git")))
 '(as-continuation-offset 2)
 '(as-indent-offset 2)
 '(async-bytecomp-package-mode t)
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
 '(current-language-environment "UTF-8")
 '(elfeed-curl-program-name "/usr/local/opt/curl/bin/curl")
 '(erc-autojoin-channels-alist (quote (("freenode.net" "#machomebrew" "#code4lib"))))
 '(erc-autojoin-mode t)
 '(erc-button-mode t)
 '(erc-fill-mode t)
 '(erc-irccontrols-mode t)
 '(erc-list-mode t)
 '(erc-match-mode t)
 '(erc-menu-mode t)
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring services stamp track)))
 '(erc-move-to-prompt-mode t)
 '(erc-netsplit-mode t)
 '(erc-networks-mode t)
 '(erc-nick "dunndunndunn")
 '(erc-noncommands-mode t)
 '(erc-pcomplete-mode t)
 '(erc-port 6697)
 '(erc-prompt-for-nickserv-password nil)
 '(erc-prompt-for-password nil)
 '(erc-readonly-mode t)
 '(erc-ring-mode t)
 '(erc-server "irc.freenode.net")
 '(erc-services-mode t)
 '(erc-stamp-mode t)
 '(erc-track-minor-mode t)
 '(erc-track-mode t)
 '(fountain-export-title-page-title-template "${title}
${credit}
${author}
${source}")
 '(global-homebrew-mode t)
 '(global-mark-ring-max 100)
 '(gnutls-trustfiles (quote ("/etc/ssl/certs/system.pem")))
 '(gnutls-verify-error t)
 '(homebrew-cache-dir "~/Library/Caches/Homebrew")
 '(homebrew-default-args (quote ("-v" "-s" "--sandbox")))
 '(homebrew-poet-executable "/usr/local/bin/poet")
 '(indent-tabs-mode nil)
 '(irony-server-install-prefix "/usr/local/opt/irony-mode/bin")
 '(ivy-mode t)
 '(ivy-use-virtual-buffers t)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-strict-missing-semi-warning nil)
 '(magit-completing-read-function (quote ivy-completing-read))
 '(magit-process-popup-time 5)
 '(magit-use-overlays nil)
 '(mark-ring-max 100)
 '(markdown-enable-math t)
 '(menu-bar-mode t)
 '(message-hidden-headers
   (quote
    ("^User-Agent:" "^References:" "^Face:" "^X-Face:" "^X-Draft-From:")))
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
 '(package-directory-list nil)
 '(package-selected-packages
   (quote
    (pass nix-mode company-nixos-options slime slime-autoloads rubocop htmlize rspec-mode ruby-tools json-mode diffview suggest pkg-info list-unicode-display company-web company-php company-ansible exec-path-from-shell ansible-doc php-mode markdown-mode flycheck company neotree swiper cmake-mode notmuch pdf-tools yaml-mode elisp-slime-nav web-mode unkillable-scratch typo scss-mode robe rainbow-mode nlinum markdown-toc magit js2-mode homebrew-mode gitignore-mode gitconfig-mode gitattributes-mode gist fountain-mode flycheck-package flycheck-cask elfeed editorconfig diff-hl counsel company-emoji beacon ag)))
 '(package-user-dir "")
 '(pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
 '(perl-indent-level 2)
 '(require-final-newline t)
 '(robe-completing-read-func (quote ivy-completing-read))
 '(scss-compile-at-save nil)
 '(send-mail-function (quote sendmail-send-it))
 '(sendmail-program "msmtp")
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(show-trailing-whitespace t)
 '(standard-indent 2)
 '(tab-width 2)
 '(tls-checktrust t)
 '(tls-program
   (quote
    ("gnutls-cli --x509cafile /etc/ssl/certs/system.pem -p %p %h")))
 '(tramp-connection-timeout 15)
 '(tramp-use-ssh-controlmaster-options t)
 '(tramp-verbose 6)
 '(twittering-convert-fix-size 32)
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
 '(default ((t (:height 170 :family "Inconsolata"))))
 '(js2-error ((t (:foreground "#dc322f"))))
 '(js2-external-variable ((t (:foreground "#cb4b16"))))
 '(js2-function-call ((t (:inherit default :foreground "#6c71c4"))))
 '(js2-function-param ((t (:foreground "#d33682" :slant italic))))
 '(js2-instance-member ((t (:foreground "#d33682"))))
 '(js2-jsdoc-html-tag-delimiter ((t (:foreground "#859900"))))
 '(js2-jsdoc-html-tag-name ((t (:foreground "#b58900"))))
 '(notmuch-crypto-part-header ((t (:foreground "DodgerBlue1"))))
 '(web-mode-symbol-face ((t (:foreground "#d33682")))))
