;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialization
;; set path
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; general packages
(require 'cl)
(require 'thingatpt)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
;; recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
;; color theme
(require 'color-theme)
(color-theme-initialize)
;;(unless window-system
;;  (color-theme-billw))
(color-theme-billw)
;; insert file path
(load-library "insert-path")
;; give me ido
(require 'ido)
    (ido-mode t)
    (setq ido-enable-flex-matching t) ;; enable fuzzy matching
(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10))

;;(server-start)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general settings
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (if window-system
                                   (scroll-bar-mode -1)))
;;(unless window-system
(toggle-menu-bar-mode-from-frame)
;;)

(defun toggle-vert-max ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))
(if (window-system)
    (toggle-vert-max))

(if (window-system)
  (set-frame-width (selected-frame) 90))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(setq visible-bell t
      echo-keystrokes 0.1
      inhibit-startup-message t
      inhibit-splash-screen t
      transient-mark-mode t
      color-theme-is-global t
      shift-select-mode nil
      mouse-yank-at-point t
      require-final-newline t
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'forward
      whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100
      ediff-window-setup-function 'ediff-setup-windows-plain
      xterm-mouse-mode t
      ring-bell-function 'ignore
      delete-old-versions t
      column-number-mode t
      x-select-enable-clipboard t) ;; make emacs use the clipboard

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general editing settings

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(ansi-color-for-comint-mode-on)


;; Transparently open compressed files
(auto-compression-mode t)

(require 'saveplace)
(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(setq-default save-place t)                   ;; activate it for all buffers


;; Always do syntax highlighting
(cond ((fboundp 'global-font-lock-mode)
       ;; turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       ;; maximum colors
       (setq font-lock-maximum-decoration t)))

;; Also highlight parens
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

;; auto open closing parentheses when open
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers 

; Use C-xaig to correct common typos
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
(setq default-abbrev-mode t)            ;; turn it on
(setq save-abbrevs nil)                 ;; don't ask
(add-hook 'kill-emacs-hook             ;; write when ...
  'write-abbrev-file)                  ;; ... exiting emacs

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

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

(defun insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    ;; TODO: switch to nxml/nxhtml mode
    (cond ((search-forward "<?xml" nil t) (xml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VC
(require 'magit)
(setq user-mail-address "accidentalbits@googlemail.com")
(setq user-full-name "Chris Martel")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YaSnippet
(require 'yasnippet-bundle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text and related modes
(add-hook 'text-mode-hook 'text-mode-hook-identify)
(setq default-major-mode 'text-mode)
;; give me auto-fill in Text-Mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; give me spell checking
(setq-default ispell-program-name "aspell")
; double space after colon
(setq colon-double-space t)
(require 'flyspell)
(add-to-list 'auto-mode-alist '("\\.txt$" . flyspell-mode))
(add-to-list 'auto-mode-alist '("\\.txt$" . footnote-mode))
(add-to-list 'auto-mode-alist '("\\.txt$" . orgstruct-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; t2t mode
(setq auto-mode-alist 
      (append (list '("\\.t2t$" . t2t-mode))
              (if (boundp 'auto-mode-alist) auto-mode-alist)))
(autoload 't2t-mode "txt2tags-mode" "Txt2tags Mode" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown mode
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
  (cons '("\\.mdn" . markdown-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Muse
;; simple markup authoring system
(require 'muse-mode)     ; lade den authoring mode
(require 'muse-html)     ; lade die relevanten Stile
(require 'muse-latex)
(require 'muse-texinfo)
(require 'muse-docbook)
;;(require 'muse-project)  ; publish files in projects


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tex-related stuff
; yes we want auto parse for style hooks
(setq TeX-parse-self t)
(setq TeX-auto-save t)
; yes, we want RefTex with all our Latex files
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode
(setq reftex-plug-into-auctex t)
; yes, we want Source-Specials in our DVIs
(setq TeX-source-specials-mode t)
(setq TeX-pdf-mode nil)
; give me xelatex if using fontspec
;(add-to-list 'LaTeX-command-style
;             '("\\`fontspec\\'" "xelatex %S%(PDFout)"))
; yes, we want line-breaking in tex-files:
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'latex-mode-hook 'turn-on-auto-fill)
; we like aspell better than ispell
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
; yes, we don't want to press two times for local quotes
(setq TeX-quote-after-quote nil)
;; math stuff
(setq TeX-electric-sub-and-superscript t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Orgmode
(setq load-path (cons "~/.emacs.d/lisp/org/lisp" load-path))
(setq load-path (cons "~/.emacs.d/lisp/org/contrib/lisp" load-path))
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-agenda-files '("~/Org/mygtd.org"
                         "~/Org/jobs.org"
                         "~/Org/studium.org"
                         "~/Org/coding-projekte.org"))
(setq org-agenda-include-diary t)
(setq org-default-notes-file '("~/Org/notes.org"))
(setq org-log-done 'time)
;; shows gtd-file with Alt-gtd
(defun gtd ()
  (interactive)
  (find-file "/home/chm/Org/mygtd.org"))
;; show agenda on startup
;;(add-hook 'after-init-hook 'org-agenda-list)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Org/mygtd.org" "Aufgaben")
         "* TODO %?\n  %i\n  %a")
        ("m" "Musiktipp" entry (file+headline "~/Org/medien.org" "Musik") 
         "** CHECK %x\n   %t\n  %^g  %^{Artist}p\n%^{Title}p\n%^{Date}p\n%^{Where}p\n\n%^{Price}p\n%^{Review}p" :prepend t)
        ("b" "Buchtipp" entry (file+headline "~/Org/medien.org" "Buch") 
         "** CHECK %x\n   %t\n  %^g  %^{Artist}p\n%^{Title}p\n%^{Date}p\n%^{Where}p\n\n%^{Price}p\n%^{Review}p" :prepend t)
        ("v" "TV-Tipp" entry (file+headline "~/Org/medien.org" "TV") 
         "** CHECK %x\n   %t\n  %a\n  %^g  %^{Title}p\n%^{Where}p\n%^{Review}p" :prepend t)))
;; Set to the location of your Org files on your local system
(setq org-directory "~/Org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BBDB
(require 'bbdb)
(bbdb-initialize)
;; bbdb wanderlust integration
;;(require 'bbdb-wl)
;;(bbdb-wl-setup)
;; enable pop-ups
;;(setq bbdb-use-pop-up t)
;; auto collection
;(setq bbdb/mail-auto-create-p t)
;; exceptional folders against auto collection
;;(setq bbdb-wl-ignore-folder-regexp "^@")
;;(setq bbdb-wl-ignore-folder-regexp "^-")
;;(setq signature-use-bbdb t)
(setq bbdb-north-american-phone-numbers-p nil)
;; shows the name of bbdb in the summary :-)
;;(setq wl-summary-from-function 'bbdb-wl-from-func)
;; automatically add mailing list fields
;;(add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)
;;(setq bbdb-auto-notes-alist '(("X-ML-Name" (".*$" ML 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calendar
; Pfad zur Kalenderdatei in welcher die Daten gespeichert werden.
; Die .diary Datei im Homeverzeichniss.
(setq diary-file "~/Org/diary")
; Kalender Benachrichtigung bei Ereignissen
(setq display-time-24hr-format t)
(display-time)
(add-hook 'diary-hook' appt-make-list)
; Europaeisches Kalender Aussehen verwenden.
(setq european-calendar-style t)
; Anzeige der Zeit beim Kalendermodus in der Statusleiste.
(setq calendar-time-display-form 
      '(24-hours ":" minutes (if time-zone " (") time-zone (if time-zone")")))
; Anzeigen von Datum beim Kalendermodus in der Statusleiste.
(setq calendar-date-display-form 
      '((if dayname (concat dayname ", ")) day " " monthname " " year))
; Unterstreicht im Kalendermodus den heutigen Tag.
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
; Eine Woche beginnt in der Kalenderansicht mit Montag als ersten Tag
; anstelle von Sontag.
(setq calendar-week-start-day 1)
; Fuer deutsche Monatsnamen im Kalender.
;(setq calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai" "Juni" "Juli" "August" "September" "Oktober" "November" "Dezember"])
;(setq calendar-day-name-array ["Sonntag" "Montag" "Dienstag" "Mittwoch" "Donnerstag" "Freitag" "Samstag"])
; Eintraege im Kalender markieren, kann auch manuell mit der Taste M
; gemacht werden.
; Hier wird automatisch nach dem Start alles Markiert wo Eintraege
; vorhanden sind.
(setq mark-diary-entries-in-calendar t)
; Koordinaten für Sunrise
(setq calendar-latitude 48.8)
(setq calendar-longitude 9.2)
(setq calendar-location-name "Stuttgart, GE")
; show me diary entries a week
(setq number-of-diary-entries 3)
(add-hook 'diary-display-hook 'fancy-diary-display)
;;(diary) ; show diary at startup

;; give me g-client for google support
; doesn't work yet, lib is installed, but authentication fails with
; type error (stringp)
;(load-library "g")
;(setq g-user-email "christoph.martel@gmail.com")
;(gcal-emacs-calendar-setup)
;(setq g-html-handler 'w3m-buffer)
;(setq g-html-handler 'w3m-browse-url)
;(setq g-html-handler 'browse-url)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  MULTIMEDIA
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emms-stuff
;; simple setup for emms
(require 'emms-setup)
(require 'emms-info-libtag)
;(require 'emms-player-mpg321-remote)
;(emms-all)
(emms-devel)
(emms-default-players)
(push 'emms-player-mpg321 emms-player-list)
(push 'emms-player-mplayer emms-player-list)
(push 'emms-player-mplayer-playlist emms-player-list)
; lastfm interface
(setq emms-lastfm-username "uname"
      emms-lastfm-password "secret")
(setq emms-stream-bookmarks-file "~/.emacs.d/emms-streams")
; mpd interface
(require 'emms-player-mpd)
(setq emms-player-mpd-server-name "localhost")
(setq emms-player-mpd-server-port "6600")
(add-to-list 'emms-info-functions 'emms-info-mpd)
(add-to-list 'emms-player-list 'emms-player-mpd)
(setq emms-player-mpd-music-directory "/home/chm/Musik")
(setq emms-source-file-default-directory "/home/chm/Musik")
;(setq emms-volume-amixer-control "Front")
;;;;;
; dired play/add media files
(defun dired-emms-add-file ()
  "Adds the current file or directory to the EMMS playing list."
  (interactive)
  (let ((fname (dired-get-filename)))
    (if (string= (downcase (substring fname -4 nil)) ".m3u")
	(emms-add-m3u-playlist fname)
      (emms-add-file fname))))

(defun dired-emms-play-file ()
  "Plays the current file or directory."
  (interactive)
  (let ((fname (dired-get-filename)))
    (if (string= (downcase (substring fname -4 nil)) ".m3u")
	(emms-play-m3u-playlist fname)
      (emms-play-file fname))))
(define-key dired-mode-map "w" 'dired-emms-add-file)
(define-key dired-mode-map "r" 'dired-emms-play-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  INTERNET
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WWW / Browser
;; w3m
(if (= emacs-major-version 23)
    (progn
      (add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m")
      (require 'w3m-load))
(require 'w3m))
(setq mm-text-html-renderer 'w3m)
(require 'mime-w3m)  
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)
;; newsticker
(autoload 'w3m-region "w3m"
  "Render region in current buffer and replace with result." t)
(autoload 'w3m-toggle-inline-image "w3m"
  "Toggle the visibility of an image under point." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; E-mail / Mutt integration
;; autoload configuration
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-hook 'mail-mode-hook 'turn-on-auto-fill)
(add-hook 'mail-mode-hook 'flyspell-mode)
(add-hook 'mail-mode-hook 'footnote-mode)
(add-hook 'mail-mode-hook 'orgstruct-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; E-mail / Wanderlust
;; autoload configuration
;; (Not required if you have installed Wanderlust as XEmacs package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general stuff
;;
;; Directory where icons are placed.
;; Default: the peculiar value to the running version of Emacs.
;; (Not required if the default value points properly)
;; (setq wl-icon-directory "~/work/wl/etc")
;;(autoload 'wl "wl" "Wanderlust" t)
;;(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
;;(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)


;;enable wanderlust as default mail composer
;; (autoload 'wl-user-agent-compose "wl-draft" nil t)
;; (if (boundp 'mail-user-agent)
;;    (setq mail-user-agent 'wl-user-agent))
;; (if (fboundp 'define-mail-user-agent)
;;    (define-mail-user-agent
;;      'wl-user-agent
;;      'wl-user-agent-compose
;;      'wl-draft-send
;;      'wl-draft-kill
;;      'mail-send-hook))

;; (setq wl-mime-charset 'utf-8)
;; (set-language-environment 'utf-8) ; Default would be utf8
;; (setq default-mime-charset-for-write 'utf-8)
;; (setq default-mime-charset 'utf-8)
;; (setq wl-summary-width 'nil)
;; (setq wl-summary-weekday-name-lang 'de)
;; (setq elmo-folder-update-confirm 'nil)
;; ;; default folder setup
;; (setq wl-default-spec "%")
;; (setq wl-default-folder "Gmail")
;; (setq wl-draft-folder ".~/Dokumente/Entwürfe")
;; (setq wl-trash-folder "%[Gmail]/Papierkorb")
;; (setq wl-folder-check-async t) 
;; (setq elmo-passwd-alist-file-name "/home/chm/.elmo/passwd")

;; ;; no verbose user agent field
;; (setq wl-generate-mailer-string-function
;;       'wl-generate-user-agent-string-1)
;; ;; Set mail-icon to be shown universally in the modeline.
;; (setq global-mode-string
;;       (cons
;;        '(wl-modeline-biff-status
;;          wl-modeline-biff-state-on
;;          wl-modeline-biff-state-off)
;;        global-mode-string))
;; ;; Biff
;; (setq wl-biff-check-folder-list
;;         '("%inbox:christoph.martel@gmail.com/clear@imap.googlemail.com:993!"
;; ;        "-feu.mathematik.kurs.1141.diskussion:q8222665@news.fernuni-hagen.de"
;; ;        "-feu.mathematik.kurs.1142:q8222665@news.fernuni-hagen.de"
;; ;        "-feu.informatik.kurs.1613:q8222665@news.fernuni-hagen.de"
;; ;        "-comp.lang.python"
;; ;        "-comp.lang.javascript"
;; ;        "-comp.os.linux.advocacy"
;; ;        "-comp.os.linux.setup"
;;         )
;;       wl-biff-check-interval 180
;;       wl-biff-use-idle-timer t)

;; (defun my-wl-update-current-summaries ()
;;   (let ((buffers (wl-collect-summary)))
;;     (while buffers
;;       (with-current-buffer (car buffers)
;;         (save-excursion
;;           (wl-summary-sync-update)))
;;       (setq buffers (cdr buffers)))))

;; (add-hook
;;  'wl-biff-notify-hook
;;  '(lambda ()
;;     (my-wl-update-current-summaries)
;;     ))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; NNTP server for news posting. Default: `nil'
;; ;;
;; (setq wl-nntp-posting-server "news.individual.de")
;; (setq elmo-nntp-default-server "news.individual.de")
;; (setq elmo-nntp-default-user "christophmartel")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; IMAP
;; (setq elmo-imap4-default-server "imap.gmail.com")
;; (setq elmo-imap4-default-user "christoph.martel@gmail.com") 
;; (setq elmo-imap4-default-authenticate-type 'clear) 
;; (setq elmo-imap4-default-port '993)
;; (setq elmo-imap4-default-stream-type 'ssl)
;; (setq elmo-imap4-use-modified-utf7 t) 

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; SMTP server for mail posting. Default: `nil'
;; ;;
;; (setq wl-smtp-connection-type 'starttls)
;; (setq wl-smtp-posting-port 587)
;; (setq wl-smtp-authenticate-type "plain")
;; (setq wl-smtp-posting-user "christoph.martel@gmail.com")
;; (setq wl-smtp-posting-server "smtp.gmail.com")
;; (setq wl-local-domain "gmail.com")
;; (setq wl-from "christoph.martel@gmail.com")
;; (setq wl-message-id-domain "gmail.com")
;; (setq wl-message-id-use-wl-from t)
;; (setq wl-interactive-send t)

;;GnuPG support with mailcrypt and wanderlust
;; (load-library "mailcrypt")
;; (mc-setversion "gpg")
;; (add-hook 'wl-summary-mode-hook 'mc-install-read-mode)
;; (add-hook 'wl-mail-setup-hook 'mc-install-write-mode)

;; (defun mc-wl-verify-signature ()
;;  (interactive)
;;  (save-window-excursion
;;    (wl-summary-jump-to-current-message)
;;    (mc-verify)))

;; (defun mc-wl-decrypt-message ()
;;  (interactive)
;;  (save-window-excursion
;;    (wl-summary-jump-to-current-message)
;;    (let ((inhibit-read-only t))
;;      (mc-decrypt))))

;; (eval-after-load "mailcrypt"
;;   '(setq mc-modes-alist
;;        (append
;;         (quote
;;          ((wl-draft-mode (encrypt . mc-encrypt-message)
;;             (sign . mc-sign-message))
;;           (wl-summary-mode (decrypt . mc-wl-decrypt-message)
;;             (verify . mc-wl-verify-signature))))
;;         mc-modes-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Identi.ca
(require 'identica-mode)
(when (require 'netrc nil t)
  (autoload 'identica-mode "identica-mode" nil t)
  (let ((identica (netrc-machine (netrc-parse "~/.netrc") "identi.ca" t))) ; remove this `t' if you didn't specify a port
    (setq identica-password (netrc-get identica "password") ; if it's last, avoid doing C-M-x in public spaces at least ;-)
          identica-username (netrc-get identica "login"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blogging / Authoring
(require 'weblogger)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  CODING
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun turn-on-hl-line-mode ()
  (if window-system (hl-line-mode t))
;;    (set-face-background 'hl-line "#d4f8ff"))
    (set-face-background 'hl-line "#222222"))

(defun turn-on-save-place-mode ()
  (setq save-place t))

(defun turn-on-whitespace ()
  (whitespace-mode t))

(defun turn-on-paredit ()
  (paredit-mode t))

(defun turn-off-tool-bar ()
  (tool-bar-mode -1))

(defun turn-on-linenumbers ()
  (linum-mode t))

(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(add-hook 'coding-hook 'local-column-number-mode)
(add-hook 'coding-hook 'local-comment-auto-fill)
(add-hook 'coding-hook 'turn-on-hl-line-mode)
(add-hook 'coding-hook 'turn-on-save-place-mode)
(add-hook 'coding-hook 'pretty-lambdas)
(add-hook 'coding-hook 'add-watchwords)
(add-hook 'coding-hook 'turn-on-linenumbers)


(defun run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'coding-hook))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup chm viewer
(require 'chm-view)
;(setq browse-url-browser-function ‘w3m-browse-url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fedora spec file support
;; http://tihlde.org/~stigb/rpm-spec-mode.el
(autoload 'rpm-spec-mode "rpm-spec-mode.el" "RPM spec mode." t)
(setq auto-mode-alist (append '(("\\.spec" . rpm-spec-mode)) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GNU build system
(setq auto-mode-alist
    (append '(
        ("configure.in" . m4-mode)
	("\\.m4\\’" . m4-mode)
	("\\.am\\’" . makefile-mode))
    auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; devhelp integration
;; by Richard Hult <richard@imendio.com>
(defun devhelp-word-at-point ()
  "runs devhelp"
  (interactive)
  (start-process-shell-command "devhelp" nil "devhelp" "-s" (current-word))
  )
;; ; Example: bind F7 to start devhelp and search for the word at the point.
(global-set-key [f7] 'devhelp-word-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; HTML Helper mode
;; (setq load-path (cons "path_to_html-helper-mode_directory" load-path))
;; (autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;; (add-hook 'html-helper-mode-hook 'run-coding-hook)
;; (setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
;; ;Recognize server-parsed HTML files
;; (setq auto-mode-alist (cons '("\\.shtml$" . html-helper-mode) auto-mode-alist))
;; ;Insert new document HTML template
;; (setq html-helper-build-new-buffer t)
;; ;Insert address
;; (setq html-helper-address-string 
;;	"<a href=\"http://www.cmartel.de\">Christoph Martel ;;&lt;c.martel@gmx.net&gt;</a>")
;; ;Enable time stamp
;; (setq html-helper-do-write-file-hooks t)
;; ;Enable hyperlink prompt
;; (setq tempo-interactive t)
;; ;Enable PHP support
;; (require 'php-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML / HTML
(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
                   'nxml-mode))
(unify-8859-on-decoding-mode)
(setq magic-mode-alist
	  (cons '("<＼＼?xml " . nxml-mode)
            magic-mode-alist))
(fset 'xml-mode 'nxml-mode)
;(fset 'html-mode 'nxml-mode)
;; use nxhtml for web dev
;;(load "~/.emacs.d/lisp/nxhtml/autostart.el")
;; Conditional load of nxhtml because its really heavy, but really good.
(setq *nxhtml-autostart-file* (expand-file-name "~/.emacs.d/lisp/nxhtml/autostart.el"))
(defun load-nxhtml-if-required ()
  (if (and (string-match ".+\\.\\(php\\|html\\)$" (buffer-file-name))
           (not (featurep 'nxhtml-autostart)))
      (progn 
        (load *nxhtml-autostart-file*)
        (nxhtml-mumamo-mode)))) ;; mumamo loads nxhtml-mode et al
(add-hook 'find-file-hook 'load-nxhtml-if-required)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript mode
(autoload 'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))
(add-hook 'espresso-mode-hook 'moz-minor-mode)
(add-hook 'espresso-mode-hook 'esk-paredit-nonlisp)
(add-hook 'espresso-mode-hook 'run-coding-hook)
(setq espresso-indent-level 2)

(eval-after-load 'espresso
  '(progn (define-key espresso-mode-map "{" 'paredit-open-curly)
          (define-key espresso-mode-map "}" 'paredit-close-curly-and-newline)
          ;; fixes problem with pretty function font-lock
          (define-key espresso-mode-map (kbd ",") 'self-insert-command)
          (font-lock-add-keywords
           'espresso-mode `(("\\(function *\\)("
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "ƒ")
                                       nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pascal mode
(add-to-list 'auto-mode-alist '("\\.pas$" . pascal-mode))
(autoload 'pascal-mode "pasc-mode" "Pascal Mode." t)
(add-hook 'pascal-mode-hook 'run-coding-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme/Lisp stuff
;; set scheme backend
(setq scheme-program-name "mit-schem")
(add-hook 'scheme-mode-hook 'run-coding-hook)

;; You want quack. Really.
(require 'quack)
;; but not always
(setq quack-fontify-style 'plt
      quack-default-program "stk"
      quack-newline-behavior 'newline-indent
      quack-programs '("mit-schem" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -M errortrace" "rs" "scheme" "scheme48" "scsh" "sisc" "stk" "stk-simply" "stklos" "sxi"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ Stuff
(setq load-path (cons "~/.emacs.d/lisp/cc-mode" load-path))
(require 'cc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lua stuff
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-hook 'lua-mode-hook 'turn-on-font-lock)
(add-hook 'lua-mode-hook 'hs-minor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BACKMATTER
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load key bindings
(require 'key-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save easy customization interface stuff here:
(setq custom-file "~/.emacs-custom.el")
(load custom-file)  ; overriding .emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start in org scratch buffer
(switch-to-buffer (get-buffer-create (generate-new-buffer-name "*org-scratch*")))
;;(insert "Scratch buffer with org-mode.\n\n")
(org-mode)
