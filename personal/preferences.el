;;;; PRELUDE
(add-hook 'prog-mode-hook 'prelude-turn-off-whitespace t)


;;;; FULLL SCREEEENN
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)

;; Emacs 24 Package ext
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package))))
 '(erlang scala-mode tidy rainbow-mode))

(setq column-number-mode t)

(setq visible-bell t)
(tool-bar-mode 0)

;;;; Frame setup
(setq default-frame-alist '( (top . 20)(left . 20)
                             (width . 160) (height . 56)))
(setq initial-frame-alist '( (top . 20) (left . 20)
                             (width . 160) (height . 56)))

(setq-default cursor-type 'bar)
(setq indent-tabs-mode nil)
(show-paren-mode t)
(setq nxml-child-indent 4)

;; Spelling
;;;; ASpell for osx setup
;;(add-to-list 'exec-path "/usr/local/bin")
(setq ispell-dictionary "english"
      ;;ispell-list-command "list"
      ispell-dictionary-alist
      (let ((default '("[A-Za-z]" "[^A-Za-z]" "[']" nil
                       ("-B" "-d" "english" "--dict-dir"
                        "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
                       nil iso-8859-1)))
        `((nil ,@default)
          ("english" ,@default))))
;; This requires aspell, ispell or cocoAspell on os x
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'java-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'erlang-mode-hook (lambda () (flyspell-prog-mode)))

;;Make dired happy with my system
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

;;;; SCALA
(require 'scala-mode-auto)
(add-hook 'scala-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;;;; ERLANG
(add-hook 'erlang-mode-hook (lambda () (setq truncate-lines t)))
(require 'erlang-start)

(put 'dired-find-alternate-file 'disabled nil)

;;;; GO
(require 'go-mode-load)
(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

;;;; TIDY
(require 'tidy)
(autoload 'tidy-buffer "tidy" "Run Tidy HTML parser on current buffer" t)
(autoload 'tidy-parse-config-file "tidy" "Parse the `tidy-config-file'" t)
(autoload 'tidy-save-settings "tidy" "Save settings to `tidy-config-file'" t)
(autoload 'tidy-build-menu  "tidy" "Install an options menu for HTML Tidy." t)

(defun my-html-mode-hook () "Customize my html-mode."
(tidy-build-menu html-mode-map)
(local-set-key [(control c) (control c)] 'tidy-buffer)
(setq sgml-validate-command "tidy"))
(add-hook 'html-mode-hook 'my-html-mode-hook)

;;;; MAC KEYS
(global-set-key [s-left] 'beginning-of-line)
(global-set-key [s-right] 'end-of-line)

;;;; SQLPLUS
(require 'sqlplus)
(add-to-list 'auto-mode-alist '("\\.sqp\\'" . sqlplus-mode))
(require 'plsql)
(setq auto-mode-alist
      (append '(("\\.pls\\'" . plsql-mode) ("\\.pkg\\'" . plsql-mode)
                ("\\.pks\\'" . plsql-mode) ("\\.pkb\\'" . plsql-mode)
                ("\\.sql\\'" . plsql-mode) ("\\.PLS\\'" . plsql-mode)
                ("\\.PKG\\'" . plsql-mode) ("\\.PKS\\'" . plsql-mode)
                ("\\.PKB\\'" . plsql-mode) ("\\.SQL\\'" . plsql-mode)
                ("\\.prc\\'" . plsql-mode) ("\\.fnc\\'" . plsql-mode)
                ("\\.trg\\'" . plsql-mode) ("\\.vw\\'" . plsql-mode)
                ("\\.PRC\\'" . plsql-mode) ("\\.FNC\\'" . plsql-mode)
                ("\\.TRG\\'" . plsql-mode) ("\\.VW\\'" . plsql-mode))
              auto-mode-alist ))
