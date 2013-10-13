;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Basic Editor Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Font And Frame Setup
(set-face-attribute 'default nil :font  "-apple-Inconsolata-medium-normal-normal-*-*-180-*-*-m-0-iso10646-1")
;;(setq default-frame-alist '( (top . 20)(left . 20)
;;                             (width . 160) (height . 50)))
;;(setq initial-frame-alist '( (top . 20) (left . 20)
;;                             (width . 160) (height . 50)))

(setq-default cursor-type 'bar)
(setq indent-tabs-mode nil)
(show-paren-mode t)
(setq nxml-child-indent 4)

;; Spelling
;;;; ASpell for osx setup
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
(put 'dired-find-alternate-file 'disabled nil)

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
 '(auto-complete erlang scala-mode tidy rainbow-mode inf-ruby go-mode protobuf-mode cedet))

(setq column-number-mode t)

(setq visible-bell t)
(tool-bar-mode 0)

;;;; PRELUDE
(setq prelude-whitespace nil)
(add-hook 'text-mode-hook 'turn-off-auto-fill)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Programing Languages And Code Suppport
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; CEDET
(semantic-mode 1)
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
;;(global-srecode-minor-mode 1)            ; Enable template insertion menu
;;(semantic-load-enable-minimum-features) ;; or enable more if you wish

;;;; ERLANG
(add-hook 'erlang-mode-hook (lambda () (setq truncate-lines t)))
(require 'erlang-start)

;;;; GO
(require 'go-mode-load)
(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

;;;; RUBY
 (autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(autoload 'inf-ruby-keys "inf-ruby" "" t)
(eval-after-load 'ruby-mode '(add-hook 'ruby-mode-hook 'inf-ruby-keys))

;;;; SCALA
(require 'scala-mode-auto)
(add-hook 'scala-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

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

;;;; MAC KEYS dont think i use these anymore
;;(global-set-key [s-left] 'beginning-of-line)
;;(global-set-key [s-right] 'end-of-line)

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


;;;; JAVA
;;;; AJC - disabled for now. trying malabar
;;(require 'ajc-java-complete-config)
;;(add-hook 'java-mode-hook 'ajc-java-complete-mode)
;;(add-hook 'find-file-hook 'ajc-4-jsp-find-file-hook)
;;
;;(ac-set-trigger-key "TAB")
;;(setq ac-auto-start nil)
;;(setq ac-menu-height 20)
;;;; malabar-mode
;;(add-to-list 'load-path (concat prelude-dir "vendor/malabar-1.5-SNAPSHOT/lisp"))
;;(require 'malabar-mode)
;;(setq malabar-groovy-lib-dir (concat prelude-dir "vendor/malabar-1.5-SNAPSHOT/lib"))
;;(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
(require 'semantic/db-javap)
;;(ede-java-root-project "TestProject"
;;                       :file "~/Documents/workspace/gerrit/unuServer"
;;                       :srcroot '("src/main" "src/test"))
;;)

;;;; WEB
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
