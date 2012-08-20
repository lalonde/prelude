;; Start speedbar and tabbar

(setq EmacsPortable-global-tabbar 't) ; If you want tabbar

(require 'tabbar-ruler)

(defun my-tabbar-buffer-groups ()
   "Return the list of group names the current buffer belongs to.
 This function is a custom function for tabbar-mode's tabbar-buffer-groups.
 This function group all buffers into 3 groups:
 Those Dired, those user buffer, and those emacs buffer.
 Emacs buffer are those starting with “*”."
   (list
    (cond
     ((string-equal "*" (substring (buffer-name) 0 1))
      '"Emacs Buffer"
      )
     ((string-match ".+\\.sqp" (buffer-name))
      '"SQLPlus"
      )
     ((eq major-mode 'dired-mode)
      '"Dired"
      )
     (t
      '"User Buffer"
      )
     )))

(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)


(global-set-key [M-s-left] 'tabbar-backward)
(global-set-key [M-s-right] 'tabbar-forward)
(global-set-key [M-s-up] 'tabbar-forward-group)
(global-set-key [M-s-down] 'tabbar-backward-group)

;; (setq tabbar-buffer-groups-function
;;      (lambda ()
;;	(list "All")))

;; Spelling
;; This requires aspell, ispell or cocoAspell on os x
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'java-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'erlang-mode-hook (lambda () (flyspell-prog-mode)))
