;;; ../projects/the-livingroom/sys-config/emacs-configs/doom-emacs-config.doom.d/org.d/scala-config.el -*- lexical-binding: t; -*-
;;;
;;;
(after! projectile
  (add-to-list 'projectile-project-root-files "build.sbt"))


;;; Packages
;; Enable scala-mode for highlighting, indentation and motion commands
(use-package! scala-mode
  :interpreter
  ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package! sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  )

(after! scala-mode

  (setq-hook! 'scala-mode-hook
    comment-line-break-function #'+scala-comment-indent-new-line-fn)

  (setq-hook! 'scala-mode-hook lsp-enable-indentation nil)
  (add-hook 'scala-mode-local-vars-hook #'lsp!)

  (use-package! sbt-mode
    :after scala-mode
    :config (set-repl-handler! 'scala-mode #'+scala/open-repl :persist t))

  ;; Keep auto-save/backup files separate from source code:  https://github.com/scalameta/metals/issues/1027
  (setq
   backup-directory-alist `((".*" . ,temporary-file-directory))
   auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


  ;; Enable nice rendering of diagnostics like compile errors.
  ;; (use-package flycheck
  ;;   :init (global-flycheck-mode))

  ;; Uncomment following section if you would like to tune lsp-mode performance according to
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  ;;       (setq gc-cons-threshold 100000000) ;; 100mb
  ;;       (setq read-process-output-max (* 1024 1024)) ;; 1mb
  ;;       (setq lsp-idle-delay 0.500)
  ;;       (setq lsp-log-io nil)
  ;;       (setq lsp-completion-provider :capf)

  ;; Add metals backend for lsp-mode
  (use-package lsp-metals
    :config (setq lsp-metals-treeview-show-when-views-received t))

  ;; Enable nice rendering of documentation on hover
  ;;   Warning: on some systems this package can reduce your emacs responsiveness significally.
  ;;   (See: https://emacs-lsp.github.io/lsp-mode/page/performance/)
  ;;   In that case you have to not only disable this but also remove from the packages since
  ;;   lsp-mode can activate it automatically.
  (use-package lsp-ui)

  ;; lsp-mode supports snippets, but in order for them to work you need to use yasnippet
  ;; If you don't want to use snippets set lsp-enable-snippet to nil in your lsp-mode settings
  ;;   to avoid odd behavior with snippets and indentation
  (use-package yasnippet)

  ;; Add company-lsp backend for metals.
  ;;   (depending on your lsp-mode version it may be outdated see:
  ;;    https://github.com/emacs-lsp/lsp-mode/pull/1983)
  (use-package company-lsp)

  ;; Use the Debug Adapter Protocol for running tests and debugging
  (use-package posframe
    ;; Posframe is a pop-up tool that must be manually installed for dap-mode
    )
  (use-package dap-mode
    :hook
    (lsp-mode . dap-mode)
    (lsp-mode . dap-ui-mode)
    )

  ;; LSP :: You can configure this warning with the `lsp-enable-file-watchers' and `lsp-file-watch-threshold' variables

  ;; (defcustom lsp-file-watch-ignored-directories
  (message "running custom config-scala-mode")

  ;; (add-hook 'scala-mode-hook 'turn-on-auto-revert-mode)

  ;; When a buffer is reverted, a message is generated.  This can be
  ;; suppressed by setting ‘auto-revert-verbose’ to nil.

  ;; Use ‘global-auto-revert-mode’ to automatically revert all buffers.
  ;; Use ‘auto-revert-tail-mode’ if you know that the file will only grow
  ;; without being changed in the part that is already in the buffer.

  ;; Automatically insert asterisk in a comment when enabled
  (defun scala/newline-and-indent-with-asterisk ()
    (interactive)
    (newline-and-indent)
    (when scala-auto-insert-asterisk-in-comments
      (scala-indent:insert-asterisk-on-multiline-comment)))

  (evil-define-key 'insert scala-mode-map
		   (kbd "RET") 'scala/newline-and-indent-with-asterisk)

  ;; ;;(evil-define-key 'normal scala-mode-map "J" 'spacemacs/scala-join-line)


  ;; --"md"  'lsp-metals-doctor-run       ;; Execute Doctor
  ;; --"mt"  'lsp-metals-treeview         ;; Switch to treeview
  ;; --"mbi" 'lsp-metals-build-import  ;; Unconditionally run `sbt bloopInstall` and re-connect to the build server."
  ;; --"mbc" 'lsp-metals-build-connect ;; Unconditionally cancel existing build server connection and re-connect."
  ;; --"mbs" 'lsp-metals-bsp-switch  ;; Interactively switch between BSP servers.
  ;; --"mss" 'lsp-metals-sources-scan  ;; Walk all files in the workspace and index where symbols are defined."
  ;; --"mrc" 'lsp-metals-reset-choice  ;; Reset a decision you made about different settings. E.g. If you choose to import workspace with sbt you can decide to reset and change it again."

  (setq scala-indent:step 2
	scala-indent:indent-value-expression nil
	scala-indent:align-parameters nil
	scala-indent:align-forms t

	;; defconst scala-indent:eager-strategy 0
	;; defconst scala-indent:operator-strategy 1
	;; defconst scala-indent:reluctant-strategy 2
	scala-indent:default-run-on-strategy scala-indent:operator-strategy

	scala-indent:add-space-for-scaladoc-asterisk t
	scala-indent:use-javadoc-style nil
	)


  )
