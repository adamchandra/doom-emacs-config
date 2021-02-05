;;; config.el -*- lexical-binding: t; -*-

;; [[file:../config.org::*Personal Information][Personal Information:1]]
(setq user-full-name "adamchandra"
      user-mail-address "adamchandra@gmail.com")
;; Personal Information:1 ends here

;; [[file:../config.org::*Font Face][Font Face:1]]
(setq doom-font (font-spec :family "JetBrains Mono" :size 24)
      doom-big-font (font-spec :family "JetBrains Mono" :size 36)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 24)
      doom-serif-font (font-spec :family "Bitstream Vera Sans" :weight 'light))
;; Font Face:1 ends here

;; [[file:../config.org::*Theme and modeline][Theme and modeline:1]]
(add-to-list 'custom-theme-load-path "themes")
(setq doom-theme 'leuven-solar)
;; (setq doom-theme 'doom-solarized-light)
;; (delq! t custom-theme-load-path)
;; Theme and modeline:1 ends here

;; [[file:../config.org::*Package Config][Package Config:1]]
(use-package! terminal-here
  :defer t
  )
(use-package! auto-save-buffers-enhanced
  :config (progn
    (auto-save-buffers-enhanced t)
    (auto-save-buffers-enhanced-include-only-checkout-path nil)
    (setq auto-save-buffers-enhanced-interval 0.5) ;; seconds
    (setq auto-save-buffers-enhanced-quiet-save-p t)
    ;; (setq auto-save-buffers-enhanced-include-regexps '(".+"))
     ;; (setq auto-save-buffers-enhanced-exclude-regexps '("^not-save-file" "\\.ignore$"))

            )
  )
;; Package Config:1 ends here

;; [[file:../config.org::*Spacemacs Copypasta][Spacemacs Copypasta:1]]
(defun spacemacs/alternate-window ()
  "Switch back and forth between current and last window in the current frame."
  (interactive)
  (let (;; switch to first window previously shown in this frame
        (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
    (unless prev-window (user-error "Last window not found."))
    (select-window prev-window)))


(defun spacemacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the current window.

If `spacemacs-layouts-restrict-spc-tab' is `t' then this only switches between
the current layouts buffers."
  (interactive)
  (cl-destructuring-bind (buf start pos)
      (if (bound-and-true-p spacemacs-layouts-restrict-spc-tab)
          (let ((buffer-list (persp-buffer-list))
                (my-buffer (window-buffer window)))
            ;; find buffer of the same persp in window
            (seq-find (lambda (it) ;; predicate
                        (and (not (eq (car it) my-buffer))
                             (member (car it) buffer-list)))
                      (window-prev-buffers)
                      ;; default if found none
                      (list nil nil nil)))
        (or (cl-find (window-buffer window) (window-prev-buffers)
                     :key #'car :test-not #'eq)
            (list (other-buffer) nil nil)))
    (if (not buf)
        (message "Last buffer not found.")
      (set-window-buffer-start-and-point window buf start pos))))



  (defun spacemacs/helm-M-x-fuzzy-matching ()
    "Helm M-x with fuzzy matching enabled"
    (interactive)
    (let ((completion-styles completion-styles))
      (add-to-list 'completion-styles `,(if (version< emacs-version "27") 'helm-flex 'flex) t)
      (call-interactively 'helm-M-x)))

  (defun my-split-window-horizontally()
      (interactive)
      (doom/window-maximize-buffer)
      (split-window-right)
      (balance-windows)
      ;; (spacemacs/window-split-single-column)
    )
;; Spacemacs Copypasta:1 ends here

;; [[file:../config.org::*Spacemacs Copypasta][Spacemacs Copypasta:2]]
(defun spacemacs/scala-join-line ()
    "Adapt `scala-indent:join-line' to behave more like evil's line join.

    `scala-indent:join-line' acts like the vanilla `join-line',
    joining the current line with the previous one. The vimmy way is
    to join the current line with the next.

    Try to move to the subsequent line and then join. Then manually move
    point to the position of the join."
      (interactive)
      (let (join-pos)
        (save-excursion
          (goto-char (line-end-position))
          (unless (eobp)
            (forward-line)
            (call-interactively 'scala-indent:join-line)
            (setq join-pos (point))))

        (when join-pos
          (goto-char join-pos))))


  (defun find-my-init-files ()
    (interactive)
    (find-file-existing *acs-layer-path* )
    )

  (defun find-my-theme-file ()
    "find the directory containing private init files"
    (interactive)
    (progn
      (find-file-existing *theme-path*)
      ))

(defun enhanced-save-buffer ()
  (interactive)
  (progn
    (delete-trailing-whitespace)
    ;; (pcase (file-name-extension buffer-file-name)
    ;;               ("ts" (if (fboundp 'prettier-js) (prettier-js)))
    ;;               ("tsx" (if (fboundp 'prettier-js) (prettier-js)))
    ;;               (_ ()))
    (save-buffer)
    ))
;; Spacemacs Copypasta:2 ends here

;; [[file:../config.org::*Spacemacs Copypasta][Spacemacs Copypasta:3]]
(setq *adams-config-ran* nil)

(defun disable-autosave ()
  (progn
    (setq auto-save-buffers-enhanced-activity-flag nil)
    ))

(defun enable-autosave ()
  (progn
    (setq auto-save-buffers-enhanced-activity-flag t)
    ))



  (defun adamchandra/final-config ()
    (interactive)
    (if (not *adams-config-ran*)
        (progn
          (message "adamchandra/final-config running")
          (setq *adams-config-ran* t)

          ;; -- ;; (add-hook 'dired-mode-hook #'my-dired-config)

          ;; -- ;; (require 'org-config)
          ;; -- ;; (require 'ts-config)
          ;; -- ;; (require 'translate-funcs)
          ;; -- ;; (require 'livedown)

          ;; -- ;; (config-markdown-mode)

          ;; -- ;; ;; prevent .#filname.xx files (which cause a problem w/ensime)
          ;; -- ;; (setq create-lockfiles nil)

          ;; -- ;; (spacemacs/set-leader-keys
          ;; -- ;;   "bk" 'spacemacs/kill-this-buffer
          ;; -- ;;   )


          (add-hook! evil-insert-state-entry 'disable-autosave)
          (add-hook! evil-insert-state-exit 'enable-autosave)
          ;; -- ;; ;; (add-hook 'evil-insert-state-exit-hook 'autosave-file-buffer)
          ;; -- ;; (setq dotspacemacs-auto-save-file-location nil)

          ;; -- ;; (setq history-delete-duplicates t)

          ;; -- ;; (spacemacs/toggle-smooth-scrolling-off)

          ;; -- ;; (setq truncate-lines t)

          ;; -- ;; (remove-hook 'prog-mode-hook 'auto-complete-mode)
          ;; -- ;; (remove-hook 'prog-mode-hook 'rainbow-delimiters-mode)


          ;; -- ;; (menu-bar-mode -1)
          ;; -- ;; (tool-bar-mode -1)
          ;; -- ;; (scroll-bar-mode -1)

          ;; -- ;; ;; Defined in `helm-ag.el'.
          ;; -- ;; ;; Ignore patterns for `ag'. This parameters are specified as --ignore
          ;; -- ;; ;; (setq helm-ag-ignore-patterns nil)
          ;; -- ;; (setq helm-ag-use-grep-ignore-list t)
          ;; -- ;; ;; Use .agignore where is at project root if it exists.
          ;; -- ;; (setq helm-ag-use-agignore t)

          ;; -- ;; ;; (setq browse-url-browser-function 'browse-url-generic
          ;; -- ;; ;;       browse-url-generic-program "google-chrome")
          ;; -- ;; ;; (setq browse-url-browser-function 'browse-url-generic
          ;; -- ;; ;;       browse-url-generic-program "firefox")

          ;; -- ;; (setq browse-url-browser-function
          ;; -- ;;       '(("^mailto:" . browse-url-mail)
          ;; -- ;;         ("." . browse-url-firefox))
          ;; -- ;;       )

          ;; -- ;; (setq browse-url-firefox-program "firefox")

          ;; -- ;; ;; defined in `grep.el'.
          ;; -- ;; (setq grep-find-ignored-directories
          ;; -- ;;       '("target" ".ensime_cache"
          ;; -- ;;         "SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" ;; defaults
          ;; -- ;;         ))

          ;; -- ;; (setq grep-find-ignored-files
          ;; -- ;;       '(".#*" "*.lock" "*.gen" "*.si" "*.cfs" "*.cfe" "*.hi" "*.o" "*~" "*.bin"
          ;; -- ;;         "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo"
          ;; -- ;;         "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem"
          ;; -- ;;         "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl"
          ;; -- ;;         "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl"
          ;; -- ;;         "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl"
          ;; -- ;;         "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux"
          ;; -- ;;         "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns"
          ;; -- ;;         "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo"
          ;; -- ;;         ))



          ;; -- ;; (remove-hook 'js2-mode-hook 'skewer-mode)
          ;; -- ;; (remove-hook 'js2-mode-hook 'js2-imenu-extras-mode)

          ;; -- ;; (eval-after-load 'tern
          ;; -- ;;      '(progn
          ;; -- ;;         (require 'tern-auto-complete)
          ;; -- ;;         (tern-ac-setup)))

          ;; -- ;; ;; (setq javascript-disable-tern-port-files t)

          ;; -- ;; (add-hook 'js2-mode-hook
          ;; -- ;;           (defun my-js2-mode-setup ()
          ;; -- ;;             ;; (require 'tern)
          ;; -- ;;             (flycheck-mode t)
          ;; -- ;;             (auto-complete-mode t)
          ;; -- ;;             (when (executable-find "eslint")
          ;; -- ;;               (flycheck-select-checker 'javascript-eslint))
          ;; -- ;;             ))

          ;; -- ;; (global-display-line-numbers-mode)
          ;; -- ;; (spacemacs/toggle-smartparens-globally-on)
          ;; -- ;; (show-smartparens-global-mode)

          ;; -- ;; (global-auto-revert-mode)
          ;; -- ;; (setq auto-revert-verbose t)

          ;; -- ;; ;; (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
          ;; -- ;; ;; (global-visual-fill-column-mode)

          ;; -- ;; (fringe-mode '(24 . 14))
          ;; -- ;; (setq visual-line-fringe-indicators '(left-curly-arrow nil))

          ;; -- ;; ;; Warning (yasnippet): ‘Snippet’ modified buffer in a backquote expression.
          ;; -- ;; ;; To hide this warning, add (yasnippet backquote-change) to ‘warning-suppress-types’.
          ;; -- ;; (setq warning-suppress-types '(yasnippet backquote-change))

          ;; -- ;; (setq bookmark-default-file "~/.emacs.d/bookmarks")  ;;define file to use.
          ;; -- ;; (setq bookmark-save-flag 1)  ;save bookmarks to .emacs.bmk after each entry

          (message "adamchandra/final-config (done) running")

          )

      (progn
        (message "adamchandra/final-config *not* running, already ran")
        )
      )
    )
;; Spacemacs Copypasta:3 ends here

;; [[file:../config.org::*Scala Config][Scala Config:1]]
;; LSP :: You can configure this warning with the `lsp-enable-file-watchers' and `lsp-file-watch-threshold' variables

;; (defcustom lsp-file-watch-ignored-directories
(after! lsp-metals
  (setq lsp-file-watch-ignored-directories
        (append lsp-file-watch-ignored-directories
                '(; SCM tools
                  "[/\\\\]\\.bsp\\'"
                  "[/\\\\].*corpus\\.d\\'"
                  )
                )
        )
  )

(after! scala-mode
  (progn
    (message "running acs-config-scala-mode")

    (add-hook 'scala-mode-hook 'turn-on-auto-revert-mode)

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

    ;; (lsp-file-watch-ignored)

    ;; (spacemacs/declare-prefix-for-mode 'scala-mode "m" nil)
    ;; (spacemacs/declare-prefix-for-mode 'scala-mode "mm" "")
    (map! :map scala-mode-map
          :localleader
          :desc "errors"                                      "ee"  #'lsp-ui-flycheck-list
          :desc "errors"                                      "eq"  #'lsp-ui-flycheck-list--quit
          :desc "Format buffer"                               "fb"  #'lsp-format-buffer
          :desc "Format buffer"                               "rp"  #'lsp-format-buffer
          :desc "Format region or line"                       "fr"  #'lsp-format-region
          :desc "Goto declarations of symbol under point"     "gd"  #'lsp-find-declaration
          :desc "Goto definitions of symbol"                  "gf"  #'lsp-find-definition
          :desc "Goto implementations of symbol under point"  "gi"  #'lsp-find-implementation
          :desc "Goto references to symbol under point"       "gr"  #'lsp-find-references
          :desc "Goto type definitions of symbol under point" "gt"  #'lsp-find-type-definition
          )

    ;;       (map! :after python
    ;;         :map python-mode-map
    ;;         :localleader
    ;;         (:prefix ("i" . "imports")
    ;;           :desc "Sort imports"      "s" #'py-isort-buffer
    ;;           :desc "Sort region"       "r" #'py-isort-region)))


    ;; (evil-leader/set-key
    ;; --(spacemacs/set-leader-keys-for-major-mode 'scala-mode

    ;; --"esl"  'lsp-switch-to-io-log-buffer ;; Switch to log View IO logs for workspace
    ;; --"etl"  'lsp-lens-mode               ;; Toggle Code Lenses
    ;; --"etd"  'lsp-describe-thing-at-point ;; Describe thing at point
    ;; --"ea"  'lsp-execute-code-action     ;; Execute code action
    ;; --;; "e"  'lsp-document-highlight      ;; Highlight references to symbol under point
    ;; --;; "e"  'lsp-rename                  ;; Rename symbol under point
    ;; --;; "e"  'lsp-shutdown-workspace      ;; Shutdown language server
    ;; --"udd"  'lsp-ui-doc-show
    ;; --"upr"  'lsp-ui-peek-find-references
    ;; --"upd"  'lsp-ui-peek-find-definitions
    ;; --
    ;; --"exs"  'lsp-restart-workspace       ;; Execute (Restart) language server
    ;; --"md"  'lsp-metals-doctor-run       ;; Execute Doctor
    ;; --"mt"  'lsp-metals-treeview         ;; Switch to treeview
    ;; --"mbi" 'lsp-metals-build-import  ;; Unconditionally run `sbt bloopInstall` and re-connect to the build server."
    ;; --"mbc" 'lsp-metals-build-connect ;; Unconditionally cancel existing build server connection and re-connect."
    ;; --"mbs" 'lsp-metals-bsp-switch  ;; Interactively switch between BSP servers.
    ;; --"mss" 'lsp-metals-sources-scan  ;; Walk all files in the workspace and index where symbols are defined."
    ;; --"mrc" 'lsp-metals-reset-choice  ;; Reset a decision you made about different settings. E.g. If you choose to import workspace with sbt you can decide to reset and change it again."
    ;; --)

    ;; lsp-find-definition
    (evil-define-key 'normal scala-mode-map
      (kbd "M-.") 'lsp-find-definition
      )

    (setq scala-indent:step 2
          scala-indent:indent-value-expression nil
          scala-indent:align-parameters nil
          scala-indent:align-forms t

          ;; (defconst scala-indent:eager-strategy 0
          ;; (defconst scala-indent:operator-strategy 1
          ;; (defconst scala-indent:reluctant-strategy 2
          scala-indent:default-run-on-strategy scala-indent:operator-strategy

          scala-indent:add-space-for-scaladoc-asterisk t
          scala-indent:use-javadoc-style nil
          )


    (setq lsp-ui-sideline-enable t                ;;   "Whether or not to enable ‘lsp-ui-sideline’."
          ;; lsp-ui-sideline-ignore-duplicate nil    ;;   "Control to ignore duplicates when there is a same symbol with the same contents."
          ;; lsp-ui-sideline-show-symbol t           ;;   "When t, show the symbol name on the right of the information."
          ;; lsp-ui-sideline-show-hover t            ;;   "Whether to show hover messages in sideline."
          ;; lsp-ui-sideline-show-diagnostics t      ;;   "Whether to show diagnostics messages in sideline."
          ;; lsp-ui-sideline-show-code-actions t     ;;   "Whether to show code actions in sideline."
          ;; lsp-ui-sideline-update-mode 'line       ;;   "Define the mode for updating sideline information.
          ;; lsp-ui-sideline-delay 0.2               ;;   "Number of seconds to wait before showing sideline."
          ;; lsp-ui-sideline-diagnostic-max-lines 20 ;;   "Maximum number of lines to show of diagnostics in sideline."
          );

    (setq lsp-ui-doc-enable t              ;;   "Whether or not to enable lsp-ui-doc."
          lsp-ui-doc-header nil            ;;   "Whether or not to enable the header which display the symbol string."
          lsp-ui-doc-include-signature t   ;;   "Whether or not to include the object signature/type in the frame."
          lsp-ui-doc-position 'at-point      ;;   "Where to display the doc. top|bottom|at-point
          lsp-ui-doc-border "blue"        ;;   "Border color of the frame."
          lsp-ui-doc-max-width 150         ;;   "Maximum number of columns of the frame."
          lsp-ui-doc-max-height 30         ;;   "Maximum number of lines in the frame."
          lsp-ui-doc-use-childframe t      ;;   "Whether to display documentation in a child-frame or the current frame.
          lsp-ui-doc-use-webkit t        ;;   "Whether to display documentation in a WebKit widget in a child-frame.
          lsp-ui-doc-delay 1.0             ;;   "Number of seconds before showing the doc."
          )

    (setq lsp-ui-flycheck-enable t            ;;   "Whether or not to enable ‘lsp-ui-flycheck’."
          lsp-ui-flycheck-live-reporting t      ;;   "If non-nil, diagnostics in buffer will be reported as soon as possible.
          lsp-ui-flycheck-list-position 'bottom ;;   "Position where `lsp-ui-flycheck-list' will show diagnostics for the whole workspace. (bottom|right)
          )

    (setq lsp-ui-imenu-enable nil
          lsp-ui-imenu-kind-position 'top                 ;;   "Where to show the entries kind."
          lsp-ui-imenu-colors '("deep sky blue" "green3") ;;   "Color list to cycle through for entry groups."
          )

    (setq lsp-ui-peek-enable t           ;;   "Whether or not to enable ‘lsp-ui-peek’."
          lsp-ui-peek-peek-height 20     ;;   "Height of the peek code."
          lsp-ui-peek-list-width 50      ;;   "Width of the right panel."
          lsp-ui-peek-fontify 'on-demand ;;   "Whether to fontify chunks of code (use semantics colors).
          lsp-ui-peek-always-show nil    ;;   "Show the peek view even if there is only 1 cross reference.
          )

    )
  )
;; Scala Config:1 ends here

;; [[file:../config.org::*Keymap Definitions][Keymap Definitions:1]]
;; originally from magnars and modified by ffevotte for dedicated windows
;; support, it has quite diverged by now
(defun spacemacs/rotate-windows-forward (count)
  "Rotate each window forwards.
A negative prefix argument rotates each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (let* ((non-dedicated-windows (cl-remove-if 'window-dedicated-p (window-list)))
         (states (mapcar #'window-state-get non-dedicated-windows))
         (num-windows (length non-dedicated-windows))
         (step (+ num-windows count)))
    (if (< num-windows 2)
        (error "You can't rotate a single window!")
      (dotimes (i num-windows)
        (window-state-put
         (elt states i)
         (elt non-dedicated-windows (% (+ step i) num-windows)))))))
;; Keymap Definitions:1 ends here

;; [[file:../config.org::*Keymap Definitions][Keymap Definitions:2]]
;; (after! snipe
;;   (setq evil-snipe-override-evil-repeat-keys nil)
;;   (setq doom-localleader-key ",")
;;   (setq doom-localleader-alt-key "M-,")
;;   )

(setq evil-snipe-override-evil-repeat-keys nil)
(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-,")


(after! winum
  (map! :leader
        :desc "Win Split Horizontal"          "w2"   #'my-split-window-horizontally
        :desc "Win Rotate Fwd"                "wr"   #'spacemacs/rotate-windows-forward
        )
  )

(map! :leader
      :desc "M-x w/fuzzy matching"          "SPC"  #'spacemacs/helm-M-x-fuzzy-matching
      :desc "Find file in project"          "pf"   #'projectile-find-file
      :desc "Search for symbol in project"  "/"    #'+default/search-project-for-symbol-at-point
      :desc "Search project"                "*"    #'+default/search-project
      :desc "Previous Buffer"               "TAB"  #'spacemacs/alternate-buffer
      )

(map! ;;
 "C-x C-s" #'enhanced-save-buffer )
;; Keymap Definitions:2 ends here

;; [[file:../config.org::*Keymap Definitions][Keymap Definitions:3]]
;; (define-key! :states 'normal :keymaps dired-mode-map
;;    "i" 'dired-subtree-insert
;;    "t" 'dired-subtree-toggle
;;    (kbd "TAB") 'dired-subtree-cycle
;;    )
;; Keymap Definitions:3 ends here

;; [[file:../config.org::*Run Final Config][Run Final Config:1]]
(adamchandra/final-config)
;; Run Final Config:1 ends here
