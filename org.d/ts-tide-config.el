
(after! flycheck
  (setq flycheck-executable-find #'my/flycheck-executable-find)
  )


(after! web-mode
  (add-hook! web-mode
    (defun my-web-mode-hook ()
      (smartparens-mode)
      (setq web-mode-enable-auto-quoting nil)
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-attr-indent-offset 2)
      (setq web-mode-attr-value-indent-offset 2)
      ))
  )


(after! typescript-mode
  (setq typescript-linter 'eslint)
  (message (concat "Initialzing typescript on " buffer-file-name))

  (add-hook!
   'tide-mode-hook
   (defun my-tide-setup-hook ()
     (message (concat "running my-tide-setup-hook on: " buffer-file-name))
     ;; (tide-setup)
     ;; (eldoc-mode)
     (flycheck-mode-on-safe)
     (tide-hl-identifier-mode +1)
     (turn-on-smartparens-mode)

     (set (make-local-variable 'company-backends)
          '((company-tide company-files)
            (company-dabbrev-code company-dabbrev)))


     ;; (flycheck-add-mode 'typescript-tslint 'web-mode)
     (setq flycheck-check-syntax-automatically '(save mode-enabled))

     (setq tide-tsserver-executable (executable-find-prefer-node-modules "tsserver"))
     (setq prettier-js-command (executable-find-prefer-node-modules "prettier"))

     (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tsserver.log"))
     ;; (defcustom tide-server-max-response-length 102400
     ;;   "Maximum allowed response length from tsserver. Any response greater than this would be ignored."

     (setq flycheck-eslint-args `("--no-eslintrc" ,(concat "--config=" (find-eslint-config))))

     (custom-set-variables
      '(js-indent-level 2)
      '(typescript-indent-level 2)
      '(tide-completion-detailed t)
      '(tide-completion-ignore-case t)
      )

     (setq tide-user-preferences
           '(
             :disableSuggestions nil
             ;;  If enabled, TypeScript will search through all external modules' exports and add them to the completions list.
             ;;  This affects lone identifier completions but not completions on the right hand side of `obj.`.
             :includeCompletionsForModuleExports t

             ;;  If enabled, the completion list will include completions with invalid identifier names.
             ;;  For those entries, The `insertText` and `replacementSpan` properties will be set to change from `.x` property access to `["x"]`.
             :includeCompletionsWithInsertText t

             :allowTextChangesInNewFiles t
             :quotePreference "single" ;;  "auto" | "double" | "single";

             ;; :importModuleSpecifierPreference "relative";; "relative" | "non-relative";
             ;; :allowTextChangesInNewFiles  t;; boolean;
             ;; :lazyConfiguredProjectsFromExternalProject?: boolean;
             ;; :providePrefixAndSuffixTextForRename?: boolean;
             ;; :allowRenameOfImportPath?: boolean;
             ))


     ;; Overriding this function due to bug (error "Selecting deleted buffer") in with-current-buffer
     (defun tide-dispatch-event (event)
       (-when-let (listener (gethash (tide-project-name) tide-event-listeners))
         (progn
           (if (buffer-live-p (car listener))
               (with-current-buffer (car listener)
                 (apply (cdr listener) (list event)))))
         ))

     )
   )

  (map! :map tide-mode-map
        :leader
        (:prefix ("e" . "errors"))
        :desc "Next Error"          "en"  #'flycheck-next-error
        :desc "Previous Error"      "ep"  #'flycheck-previous-error
        )

  (map! :map tide-mode-map
        :localleader
        (:prefix ("e" . "errors"))
        :desc "Error List"          "ee"  #'tide-project-errors
        :desc "Error at point"      "es"  #'tide-error-at-point
        :desc "Next Error"          "en"  #'flycheck-next-error
        :desc "Previous Error"      "ep"  #'flycheck-previous-error

        (:prefix ("g" . "Goto"))
        "gd" #'tide-documentation-at-point
        "gr" #'tide-references


        (:prefix ("r" . "Run"))
        :desc "Code Fix"          "rff"   #'tide-fix
        :desc "Rename Symbol"     "rrs"   #'tide-rename-symbol
        :desc "Refactor"          "rF"    #'tide-refactor
        :desc "Format Buffer"     "rff"   #'tide-format
        :desc "Organize Imports"  "roi"   #'tide-organize-imports
        ;; :desc "tslint" "ei" #'tide-add-tslint-disable-next-line
        ;; :desc "" "rP" #'prettier-js
        ;; :desc "" "rR" #'tide-rename-file

      (:prefix ("s" . "Server"))
        :desc "Execute (Restart) language server"            "sr"  #'tide-restart-server
        )

  (evil-define-key 'normal tide-mode-map
    (kbd "M-.") 'tide-jump-to-definition
    )

  (evil-define-key 'normal tide-project-errors-mode-map
    (kbd "RET") 'tide-goto-error
    )

  )


;; (use-package! prettier-js :defer t)

;; (use-package! tide
;;   :defer t)
;;
;; (defun web-mode-setup-my-hooks ()
;;     (pcase (file-name-extension buffer-file-name)
;;       ("tsx" (my-tide-setup-hook))
;;       (_ (my-web-mode-hook)))
;;     )
;;
;; (use-package! web-mode
;;   :mode (("\\.tsx$" . web-mode))
;;   :init
;;
;;   ;; (remove-hook 'web-mode-hook 'spacemacs/toggle-smartparens-off)
;;   ;; (remove-hook 'web-mode-hook 'turn-on-evil-matchit-mode)
;;   (remove-hook 'web-mode-hook 'web-mode-setup-my-hooks)
;;
;;   (add-hook 'web-mode-hook 'company-mode)
;;   (add-hook 'web-mode-hook 'web-mode-setup-my-hooks)
;;   )
;;
;; (use-package! typescript-mode
;;   :mode (("\\.ts$" . typescript-mode))
;;   :init
;;   (add-hook 'typescript-mode-hook 'my-tide-setup-hook t)
;;   (add-hook 'typescript-mode-hook 'company-mode)
;;   )

;; (package! tide)
;; (package! typescript-mode)
;; (package! add-node-modules-path)
;; Required, but configured elsewhere
;; (package! web-mode)
;; (package! eldoc)
;; (package! emmet-mode)
;; (package! flycheck)
;; (package! smartparens)
;; (package! import-js)
;; (package! yasnippet)
;;(package! company)
