;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Press 'K' on a module to view its documentation, and 'gd' to browse its directory.

(doom! :completion
       (company                     ; the ultimate code completion backend
        +childframe)                ; ... when your children are better than you
       helm                         ; the *other* search engine for love and life
       ;;ido                        ; the other *other* search engine...
       (ivy                         ; a search engine for love and life
        +icons                      ; ... icons are nice
        +prescient)                 ; ... I know what I want(ed)

       :ui
       doom                         ; what makes DOOM look the way it does
       doom-dashboard               ; a nifty splash screen for Emacs
       doom-quit                    ; DOOM quit-message prompts when you quit Emacs
       ;;fill-column                ; a `fill-column' indicator
       hl-todo                      ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ;;hydra                      ; quick documentation for related commands
       ;;indent-guides              ; highlighted indent columns, notoriously slow
       ;; (ligatures +extra)           ; ligatures and symbols to make your code pretty again
       ;;minimap                    ; show a map of the code on the side
       modeline                     ; snazzy, Atom-inspired modeline, plus API
       nav-flash                    ; blink the current line after jumping
       ;;neotree                    ; a project drawer, like NERDTree for vim
       ophints                      ; highlight the region an operation acts on
       (popup                       ; tame sudden yet inevitable temporary windows
        +all                        ; catch all popups that start with an asterix
        +defaults)                  ; default popup rules
       treemacs                     ; a project drawer, like neotree but cooler
       unicode                    ; extended unicode support for various languages
       vc-gutter                    ; vcs diff in the fringe
       vi-tilde-fringe              ; fringe tildes to mark beyond EOB
       (window-select +numbers)     ; visually switch windows
       zen                          ; distraction-free coding or writing

       :editor
       (evil +everywhere)
       file-templates               ; auto-snippets for empty files
       snippets                     ; my elves. They type so I don't have to

       :emacs
       (dired +icons)               ; making dired pretty [functional]
       (ibuffer +icons)             ; interactive buffer management
       (undo +tree)                 ; persistent, smarter undo for your inevitable mistakes

       ;; :checkers
       ;; syntax
       ;; spell
       ;; grammar

       :tools
       (lookup                      ; helps you navigate your code and documentation
        +dictionary                 ; dictionary/thesaurus is nice
        +docsets)                   ; ...or in Dash docsets locally
       lsp                          ; Language Server Protocol
       (magit                       ; a git porcelain for Emacs
        +forge)                     ; interface with git forges
       pdf                          ; pdf enhancements
       rgb                          ; creating color strings
       upload                       ; map local to remote projects via ssh/ftp
       vc

       :lang
       data                         ; config/data formats
       emacs-lisp                   ; drown in parentheses
       (haskell +dante)             ; a language that's lazier than I am
       (javascript +lsp)            ; all(hope(abandon(ye(who(enter(here))))))
       (latex                       ; writing papers in Emacs has never been so fun
        +latexmk                    ; what else would you use?
        +cdlatex                    ; quick maths symbols
        +fold)                      ; fold the clutter away nicities
       markdown                     ; writing docs for people to ignore
       (org                         ; organize your plain life in plain text
        +pretty                     ; yessss my pretties! (nice unicode symbols)
        +jupyter                    ; ipython/jupyter support for babel
        +pandoc                     ; export-with-pandoc support
        +gnuplot                    ; who doesn't like pretty pictures
        )
       (python +lsp +pyright)       ; beautiful is better than ugly
       (scala +lsp)                 ; java, but good
       scheme                       ; a fully conniving family of lisps
       sh                           ; she sells {ba,z,fi}sh shells on the C xor
       web                          ; the tubes
       yaml                         ; JSON, but readable

       :config
       ; literate
       (default +bindings +smartparens)
       )
