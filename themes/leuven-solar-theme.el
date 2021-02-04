;;; Commentary:

(deftheme leuven-solar
  "Face colors with a light background.
Basic, Font Lock, Isearch, Gnus, Message, Org mode, Diff, Ediff,
Flyspell, Semantic, and Ansi-Color faces are included -- and much
more...")

(let* (
      (class '((class color) (min-colors 89)))
      (s-base03    "#002b36")
      (s-base02    "#073642")
      ;; emphasized content
      (s-base01    "#586e75")
      ;; primary content
      (s-base00    "#657b83")
      (s-base0     "#839496")
      ;; comments
      (s-base1     "#93a1a1")
      ;; background highlight light
      (s-base2     "#eee8d5")
      ;; background light
      (s-base3     "#fdf6e3")

      ;; Solarized accented colors
      (yellow    "#b58900")
      (orange    "#cb4b16")
      (red       "#dc322f")
      (magenta   "#d33682")
      (violet    "#6c71c4")
      (blue      "#268bd2")
      (cyan      "#2aa198")
      (green     "#859900")


      ;; Darker and lighter accented colors
      ;; Only use these in exceptional circumstances!
      (yellow-d  "#7B6000")
      (yellow-l  "#DEB542")
      (orange-d  "#8B2C02")
      (orange-l  "#F2804F")
      (red-d     "#990A1B")
      (red-l     "#FF6E64")
      (magenta-d "#93115C")
      (magenta-l "#F771AC")
      (violet-d  "#3F4D91")
      (violet-l  "#9EA0E5")
      (blue-d    "#00629D")
      (blue-l    "#69B7F0")
      (cyan-d    "#00736F")
      (cyan-l    "#69CABF")
      (green-d   "#546E00")
      (green-l   "#B4C342")

      ;;
      (default-fg     s-base00)
      (default-bg     s-base3)
      (attr           magenta)



      ;; Leuven generic colors.
      (cancel '(:slant italic :strike-through t :foreground "#A9A9A9"))
      (clock-line '(:box (:line-width 1 :color "#335EA8") :foreground "black" :background "#EEC900"))
      (code-block '(:foreground "#000088" :background "#FFFFE0"))
      (code-inline '(:foreground "#006400" :background "#FDFFF7"))
      (column '(:height 1.0 :weight normal :slant normal :underline nil :strike-through nil :foreground "#E6AD4F" :background "#FFF2DE"))
      (diff-added '(:foreground "#008000" :background "#DDFFDD"))
      (diff-changed '(:foreground "#0000FF" :background "#DDDDFF"))
      (diff-header '(:foreground "#800000" :background "#FFFFAF"))
      (diff-hunk-header '(:foreground "#990099" :background "#FFEEFF"))
      (diff-none '(:foreground "gray33"))
      (diff-removed '(:foreground "#A60000" :background "#FFDDDD"))
      (directory '(:weight bold :foreground "blue" :background "#FFFFD2"))
      (highlight-line '(:background "#FFFFD7")) ; #F5F5F5
      (highlight-line-gnus '(:background "#DAEAFC")) ; defined in `gnus-leuven.el'
      (link '(:weight normal :underline t :foreground "#006DAF"))
      (mail-header-name '(:family "Sans Serif" :weight normal :foreground "#A3A3A2"))
      (mail-header-other '(:family "Sans Serif" :slant normal :foreground "#666666"))
      (mail-read '(:foreground "#A9A9A9"))
      (mail-read-high '(:foreground "#808080"))
      (mail-ticked '(:foreground "#E92415"))
      (mail-to '(:family "Sans Serif" :underline nil :foreground "#006DAF"))
      (mail-unread '(:foreground "#1B7BB8"))
      (mail-unread-high '(:foreground "#135985"))
      (marked-line '(:foreground "red" :background "pink"))
      (match '(:weight bold :background "#FBE448")) ; occur patterns
      (ol1 '(:height 1.0 :weight bold :overline "#A7A7A7" :foreground "#3b3b3b" :background "#F0F0F0"))
      (ol2 '(:height 1.0 :weight bold :overline "#123555" :foreground "#123555" :background "#E5F4FB"))
      (ol3 '(:height 1.0 :weight bold :foreground "#005522" :background "#EFFFEF"))
      (ol4 '(:height 1.0 :weight bold :slant normal :foreground "#EA6300"))
      (ol5 '(:height 1.0 :weight bold :slant normal :foreground "#E3258D"))
      (ol6 '(:height 1.0 :weight bold :slant italic :foreground "#0077CC"))
      (ol7 '(:height 1.0 :weight bold :slant italic :foreground "#2EAE2C"))
      (ol8 '(:height 1.0 :weight bold :slant italic :foreground "#FD8008"))
      (paren-matched '(:background "black" :weight bold))
      (paren-unmatched '(:underline "red" :foreground nil :background "#FFDCDC"))
      (region '(:background "#ABDFFA"))
      (shadow '(:foreground "#7F7F7F"))
      (string '(:foreground "#008000")) ; or #D0372D
      (subject '(:family "Source Code Pro" :weight bold :foreground "black"))
      (symlink '(:foreground "deep sky blue"))
      (tab '(:foreground "#D0D0D0" :background "white"))
      (volatile-highlight '(:underline nil :background "#FFF876"))
      (vc-branch '(:box (:line-width 1 :color "#00CC33") :foreground "black" :background "#AAFFAA")))

  (custom-theme-set-faces
   'leuven-solar
   `(default ((,class (:foreground ,s-base00 :background ,s-base3))))
   `(bold ((,class (:weight bold :foreground "black"))))
   `(bold-italic ((,class (:weight bold :slant italic :foreground "black"))))
   `(italic ((,class (:slant italic :foreground "#1A1A1A"))))
   `(underline ((,class (:underline t))))
   `(cursor ((,class (:foreground "#ffffff" :background "#00aaff" :inverse-video t))))
   `(lsp-ui-doc-background ((,class (:background ,s-base2))))

   `(mmm-init-submode-face ((,class (:background ,s-base2))))
   `(mmm-cleanup-submode-face ((,class (:background ,s-base2))))
   `(mmm-declaration-submode-face ((,class (:background ,s-base2))))
   `(mmm-comment-submode-face ((,class (:background ,s-base2))))
   `(mmm-output-submode-face ((,class (:background ,s-base2))))
   `(mmm-special-submode-face ((,class (:background ,s-base2))))
   `(mmm-code-submode-face ((,class (:background ,s-base2))))
   `(mmm-default-submode-face ((,class (:background ,s-base2))))
   `(mmm-delimiter-face ((,class (:background ,s-base2))))

   ;; Highlighting faces.
   `(fringe ((,class (:foreground ,s-base3 :background ,s-base1))))
   `(highlight ((,class ,volatile-highlight)))
   `(region ((,class ,region)))
   `(secondary-selection ((,class ,match))) ; used by Org-mode for highlighting matched entries and keywords
   `(isearch ((,class (:weight bold :underline "#FF9632" :foreground nil :background "#FDBD33"))))
   `(isearch-fail ((,class (:weight bold :foreground "black" :background "#FF9999"))))
   `(lazy-highlight ((,class (:underline "#FF9632" :background "#FFFF00")))) ; isearch others
   `(trailing-whitespace ((,class (:background "#FFFF57"))))
   `(whitespace-hspace ((,class (:foreground "#D2D2D2")))) ; see also `nobreak-space'
   `(whitespace-indentation ((,class ,tab)))
   `(whitespace-line ((,class (:foreground "#CC0000" :background "#FFFF88"))))
   `(whitespace-tab ((,class ,tab)))
   `(whitespace-trailing ((,class (:foreground "#B3B3B3" :background "#FFFF57"))))

   ;; Mode line faces.
   `(mode-line ((,class (:box (:line-width 1 :color "#1A2F54") :foreground "#85CEEB" :background "#335EA8"))))
   `(mode-line-inactive ((,class (:box (:line-width 1 :color "#4E4E4C") :foreground "#F0F0EF" :background "#9B9C97"))))
   `(mode-line-buffer-id ((,class (:weight bold :foreground "white"))))
   `(mode-line-emphasis ((,class (:weight bold :foreground "white"))))
   `(mode-line-highlight ((,class (:foreground "yellow"))))

   ;; Escape and prompt faces.
   `(minibuffer-prompt ((,class (:weight bold :foreground "black" :background "gold"))))
   `(minibuffer-noticeable-prompt ((,class (:weight bold :foreground "black" :background "gold"))))
   `(escape-glyph ((,class (:foreground "#008ED1"))))
   `(error ((,class (:foreground "red" :underline (:style wave :color "#fe3434")))))
   `(warning ((,class (:weight bold :foreground "orange"))))
   `(success ((,class (:foreground "green"))))

   ;; Font lock faces.
   `(font-lock-builtin-face ((,class (:foreground "#006FE0"))))
   `(font-lock-comment-delimiter-face ((,class (:foreground "#8D8D84")))) ; #696969
   `(font-lock-comment-face ((,class (:slant italic :foreground "#8D8D84")))) ; #696969
   `(font-lock-constant-face ((,class (:foreground "#D0372D"))))
   `(font-lock-doc-face ((,class (:foreground "#036A07"))))
   ;; `(font-lock-doc-string-face ((,class (:foreground "#008000")))) ; XEmacs only, but is used for HTML exports from org2html (and not interactively)
   `(font-lock-function-name-face ((,class (:weight normal :foreground "#006699"))))
   `(font-lock-keyword-face ((,class (:bold nil :foreground "#0000FF")))) ; #3654DC
   `(font-lock-preprocessor-face ((,class (:foreground "#808080"))))
   `(font-lock-regexp-grouping-backslash ((,class (:weight bold :inherit nil))))
   `(font-lock-regexp-grouping-construct ((,class (:weight bold :inherit nil))))
   `(font-lock-string-face ((,class ,string)))
   `(font-lock-type-face ((,class (:weight normal :foreground "#6434A3"))))
   `(font-lock-variable-name-face ((,class (:weight normal :foreground "#BA36A5")))) ; #800080
   `(font-lock-warning-face ((,class (:weight bold :foreground "red"))))

   ;; Button and link faces.
   `(link ((,class ,link)))
   `(link-visited ((,class (:underline t :foreground "#E5786D"))))
   `(button ((,class (:underline t :foreground "#006DAF"))))
   `(header-line ((,class (:weight bold :underline "black" :overline "black" :foreground "black" :background "#FFFF88"))))

   ;; Gnus faces.
   `(gnus-button ((,class (:weight normal))))
   `(gnus-cite-attribution-face ((,class (:foreground "#5050B0"))))
   `(gnus-cite-face-1 ((,class (:foreground "#5050B0"))))
   `(gnus-cite-face-10 ((,class (:foreground "#990000"))))
   `(gnus-cite-face-2 ((,class (:foreground "#660066"))))
   `(gnus-cite-face-3 ((,class (:foreground "#007777"))))
   `(gnus-cite-face-4 ((,class (:foreground "#990000"))))
   `(gnus-cite-face-5 ((,class (:foreground "#000099"))))
   `(gnus-cite-face-6 ((,class (:foreground "#BB6600"))))
   `(gnus-cite-face-7 ((,class (:foreground "#5050B0"))))
   `(gnus-cite-face-8 ((,class (:foreground "#660066"))))
   `(gnus-cite-face-9 ((,class (:foreground "#007777"))))
   `(gnus-emphasis-bold ((,class (:weight bold))))
   `(gnus-emphasis-highlight-words ((,class (:foreground "yellow" :background "black"))))
   `(gnus-group-mail-1 ((,class (:weight bold :foreground "#FF50B0"))))
   `(gnus-group-mail-1-empty ((,class (:foreground "#5050B0"))))
   `(gnus-group-mail-2 ((,class (:weight bold :foreground "#FF0066"))))
   `(gnus-group-mail-2-empty ((,class (:foreground "#660066"))))
   `(gnus-group-mail-3 ((,class ,mail-unread)))
   `(gnus-group-mail-3-empty ((,class ,mail-read)))
   `(gnus-group-mail-low ((,class ,cancel)))
   `(gnus-group-mail-low-empty ((,class ,cancel)))
   `(gnus-group-news-1 ((,class (:weight bold :foreground "#FF50B0"))))
   `(gnus-group-news-1-empty ((,class (:foreground "#5050B0"))))
   `(gnus-group-news-2 ((,class (:weight bold :foreground "#FF0066"))))
   `(gnus-group-news-2-empty ((,class (:foreground "#660066"))))
   `(gnus-group-news-3 ((,class ,mail-unread)))
   `(gnus-group-news-3-empty ((,class ,mail-read)))
   `(gnus-group-news-4 ((,class (:weight bold :foreground "#FF0000"))))
   `(gnus-group-news-4-empty ((,class (:foreground "#990000"))))
   `(gnus-group-news-5 ((,class (:weight bold :foreground "#FF0099"))))
   `(gnus-group-news-5-empty ((,class (:foreground "#000099"))))
   `(gnus-group-news-6 ((,class (:weight bold :foreground "gray50"))))
   `(gnus-group-news-6-empty ((,class (:foreground "#808080"))))
   `(gnus-header-content ((,class ,mail-header-other)))
   `(gnus-header-from ((,class (:family "Sans Serif" :foreground "black"))))
   `(gnus-header-name ((,class ,mail-header-name)))
   `(gnus-header-newsgroups ((,class (:family "Sans Serif" :foreground "#3399CC"))))
   `(gnus-header-subject ((,class ,subject)))
   `(gnus-picon ((,class (:foreground "yellow" :background "white"))))
   `(gnus-picon-xbm ((,class (:foreground "yellow" :background "white"))))
   `(gnus-server-closed ((,class (:slant italic :foreground "blue" :background "white"))))
   `(gnus-server-denied ((,class (:weight bold :foreground "red" :background "white"))))
   `(gnus-server-opened ((,class (:family "Sans Serif" :foreground "white" :foreground "#466BD7"))))
   `(gnus-signature ((,class (:slant italic :foreground "#8B8D8E"))))
   `(gnus-splash ((,class (:foreground "#FF8C00"))))
   `(gnus-summary-cancelled ((,class ,cancel)))
   `(gnus-summary-high-ancient ((,class ,mail-read-high)))
   `(gnus-summary-high-read ((,class ,mail-read-high)))
   `(gnus-summary-high-ticked ((,class ,mail-ticked)))
   `(gnus-summary-high-unread ((,class ,mail-unread-high)))
   `(gnus-summary-low-ancient ((,class (:slant italic :foreground "gray55"))))
   `(gnus-summary-low-read ((,class (:slant italic :foreground "#999999" :background "#E0E0E0"))))
   `(gnus-summary-low-ticked ((,class ,mail-ticked)))
   `(gnus-summary-low-unread ((,class (:slant italic :foreground "black"))))
   `(gnus-summary-normal-ancient ((,class ,mail-read)))
   `(gnus-summary-normal-read ((,class ,mail-read)))
   `(gnus-summary-normal-ticked ((,class ,mail-ticked)))
   `(gnus-summary-normal-unread ((,class ,mail-unread)))
   `(gnus-summary-selected ((,class (:foreground "white" :background "#008CD7"))))
   `(gnus-x-face ((,class (:foreground "black" :background "white"))))

   ;; Message faces.
   `(message-header-name ((,class ,mail-header-name)))
   `(message-header-cc ((,class ,mail-to)))
   `(message-header-other ((,class ,mail-header-other)))
   `(message-header-subject ((,class ,subject)))
   `(message-header-to ((,class ,mail-to)))
   `(message-cited-text ((,class (:foreground "#5050B0"))))
   `(message-separator ((,class (:family "Sans Serif" :weight normal :foreground "#BDC2C6"))))
   `(message-header-newsgroups ((,class (:family "Sans Serif" :foreground "#3399CC"))))
   `(message-header-xheader ((,class ,mail-header-other)))
   `(message-mml ((,class (:foreground "forest green"))))

   ;; Diff.
   `(diff-added ((,class ,diff-added)))
   `(diff-changed ((,class ,diff-changed)))
   `(diff-context ((,class ,diff-none)))
   `(diff-file-header ((,class ,diff-header)))
   `(diff-file1-hunk-header ((,class (:foreground "dark magenta" :background "#EAF2F5"))))
   `(diff-file2-hunk-header ((,class (:foreground "#2B7E2A" :background "#EAF2F5"))))
   `(diff-function ((,class (:foreground "darkgray"))))
   `(diff-header ((,class ,diff-header)))
   `(diff-hunk-header ((,class ,diff-hunk-header)))
   `(diff-index ((,class ,diff-header)))
   `(diff-indicator-added ((,class (:background "#AAFFAA"))))
   `(diff-indicator-changed ((,class (:background "#8080FF"))))
   `(diff-indicator-removed ((,class (:background "#FFBBBB"))))
   `(diff-refine-change ((,class (:background "#DDDDFF"))))
   `(diff-removed ((,class ,diff-removed)))

   ;; SMerge.
   `(smerge-refined-change ((,class (:background "#AAAAFF"))))

   ;; Ediff.
   `(ediff-current-diff-A ((,class (:foreground "gray33" :background "#FFDDDD"))))
   `(ediff-current-diff-B ((,class (:foreground "gray33" :background "#DDFFDD"))))
   `(ediff-current-diff-C ((,class (:foreground "black" :background "cyan"))))
   `(ediff-even-diff-A ((,class (:foreground "black" :background "light grey"))))
   `(ediff-even-diff-B ((,class (:foreground "black" :background "light grey"))))
   `(ediff-fine-diff-A ((,class (:foreground "#A60000" :background "#FFAAAA"))))
   `(ediff-fine-diff-B ((,class (:foreground "#008000" :background "#55FF55"))))
   `(ediff-odd-diff-A ((,class (:foreground "black" :background "light grey"))))
   `(ediff-odd-diff-B ((,class (:foreground "black" :background "light grey"))))

   ;; Flyspell.
   (if (version< emacs-version "24.4")
       `(flyspell-duplicate ((,class (:underline "#008000" :inherit nil))))
     `(flyspell-duplicate ((,class (:underline (:style wave :color "#008000") :inherit nil)))))
   (if (version< emacs-version "24.4")
       `(flyspell-incorrect ((,class (:underline "red" :inherit nil))))
     `(flyspell-incorrect ((,class (:underline (:style wave :color "red") :inherit nil)))))

   `(Info-title-1-face ((,class ,ol1)))
   `(Info-title-2-face ((,class ,ol2)))
   `(Info-title-3-face ((,class ,ol3)))
   `(Info-title-4-face ((,class ,ol4)))
   `(ac-completion-face ((,class (:underline nil :foreground "#C0C0C0")))) ; like Google
   `(ace-jump-face-foreground ((,class (:foreground "black" :background "#FBE448"))))
   `(ahs-face ((,class (:background "#84CFFF"))))
   `(ahs-definition-face ((,class (:weight bold :background "#84CFFF"))))
   `(ahs-plugin-defalt-face ((,class (:background "#FFB6C6"))))
   `(auto-dim-other-buffers-face ((,class (:background "#F7F7F7"))))
   `(bbdb-company ((,class (:slant italic :foreground "steel blue"))))
   `(bbdb-field-name ((,class (:weight bold :foreground "steel blue"))))
   `(bbdb-field-value ((,class (:foreground "steel blue"))))
   `(bbdb-name ((,class (:underline t :foreground "#FF6633"))))
   `(bmkp-light-autonamed ((,class (:background "#C2DDFD"))))
   `(bmkp-light-fringe-autonamed ((,class (:background "#90AFD5"))))
   `(bmkp-light-fringe-non-autonamed ((,class (:background "#D5FFD5"))))
   `(bmkp-light-non-autonamed ((,class (:background "#C4FFC4"))))
   `(browse-kill-ring-separator-face ((,class (:weight bold :foreground "slate gray"))))
   `(calendar-month-header ((,class (:weight bold :foreground "#4F4A3D" :background "#FFFFCC"))))
   `(calendar-today ((,class (:weight bold :foreground "#4F4A3D" :background "#FFFFCC"))))
   `(calendar-weekday-header ((,class (:weight bold :foreground "#1662AF"))))
   `(calendar-weekend-header ((,class (:weight bold :foreground "#4E4E4E"))))
   `(cfw:face-annotation ((,class (:foreground "green" :background "red"))))
   `(cfw:face-day-title ((,class (:foreground "#C9C9C9"))))
   `(cfw:face-default-content ((,class (:foreground "#2952A3"))))
   `(cfw:face-default-day ((,class (:weight bold))))
   `(cfw:face-disable ((,class (:foreground "DarkGray"))))
   `(cfw:face-grid ((,class (:foreground "#DDDDDD"))))
   `(cfw:face-header ((,class (:foreground "#1662AF" :background "white" :weight bold))))
   `(cfw:face-holiday ((,class (:foreground "#777777" :background "#E4EBFE"))))
   `(cfw:face-periods ((,class (:foreground "white" :background "#668CD9" :slant italic))))
   `(cfw:face-saturday ((,class (:foreground "#4E4E4E" :background "white" :weight bold))))
   `(cfw:face-select ((,class (:foreground "#4A95EB" :background "#EDF1FA"))))
   `(cfw:face-sunday ((,class (:foreground "#4E4E4E" :background "white" :weight bold))))
   `(cfw:face-title ((,class (:height 2.0 :foreground "#676767" :weight bold :inherit variable-pitch))))
   `(cfw:face-today ((,class (:foreground "#4F4A3D" :background "#FFFFCC"))))
   `(cfw:face-today-title ((,class (:foreground "#4A95EB" :background "#FFFFCC"))))
   `(cfw:face-toolbar ((,class (:background "white"))))
   `(cfw:face-toolbar-button-off ((,class (:foreground "#CFCFCF" :background "white"))))
   `(cfw:face-toolbar-button-on ((,class (:foreground "#5E5E5E" :background "#F6F6F6"))))
   `(change-log-date ((,class (:foreground "purple"))))
   `(change-log-file ((,class (:weight bold :foreground "#4183C4"))))
   `(change-log-list ((,class (:foreground "cyan3"))))
   `(change-log-name ((,class (:foreground "#008000"))))
   `(circe-highlight-all-nicks-face ((,class (:foreground "blue" :background "#F0F0F0")))) ; other nick names
   `(circe-highlight-nick-face ((,class (:foreground "#009300" :background "#F0F0F0")))) ; messages with my nick cited
   `(circe-my-message-face ((,class (:foreground "#8B8B8B" :background "#F0F0F0"))))
   `(circe-originator-face ((,class (:foreground "blue"))))
   `(circe-prompt-face ((,class (:foreground "red"))))
   `(circe-server-face ((,class (:foreground "#99CAE5"))))
   `(comint-highlight-input ((,class (:weight bold :foreground "#0000FF" :inherit nil))))
   ;; `(comint-highlight-prompt ((,class (:weight bold :foreground "black" :background "gold"))))
   `(comint-highlight-prompt ((,class (:weight bold :foreground "#0000FF" :inherit nil))))
   ;; same background as highlight-line
   `(company-preview-common ((,class (:foreground "#C0C0C0" :background "#FFFFD7"))))
   `(company-scrollbar-bg ((,class (:background "#F0F0F0"))))
   `(company-scrollbar-fg ((,class (:background "#C0C0C0"))))
   `(company-tooltip ((,class (:weight bold :foreground "black" :background "#F5F5F9"))))
   `(company-tooltip-annotation ((,class (:weight normal :foreground "#999999" :background "#F5F5F9"))))
   `(company-tooltip-common ((,class (:weight normal :inherit company-tooltip))))
   `(company-tooltip-common-selection ((,class (:weight normal :inherit company-tooltip-selection))))
   `(company-tooltip-selection ((,class (:weight bold :foreground "white" :background "#3C7FD4"))))
   `(compare-windows ((,class (:background "#FFFF00"))))
   `(compilation-error ((,class (:weight bold :foreground "red" :underline t)))) ; Used for grep error messages.
   `(compilation-info ((,class (:weight bold :foreground "#2A489E")))) ; Used for grep hits.
   `(compilation-line-number ((,class (:weight bold :foreground "#A535AE"))))
   `(compilation-warning ((,class (:weight bold :underline t))))
   `(compilation-mode-line-exit ((,class (:weight bold :foreground "green")))) ; :exit[matched]
   `(compilation-mode-line-fail ((,class (:weight bold :foreground "violet")))) ; :exit[no match]
   `(compilation-mode-line-run ((,class (:weight bold :foreground "orange")))) ; :run
   `(css-property ((,class (:foreground "#00AA00"))))
   `(css-selector ((,class (:weight bold :foreground "blue"))))
   `(custom-button ((,class (:box (:line-width 2 :style released-button) :foreground "black" :background "lightgrey"))))
   `(custom-button-mouse ((,class (:box (:line-width 2 :style released-button) :foreground "black" :background "grey90"))))
   `(custom-button-pressed ((,class (:box (:line-width 2 :style pressed-button) :foreground "black" :background "light grey"))))
   `(custom-button-pressed-unraised ((,class (:underline t :foreground "magenta4"))))
   `(custom-button-unraised ((,class (:underline t))))
   `(custom-changed ((,class (:foreground "white" :background "blue"))))
   `(custom-comment ((,class (:background "gray85"))))
   `(custom-comment-tag ((,class (:foreground "blue4"))))
   `(custom-documentation ((,class (nil))))
   `(custom-face-tag ((,class (:family "Sans Serif" :height 1.2 :weight bold))))
   `(custom-group-tag ((,class (:height 1.2 :weight bold :foreground "blue1"))))
   `(custom-group-tag-1 ((,class (:family "Sans Serif" :height 1.2 :weight bold :foreground "red1"))))
   `(custom-invalid ((,class (:foreground "yellow" :background "red"))))
   `(custom-link ((,class (:underline t :foreground "blue1"))))
   `(custom-modified ((,class (:foreground "white" :background "blue"))))
   `(custom-rogue ((,class (:foreground "pink" :background "black"))))
   `(custom-saved ((,class (:underline t))))
   `(custom-set ((,class (:foreground "blue" :background "white"))))
   `(custom-state ((,class (:foreground "green4"))))
   `(custom-themed ((,class (:foreground "white" :background "blue1"))))
   `(custom-variable-button ((,class (:weight bold :underline t))))
   `(custom-variable-tag ((,class (:family "Sans Serif" :height 1.2 :weight bold :foreground "blue1"))))
   `(custom-visibility ((,class ,link)))
   `(diff-hl-change ((,class (:foreground "blue3" :inherit diff-changed))))
   `(diff-hl-delete ((,class (:foreground "red3" :inherit diff-removed))))
   `(diff-hl-dired-change ((,class (:weight bold :foreground "black" :background "#FFA335"))))
   `(diff-hl-dired-delete ((,class (:weight bold :foreground "#D73915"))))
   `(diff-hl-dired-ignored ((,class (:weight bold :foreground "white" :background "#C0BBAB"))))
   `(diff-hl-dired-insert ((,class (:weight bold :foreground "#B9B9BA"))))
   `(diff-hl-dired-unknown ((,class (:foreground "white" :background "#3F3BB4"))))
   `(diff-hl-insert ((,class (:foreground "green4" :inherit diff-added))))
   `(diff-hl-unknown ((,class (:foreground "white" :background "#3F3BB4"))))
   `(diary-face ((,class (:foreground "#87C9FC"))))
   `(dircolors-face-asm ((,class (:foreground "black"))))
   `(dircolors-face-backup ((,class (:foreground "black"))))
   `(dircolors-face-compress ((,class (:foreground "red"))))
   `(dircolors-face-dir ((,class ,directory)))
   `(dircolors-face-doc ((,class (:foreground "black"))))
   `(dircolors-face-dos ((,class (:foreground "ForestGreen"))))
   `(dircolors-face-emacs ((,class (:foreground "black"))))
   `(dircolors-face-exec ((,class (:foreground "ForestGreen"))))
   `(dircolors-face-html ((,class (:foreground "black"))))
   `(dircolors-face-img ((,class (:foreground "magenta3"))))
   `(dircolors-face-lang ((,class (:foreground "black"))))
   `(dircolors-face-lang-interface ((,class (:foreground "black"))))
   `(dircolors-face-make ((,class (:foreground "black"))))
   `(dircolors-face-objet ((,class (:foreground "black"))))
   `(dircolors-face-package ((,class (:foreground "black"))))
   `(dircolors-face-paddb ((,class (:foreground "black"))))
   `(dircolors-face-ps ((,class (:foreground "black"))))
   `(dircolors-face-sound ((,class (:foreground "DeepSkyBlue"))))
   `(dircolors-face-tar ((,class (:foreground "red"))))
   `(dircolors-face-text ((,class (:foreground "black"))))
   `(dircolors-face-yacc ((,class (:foreground "black"))))
   `(dired-directory ((,class ,directory)))
   `(dired-header ((,class ,directory)))
   `(dired-ignored ((,class (:strike-through t :foreground "red"))))
   `(dired-mark ((,class ,marked-line)))
   `(dired-marked ((,class ,marked-line)))
   `(dired-symlink ((,class ,symlink)))
   `(diredp-compressed-file-suffix ((,class (:foreground "red"))))
   `(diredp-date-time ((,class (:foreground "purple"))))
   `(diredp-dir-heading ((,class ,directory)))
   `(diredp-dir-priv ((,class ,directory)))
   `(diredp-exec-priv ((,class (:background "#03C03C"))))
   `(diredp-executable-tag ((,class (:foreground "ForestGreen" :background "white"))))
   `(diredp-file-name ((,class (:foreground "black"))))
   `(diredp-file-suffix ((,class (:foreground "#C0C0C0"))))
   `(diredp-flag-mark-line ((,class ,marked-line)))
   `(diredp-ignored-file-name ((,class ,shadow)))
   `(diredp-read-priv ((,class (:background "#0A99FF"))))
   `(diredp-write-priv ((,class (:foreground "white" :background "#FF4040"))))

   `(dired-subtree-depth-1-face ((,class (:background "#f9e9e0"))))
   `(dired-subtree-depth-2-face ((,class (:background "#f9d9e0"))))
   `(dired-subtree-depth-3-face ((,class (:background "#f9c9e0"))))
   `(dired-subtree-depth-4-face ((,class (:background "#f9b9e0"))))
   `(dired-subtree-depth-5-face ((,class (:background "#f9a9e0"))))
   `(dired-subtree-depth-6-face ((,class (:background "#f999e0"))))


   `(file-name-shadow ((,class ,shadow)))
   `(font-latex-bold-face ((,class (:weight bold :foreground "black"))))
   `(font-latex-italic-face ((,class (:slant italic :foreground "#1A1A1A"))))
   `(font-latex-math-face ((,class (:foreground "blue"))))
   `(font-latex-sectioning-1-face ((,class (:family "Sans Serif" :height 2.7 :weight bold :foreground "cornflower blue"))))
   `(font-latex-sectioning-2-face ((,class ,ol1)))
   `(font-latex-sectioning-3-face ((,class ,ol2)))
   `(font-latex-sectioning-4-face ((,class ,ol3)))
   `(font-latex-sectioning-5-face ((,class ,ol4)))
   `(font-latex-sedate-face ((,class (:foreground "#FF5500"))))
   `(font-latex-string-face ((,class (:weight bold :foreground "#0066FF"))))
   `(font-latex-verbatim-face ((,class (:foreground "#000088" :background "#FFFFE0" :inherit nil))))
   `(git-commit-summary-face ((,class (:foreground "#000000"))))
   `(git-commit-comment-face ((,class (:slant italic :foreground "#696969"))))
   `(google-translate-text-face ((t (:foreground "#777777" :background "#F5F5F5"))))
   `(google-translate-phonetic-face ((t (:inherit shadow))))
   `(google-translate-translation-face ((t (:weight normal :foreground "#3079ED" :background "#E3EAF2"))))
   `(google-translate-suggestion-label-face ((t (:foreground "red"))))
   `(google-translate-suggestion-face ((t (:slant italic :underline t))))
   `(google-translate-listen-button-face ((t (:height 0.8))))
   `(helm-action ((,class (:foreground "black"))))
   `(helm-bookmarks-su-face ((,class (:foreground "red"))))
   `(helm-buffer-directory ((,class ,directory)))
   `(helm-buffer-process ((,class (:foreground "#008200"))))
   `(helm-candidate-number ((,class (:foreground "black" :background "#FFFF66"))))
   `(helm-dir-heading ((,class (:foreground "blue" :background "pink"))))
   `(helm-dir-priv ((,class (:foreground "dark red" :background "light grey"))))
   `(helm-ff-directory ((,class ,directory)))
   `(helm-ff-executable ((,class (:foreground "green3" :background "white"))))
   `(helm-ff-file ((,class (:foreground "black"))))
   `(helm-ff-invalid-symlink ((,class (:foreground "yellow" :background "red"))))
   `(helm-ff-symlink ((,class ,symlink)))
   `(helm-file-name ((,class (:foreground "blue"))))
   `(helm-gentoo-match-face ((,class (:foreground "red"))))
   `(helm-grep-match ((,class ,match)))
   `(helm-grep-running ((,class (:weight bold :foreground "white"))))
   `(helm-grep-lineno ((,class ,shadow)))
   `(helm-isearch-match ((,class (:background "#CCFFCC"))))
   `(helm-match ((,class ,match)))
   `(helm-moccur-buffer ((,class (:foreground "#0066CC"))))
   `(helm-selection ((,class ,volatile-highlight)))
   `(helm-selection-line ((,class ,volatile-highlight)))
   `(helm-lisp-show-completion ((,class ,volatile-highlight))) ; see `helm-dabbrev'
   `(helm-source-header ((,class (:family "Sans Serif" :height 1.3 :weight bold :foreground "white" :background "#2F69BF"))))
   `(helm-swoop-target-line-face ((,class ,volatile-highlight)))
   `(helm-swoop-target-line-block-face ((,class (:background "#CCCC00" :foreground "#222222"))))
   `(helm-swoop-target-word-face ((,class (:weight bold :foreground nil :background "#FDBD33"))))
   `(helm-visible-mark ((,class ,marked-line)))
   `(helm-w3m-bookmarks-face ((,class (:underline t :foreground "cyan1"))))
   `(highlight-symbol-face ((,class (:background "#FFFFA0"))))
   `(hl-line ((,class (:background ,s-base2))), t)
   `(hl-tags-face ((,class (:background "#FEFCAE"))))
   `(holiday-face ((,class (:foreground "#777777" :background "#E4EBFE"))))
   `(html-helper-bold-face ((,class (:weight bold :foreground "black"))))
   `(html-helper-italic-face ((,class (:slant italic :foreground "black"))))
   `(html-helper-underline-face ((,class (:underline t :foreground "black"))))
   `(html-tag-face ((,class (:foreground "blue"))))
   `(ilog-non-change-face ((,class (:height 2.0 :foreground "#6434A3"))))
   `(ilog-change-face ((,class (:height 2.0 :foreground "#008200"))))
   `(ilog-echo-face ((,class (:height 2.0 :foreground "#006FE0"))))
   `(ilog-load-face ((,class (:foreground "#BA36A5"))))
   `(ilog-message-face ((,class (:foreground "#808080"))))
   `(info-file ((,class (:family "Sans Serif" :height 1.8 :weight bold :box (:line-width 1 :color "#0000CC") :foreground "cornflower blue" :background "LightSteelBlue1"))))
   `(info-header-node ((,class (:underline t :foreground "orange")))) ; nodes in header
   `(info-header-xref ((,class (:underline t :foreground "dodger blue")))) ; cross references in header
   `(info-index-match ((,class (:weight bold :foreground nil :background "#FDBD33")))) ; when using `i'
   `(info-menu-header ((,class ,ol2))) ; menu titles (headers) -- major topics
   `(info-menu-star ((,class (:foreground "black")))) ; every 3rd menu item
   `(info-node ((,class (:underline t :foreground "blue")))) ; node names
   `(info-quoted-name ((,class ,code-inline)))
   `(info-string ((,class ,string)))
   `(info-title-1 ((,class ,ol1)))
   `(info-xref ((,class (:underline t :foreground "#006DAF")))) ; unvisited cross-references
   `(info-xref-visited ((,class (:underline t :foreground "magenta4")))) ; previously visited cross-references
   `(light-symbol-face ((,class (:background "#FFFFA0"))))
   `(linum ((,class (:foreground "#9A9A9A" :background "#EDEDED"))))
   `(log-view-file ((,class (:foreground "#0000CC" :background "#EAF2F5"))))
   `(log-view-message ((,class (:foreground "yellow3"))))
   `(lui-button-face ((,class ,link)))
   `(lui-highlight-face ((,class (:box '(:line-width 1 :color "#CC0000") :foreground "#CC0000" :background "#FFFF88")))) ; my nickname
   `(lui-time-stamp-face ((,class (:foreground "purple"))))


   ;;;;;;;;;;;;;;;;
   ;;;;; Magit
   `(magit-blame-header ((,class (:inherit magit-diff-file-header))))
   `(magit-branch ((,class ,vc-branch)))
   `(magit-diff-add ((,class ,diff-added)))
   `(magit-diff-del ((,class ,diff-removed)))
   `(magit-diff-file-header ((,class (:family "Sans Serif" :height 1.1 :weight bold :foreground "#4183C4"))))
   `(magit-diff-hunk-header ((,class ,diff-hunk-header)))
   `(magit-diff-none ((,class ,diff-none)))
   `(magit-header ((,class (:foreground "white" :background "#FF4040"))))
   `(magit-item-highlight ((,class (:background "#EAF2F5"))))
   `(magit-item-mark ((,class ,marked-line)))
   `(magit-log-head-label ((,class (:box (:line-width 1 :color "blue" :style nil)))))
   `(magit-log-tag-label ((,class (:box (:line-width 1 :color "#00CC00" :style nil)))))
   `(magit-section-title ((,class (:family "Sans Serif" :height 1.8 :weight bold :foreground "cornflower blue" :inherit nil))))

   `(neuron-link-face                    ((,class (:background ,s-base2))))
   `(neuron-invalid-zettel-id-face       ((,class (:background ,s-base3) :underline (:style wave :color ,red ) )))
   `(neuron-zettel-tag-face              ((,class (:background ,s-base0))))
   `(neuron-title-overlay-face           ((,class (:background ,s-base2 :overline t))))
   `(neuron-cf-title-overlay-face        ((,class (:background ,s-base2 :underline t))))
   `(neuron-invalid-link-face            ((,class (:background ,s-base3 :background ,red ))))
   `(neuron-link-mouse-face              ((,class (:background ,s-base2))))

   `(frog-menu-border ((,class (:foreground ,s-base01 :background ,s-base2))))
   ;; `(frog-menu-prompt-face ((,class (:foreground ,s-base01 :background ,s-base2))))
   `(frog-menu-candidates-face ((,class (:foreground ,s-base01 :background ,s-base2))))
   `(frog-menu-actions-face ((,class (:foreground "blue" :background ,s-base2 :weight bold))))
   ;; `(frog-menu-action-keybinding-face ((,class (:foreground ,s-base01 :background ,s-base2))))
   `(frog-menu-posframe-background-face ((,class (:foreground ,s-base3 :background ,s-base1))))

   `(makefile-space-face ((,class (:background "hot pink"))))
   `(makefile-targets ((,class (:weight bold :foreground "blue"))))
   `(match ((,class ,match)))           ; Used for grep matches.
   `(mm-uu-extract ((,class ,code-block)))
   `(moccur-current-line-face ((,class (:foreground "black" :background "#FFFFCC"))))
   `(moccur-face ((,class (:foreground "black" :background "#FFFF99"))))
   `(next-error ((,class ,volatile-highlight)))
   `(nobreak-space ((,class (:background "#CCE8F6"))))
   `(nxml-attribute-local-name-face ((,class (:foreground "magenta"))))
   `(nxml-attribute-value-delimiter-face ((,class (:foreground "green4"))))
   `(nxml-attribute-value-face ((,class (:foreground "green4"))))
   `(nxml-comment-content-face ((,class (:slant italic :foreground "red"))))
   `(nxml-comment-delimiter-face ((,class (:foreground "red"))))
   `(nxml-element-local-name ((,class (:box (:line-width 1 :color "#999999") :foreground "#000088" :background "#DEDEDE"))))
   `(nxml-element-local-name-face ((,class (:foreground "blue"))))
   `(nxml-processing-instruction-target-face ((,class (:foreground "purple1"))))
   `(nxml-tag-delimiter-face ((,class (:foreground "blue"))))
   `(nxml-tag-slash-face ((,class (:foreground "blue"))))
   `(org-agenda-block-count ((,class (:weight bold :foreground "#A5A5A5"))))
   `(org-agenda-calendar-event ((,class (:weight bold :foreground "#3774CC" :background "#E4EBFE"))))
   `(org-agenda-calendar-sexp ((,class (:foreground "#777777" :background "#DAEAFC"))))
   `(org-agenda-clocking ((,class (:foreground "black" :background "#EEC900"))))
   `(org-agenda-column-dateline ((,class ,column)))
   `(org-agenda-current-time ((,class (:underline t :foreground "#1662AF"))))
   `(org-agenda-date ((,class (:height 1.6 :weight bold :foreground "#1662AF"))))
   `(org-agenda-date-today ((,class (:height 1.6 :weight bold :foreground "#4F4A3D" :background "#FFFFCC"))))
   `(org-agenda-date-weekend ((,class (:height 1.6 :weight bold :foreground "#4E4E4E"))))
   `(org-agenda-diary ((,class (:weight bold :foreground "green4" :background "light blue"))))
   `(org-agenda-dimmed-todo-face ((,class (:foreground "gold2"))))
   `(org-agenda-done ((,class (:foreground "#555555"))))
   `(org-agenda-filter-category ((,class (:weight bold :foreground "orange"))))
   `(org-agenda-filter-tags ((,class (:weight bold :foreground "orange"))))
   `(org-agenda-restriction-lock ((,class (:background "#E77D63"))))
   `(org-agenda-structure ((,class (:height 1.6 :weight bold :foreground "#1F8DD6"))))
   `(org-archived ((,class (:foreground "gray70"))))
   `(org-beamer-tag ((,class (:box (:line-width 1 :color "#FABC18") :foreground "#2C2C2C" :background "#FFF8D0"))))
   `(org-block ((,class ,code-block)))
   `(org-block-background ((,class (:background "#FFFFE0"))))
   `(org-block-begin-line ((,class (:underline "#A7A6AA" :foreground "#555555" :background "#E2E1D5"))))
   `(org-block-end-line ((,class (:overline "#A7A6AA" :foreground "#555555" :background "#E2E1D5"))))
   `(org-checkbox ((,class (:weight bold :box (:line-width 1 :style pressed-button) :foreground "white" :background "#777777"))))
   `(org-clock-overlay ((,class (:foreground "white" :background "SkyBlue4"))))
   `(org-code ((,class ,code-inline)))
   `(org-column ((,class ,column)))
   `(org-column-title ((,class ,column)))
   `(org-date ((,class (:underline t :foreground "#00459E"))))
   `(org-default ((,class (:foreground "#333333" :background "#FFFFFF"))))
   `(org-dim ((,class (:foreground "#AAAAAA"))))
   `(org-document-info ((,class (:foreground "#484848"))))
   `(org-document-info-keyword ((,class (:foreground "#008ED1" :background "#EAEAFF"))))
   `(org-document-title ((,class (:family "Sans Serif" :height 1.8 :weight bold :foreground "black"))))
   `(org-done ((,class (:weight bold :box (:line-width 1 :color "#BBBBBB") :foreground "#BBBBBB" :background "#F0F0F0"))))
   `(org-drawer ((,class (:foreground "light sky blue"))))
   `(org-ellipsis ((,class (:underline nil :box (:line-width 1 :color "#999999") :foreground "#999999" :background "#FFF8C0")))) ; #FFEE62
   `(org-example ((,class (:foreground "blue" :background "#EAFFEA"))))
   `(org-footnote ((,class (:underline t :foreground "#008ED1"))))
   `(org-formula ((,class (:foreground "chocolate1"))))
   `(org-headline-done ((,class (:height 1.0 :weight normal :strike-through t :foreground "#ADADAD"))))
   `(org-hide ((,class (:foreground "#E2E2E2"))))
   `(org-inlinetask ((,class (:box (:line-width 1 :color "#EBEBEB") :foreground "#777777" :background "#FFFFD6"))))
   `(org-latex-and-related ((,class (:foreground "#336699" :background "white"))))
   `(org-link ((,class ,link)))
   `(org-list-dt ((,class (:weight bold :foreground "#335EA8"))))
   `(org-macro ((,class (:weight bold :foreground "#EDB802"))))
   `(org-meta-line ((,class (:slant normal :foreground "#008ED1" :background "#EAEAFF"))))
   `(org-mode-line-clock ((,class ,clock-line)))
   `(org-mode-line-clock-overrun ((,class (:weight bold :box (:line-width 1 :color "#335EA8") :foreground "white" :background "#FF4040"))))
   `(org-number-of-items ((,class (:weight bold :foreground "white" :background "#79BA79"))))
   `(org-property-value ((,class (:foreground "#00A000"))))
   `(org-quote ((,class (:slant italic :foreground "dim gray" :background "#FFFFE0"))))
   `(org-scheduled ((,class (:foreground "#333333"))))
   `(org-scheduled-previously ((,class (:foreground "#F22659"))))
   `(org-scheduled-today ((,class (:weight bold :foreground "#4F4A3D" :background "#FFFFCC"))))
   `(org-sexp-date ((,class (:foreground "#3774CC"))))
   `(org-special-keyword ((,class (:weight bold :foreground "#00BB00" :background "#EAFFEA"))))
   `(org-table ((,class (:foreground "dark green" :background "#EAFFEA"))))
   `(org-tag ((,class (:weight normal :slant italic :foreground "#9A9FA4" :background "white"))))
   `(org-target ((,class (:foreground "#FF6DAF"))))
   `(org-time-grid ((,class (:foreground "#CFCFCF"))))
   `(org-todo ((,class (:weight bold :box (:line-width 1 :color "#D8ABA7") :foreground "#D8ABA7" :background "#FFE6E4"))))
   `(org-upcoming-deadline ((,class (:foreground "#FF5555"))))
   `(org-verbatim ((,class (:foreground "#0066CC"))))
   `(org-verse ((,class (:slant italic :foreground "dim gray" :background "#EEEEEE"))))
   `(org-warning ((,class (:weight bold :foreground "black" :background "#CCE7FF"))))

   `(org-level-1 ((,class ,ol1)))
   `(org-level-2 ((,class ,ol2)))
   `(org-level-3 ((,class ,ol3)))
   `(org-level-4 ((,class ,ol4)))
   `(org-level-5 ((,class ,ol5)))
   `(org-level-6 ((,class ,ol6)))
   `(org-level-7 ((,class ,ol7)))
   `(org-level-8 ((,class ,ol8)))

   `(outline-1 ((,class ,ol1)))
   `(outline-2 ((,class ,ol2)))
   `(outline-3 ((,class ,ol3)))
   `(outline-4 ((,class ,ol4)))
   `(outline-5 ((,class ,ol5)))
   `(outline-6 ((,class ,ol6)))
   `(outline-7 ((,class ,ol7)))
   `(outline-8 ((,class ,ol8)))

   `(pabbrev-debug-display-label-face ((,class (:background "chartreuse"))))
   `(pabbrev-suggestions-face ((,class (:weight bold :foreground "white" :background "red"))))
   `(pabbrev-suggestions-label-face ((,class (:weight bold :foreground "white" :background "purple"))))

   ;; smartparens
   `(sp-pair-overlay-face ((,class (:background ,s-base01))))
   `(sp-wrap-overlay-face ((,class (:background ,s-base02))))
   `(sp-wrap-tag-overlay-face ((,class (:background "#058924"))))
   `(sp-show-pair-enclosing ((,class (:inherit highlight :background ,s-base00))))
   `(sp-show-pair-match-face ((,class (:background ,green :weight bold))))
   `(sp-show-pair-mismatch-face ((,class (:background ,red))))

   `(paren-face-match ((,class ,paren-matched)))
   `(paren-face-mismatch ((,class ,paren-unmatched)))
   `(paren-face-no-match ((,class ,paren-unmatched)))

   `(persp-selected-face ((,class (:weight bold :foreground "#EEF5FE"))))
   `(powerline-active1 ((,class (:background "grey22" :inherit mode-line))))
   `(powerline-active2 ((,class (:background "#4070B6" :inherit mode-line))))
   `(powerline-inactive1 ((,class (:background "#686868" :inherit mode-line-inactive))))
   `(powerline-inactive2 ((,class (:background "#A9A9A9" :inherit mode-line-inactive))))
   `(rainbow-delimiters-depth-1-face ((,class (:foreground "#707183"))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground "#7388D6"))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground "#909183"))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground "#709870"))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground "#907373"))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground "#6276BA"))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground "#858580"))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground "#80A880"))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground "#887070"))))
   `(rainbow-delimiters-mismatched-face ((,class ,paren-unmatched)))
   `(rainbow-delimiters-unmatched-face ((,class ,paren-unmatched)))
   `(recover-this-file ((,class (:weight bold :background "#FF3F3F"))))
   `(rng-error ((,class (:weight bold :foreground "red" :background "#FBE3E4"))))
   `(sh-heredoc ((,class (:foreground "blue" :background "#EEF5FE"))))
   `(sh-quoted-exec ((,class (:foreground "#FF1493"))))
   `(shadow ((,class ,shadow)))         ; Used for grep context lines.
   `(shell-option-face ((,class (:foreground "forest green"))))
   `(shell-output-2-face ((,class (:foreground "blue"))))
   `(shell-output-3-face ((,class (:foreground "purple"))))
   `(shell-output-face ((,class (:foreground "black"))))
   ;; `(shell-prompt-face ((,class (:weight bold :foreground "yellow"))))
   `(shm-current-face ((,class (:background "#EEE8D5"))))
   `(shm-quarantine-face ((,class (:background "lemonchiffon"))))
   `(show-paren-match ((,class ,paren-matched)))
   `(show-paren-mismatch ((,class ,paren-unmatched)))
   `(sml-modeline-end-face ((,class (:background "#6BADF6")))) ; #335EA8
   `(sml-modeline-vis-face ((,class (:background "#1979CA"))))
   `(speedbar-button-face ((,class (:foreground "green4"))))
   `(speedbar-directory-face ((,class (:foreground "blue4"))))
   `(speedbar-file-face ((,class (:foreground "cyan4"))))
   `(speedbar-highlight-face ((,class ,volatile-highlight)))
   `(speedbar-selected-face ((,class (:underline t :foreground "red"))))
   `(speedbar-tag-face ((,class (:foreground "brown"))))
   `(svn-status-directory-face ((,class ,directory)))
   `(svn-status-filename-face ((,class (:weight bold :foreground "#4183C4"))))
   `(svn-status-locked-face ((,class (:weight bold :foreground "red"))))
   `(svn-status-marked-face ((,class ,marked-line)))
   `(svn-status-marked-popup-face ((,class (:weight bold :foreground "green3"))))
   `(svn-status-switched-face ((,class (:slant italic :foreground "gray55"))))
   `(svn-status-symlink-face ((,class ,symlink)))
   `(svn-status-update-available-face ((,class (:foreground "orange"))))
   `(tex-verbatim ((,class (:foreground "blue"))))
   `(tool-bar ((,class (:box (:line-width 1 :style released-button) :foreground "black" :background "gray75"))))
   `(tooltip ((,class (:foreground "black" :background "light yellow"))))
   `(trailing-whitespace ((,class (:background "#F6EBFE"))))
   `(traverse-match-face ((,class (:weight bold :foreground "blue violet"))))
   `(vc-annotate-face-3F3FFF ((,class (:foreground "#3F3FFF" :background "black"))))
   `(vc-annotate-face-3F6CFF ((,class (:foreground "#3F3FFF" :background "black"))))
   `(vc-annotate-face-3F99FF ((,class (:foreground "#3F99FF" :background "black"))))
   `(vc-annotate-face-3FC6FF ((,class (:foreground "#3F99FF" :background "black"))))
   `(vc-annotate-face-3FF3FF ((,class (:foreground "#3FF3FF" :background "black"))))
   `(vc-annotate-face-3FFF56 ((,class (:foreground "#4BFF4B" :background "black"))))
   `(vc-annotate-face-3FFF83 ((,class (:foreground "#3FFFB0" :background "black"))))
   `(vc-annotate-face-3FFFB0 ((,class (:foreground "#3FFFB0" :background "black"))))
   `(vc-annotate-face-3FFFDD ((,class (:foreground "#3FF3FF" :background "black"))))
   `(vc-annotate-face-56FF3F ((,class (:foreground "#4BFF4B" :background "black"))))
   `(vc-annotate-face-83FF3F ((,class (:foreground "#B0FF3F" :background "black"))))
   `(vc-annotate-face-B0FF3F ((,class (:foreground "#B0FF3F" :background "black"))))
   `(vc-annotate-face-DDFF3F ((,class (:foreground "#FFF33F" :background "black"))))
   `(vc-annotate-face-FF3F3F ((,class (:foreground "#FF3F3F" :background "black"))))
   `(vc-annotate-face-FF6C3F ((,class (:foreground "#FF3F3F" :background "black"))))
   `(vc-annotate-face-FF993F ((,class (:foreground "#FF993F" :background "black"))))
   `(vc-annotate-face-FFC63F ((,class (:foreground "#FF993F" :background "black"))))
   `(vc-annotate-face-FFF33F ((,class (:foreground "#FFF33F" :background "black"))))
   `(w3m-anchor ((,class ,link)))
   `(w3m-arrived-anchor ((,class (:foreground "purple1"))))
   `(w3m-bitmap-image-face ((,class (:foreground "gray4" :background "green"))))
   `(w3m-bold ((,class (:weight bold :foreground "black"))))
   `(w3m-current-anchor ((,class (:weight bold :underline t :foreground "blue"))))
   `(w3m-form ((,class (:underline t :foreground "tan1"))))
   `(w3m-form-button-face ((,class (:weight bold :underline t :foreground "gray4" :background "light grey"))))
   `(w3m-form-button-mouse-face ((,class (:underline t :foreground "light grey" :background "#2B7E2A"))))
   `(w3m-form-button-pressed-face ((,class (:weight bold :underline t :foreground "gray4" :background "light grey"))))
   `(w3m-header-line-location-content-face ((,class (:foreground "#7F7F7F":background "#F7F7F7"))))
   `(w3m-header-line-location-title-face ((,class (:foreground "#2C55B1" :background "#F7F7F7"))))
   `(w3m-history-current-url-face ((,class (:foreground "lemon chiffon"))))
   `(w3m-image-face ((,class (:weight bold :foreground "DarkSeaGreen2"))))
   `(w3m-link-numbering ((,class (:foreground "#B4C7EB")))) ; mouseless browsing
   `(w3m-strike-through-face ((,class (:strike-through t))))
   `(w3m-underline-face ((,class (:underline t))))
   `(which-func ((,class (:weight bold :foreground "white"))))
   `(widget-button ((,class ,link)))
   `(widget-button-pressed ((,class (:foreground "red"))))
   `(widget-documentation ((,class (:foreground "green4"))))
   `(widget-field ((,class (:background "gray85"))))
   `(widget-inactive ((,class (:foreground "dim gray"))))
   `(widget-single-line-field ((,class (:background "gray85"))))
   `(yas/field-debug-face ((,class (:background "ivory2"))))
   `(yas/field-highlight-face ((,class (:background "DarkSeaGreen1"))))


   ;;;;; Web Mode Faces (most inherit from elsewhere, only a few are web-mode only)
   ;;;
   `(web-mode-html-tag-face                  ((,class (:foreground ,blue-l :weight bold))))
   ;; `(web-mode-html-tag-custom-face           ((,class (:foreground , :background ,bgxx))))
   ;; `(web-mode-html-tag-bracket-face          ((,class (:foreground ,fgxx :background ,bgxx))))
   `(web-mode-html-attr-name-face            ((,class (:foreground ,attr))))
   ;; `(web-mode-html-attr-custom-face          ((,class (:foreground ,fgxx :background ,bgxx))))
   ;; `(web-mode-inlay-face                     ((,class (:foreground ,fgxx :background ,bgxx))))
   ;; `(web-mode-block-face                     ((,class (:foreground ,fgxx :background ,bgxx))))
   ;; `(web-mode-current-element-highlight-face ((,class (:foreground ,fgxx :background ,bgxx))))
   ;; `(web-mode-current-column-highlight-face  ((,class (:foreground ,fgxx :background ,bgxx))))
   ;; `(web-mode-html-entity-face               ((,class (:foreground ,fgxx :background ,bgxx))))
   ;; `(web-mode-jsx-depth-1-face               ((,class (:foreground ,fgxx :background ,bgxx))))
   ;; `(web-mode-jsx-depth-2-face               ((,class (:foreground ,fgxx :background ,bgxx))))
   ;; `(web-mode-jsx-depth-3-face               ((,class (:foreground ,fgxx :background ,bgxx))))
   ;; `(web-mode-jsx-depth-4-face               ((,class (:foreground ,fgxx :background ,bgxx))))
   ;; `(web-mode-jsx-depth-5-face               ((,class (:foreground ,fgxx :background ,bgxx))))

   ;;;;;;;

   ;; `(web-mode-error-face                   ((,class (:foreground ,fgxx :background ,bgxx))))
   ;; `(web-mode-warning-face                 ((,class (:foreground ,fgxx :background ,bgxx))))
   ;; `(web-mode-preprocessor-face            ((,class (:foreground ,fgxx :background ,bgxx))))
   ;; `(web-mode-preprocessor-face            ((,class (:foreground ,fgxx :background ,bgxx))))
   ;; `(web-mode-block-delimiter-face         ((,class (:foreground ,fgxx :background ,bgxx))))
   ;; `(web-mode-block-control-face           ((,class (:foreground ,fgxx :background ,bgxx))))
   ;; `(web-mode-builtin-face                 ((,class (:foreground ,fgxx :background ,bgxx))))
   ;; `(web-mode-symbol-face                  ((,class (:foreground ,fgxx :background ,bgxx))))
   ;; `(web-mode-doctype-face                 ((,class (:foreground ,fgxx :background ,bgxx))))
   ;; `(web-mode-html-tag-unclosed-face       ((,class (:foreground ,fgxx :background ,bgxx))))
   ;; `(web-mode-html-tag-namespaced-face     ((,class (:foreground ,fgxx :background ,bgxx))))
   ;; `(web-mode-html-attr-engine-face
   ;; `(web-mode-html-attr-equal-face
   ;; `(web-mode-html-attr-value-face
   ;; `(web-mode-block-attr-name-face
   ;; `(web-mode-block-attr-value-face
   ;; `(web-mode-variable-name-face
   ;; `(web-mode-css-selector-face
   ;; `(web-mode-css-pseudo-class-face
   ;; `(web-mode-css-at-rule-face
   ;; `(web-mode-css-property-name-face
   ;; `(web-mode-css-color-face
   ;; `(web-mode-css-priority-face
   ;; `(web-mode-css-function-face
   ;; `(web-mode-css-variable-face
   ;; `(web-mode-function-name-face
   ;; `(web-mode-filter-face
   ;; `(web-mode-function-call-face
   ;; `(web-mode-string-face
   ;; `(web-mode-block-string-face
   ;; `(web-mode-part-string-face
   ;; `(web-mode-javascript-string-face
   ;; `(web-mode-interpolate-color1-face
   ;; `(web-mode-interpolate-color2-face
   ;; `(web-mode-interpolate-color3-face
   ;; `(web-mode-css-string-face
   ;; `(web-mode-json-key-face
   ;; `(web-mode-json-context-face
   ;; `(web-mode-json-string-face
   ;; `(web-mode-comment-face
   ;; `(web-mode-block-comment-face
   ;; `(web-mode-part-comment-face
   ;; `(web-mode-json-comment-face
   ;; `(web-mode-javascript-comment-face
   ;; `(web-mode-css-comment-face
   ;; `(web-mode-annotation-face
   ;; `(web-mode-annotation-tag-face
   ;; `(web-mode-annotation-type-face
   ;; `(web-mode-annotation-value-face
   ;; `(web-mode-annotation-html-face
   ;; `(web-mode-constant-face
   ;; `(web-mode-type-face
   ;; `(web-mode-keyword-face
   ;; `(web-mode-param-name-face
   ;; `(web-mode-whitespace-face
   ;; `(web-mode-part-face
   ;; `(web-mode-script-face
   ;; `(web-mode-style-face
   ;; `(web-mode-folded-face
   ;; `(web-mode-bold-face
   ;; `(web-mode-italic-face
   ;; `(web-mode-underline-face
   ;; `(web-mode-comment-keyword-face
   ;; `(web-mode-sql-keyword-face
   ))

(custom-theme-set-variables 'leuven-solar
                            '(ansi-color-faces-vector
                              [default default default italic underline success warning error])
                            '(ansi-color-names-vector
                              ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
                                        ; colors used in Shell mode
                            )

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  ;; add theme folder to `custom-theme-load-path' when installing over MELPA
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'leuven-solar)

;; This is for the sake of Emacs.
;; Local Variables:
;; no-byte-compile: t
;; time-stamp-end: "$"
;; time-stamp-format: "%:y%02m%02d.%02H%02M"
;; time-stamp-start: "Version: "
;; End:

;;; leuven-solar-theme.el ends here
