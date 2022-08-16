;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Matteo Tognolo"
      user-mail-address "matteo@tognolo.it")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Consolas NF" :size 16 :weight 'Medium))
(setq doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 16))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dark+)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-agenda-files '("~/org"))


(setq-default line-spacing 1)
;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; ORG MODE CONFIGURATION
(after! org
(let* ((variable-tuple
        (cond
         ((x-list-fonts   "Source Sans Pro") '(:font   "Source Sans Pro"))
              ((x-family-fonts "Sans Serif")      '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font."))))
       (base-font-color (face-foreground 'default nil 'default))
       (headline `(:inherit default :weight bold
                   :foreground ,base-font-color)))

  (custom-theme-set-faces
   'user
        '(org-level-8        ((t (,@headline ,@variable-tuple))))
        '(org-level-7        ((t (,@headline ,@variable-tuple))))
        '(org-level-6        ((t (,@headline ,@variable-tuple))))
        '(org-level-5        ((t (,@headline ,@variable-tuple))))
        '(org-level-4        ((t (,@headline ,@variable-tuple :height 1.0))))
        '(org-level-3        ((t (,@headline ,@variable-tuple :height 1.15))))
        '(org-level-2        ((t (,@headline ,@variable-tuple :height 1.25))))
        '(org-level-1        ((t (,@headline ,@variable-tuple :height 1.5))))
        '(org-headline-done  ((t (,@headline ,@variable-tuple :strike-through t))))
        '(org-document-title ((t (,@headline ,@variable-tuple
                                        :height 2.0 :underline nil))))))
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'global-display-line-numbers-mode)
;;  (add-hook 'org-mode-hook 'writeroom-mode)

  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit fixed-pitch))))
   '(org-table ((t (:inherit fixed-pitch))))
   )
  (setq org-roam-directory "~/org/roam/")
  (setq org-roam-index-file "~/org/roam/index.org")
)
