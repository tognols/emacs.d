;; ---------------------------------------------------------------------
;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2020 - N Λ N O developers
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------
(package-initialize)


;; Path to nano emacs modules (mandatory)
(add-to-list 'load-path "/Users/tgn/nano-emacs/")
(add-to-list 'load-path ".")

;; Default layout (optional)
(require 'nano-layout)

;; Theming Command line options (this will cancel warning messages)
(add-to-list 'command-switch-alist '("-dark"   . (lambda (args))))
(add-to-list 'command-switch-alist '("-light"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-default"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-help" . (lambda (args))))
(add-to-list 'command-switch-alist '("-compact" . (lambda (args))))


;;(cond
 ;;((member "-default" command-line-args) t)
 ;;((member "-dark" command-line-args) (require 'nano-theme-dark))
 ;;(t (require 'nano-theme-light)))

;; Customize support for 'emacs -q' (Optional)
;; You can enable customizations by creating the nano-custom.el file
;; with e.g. `touch nano-custom.el` in the folder containing this file.
(let* ((this-file  (or load-file-name (buffer-file-name)))
       (this-dir  (file-name-directory this-file))
       (custom-path  (concat this-dir "nano-custom.el")))
  (when (and (eq nil user-init-file)
             (eq nil custom-file)
             (file-exists-p custom-path))
    (setq user-init-file this-file)
    (setq custom-file custom-path)
    (load custom-file)))

;; Theme

;; switch to dark or light mode
(require 'nano-theme-dark)

(require 'nano-faces)
(nano-faces)

(require 'nano-theme)
(nano-theme)

;; Nano default settings (optional)
(require 'nano-defaults)

;; Nano session saving (optional)
(require 'nano-session)

;; Nano header & mode lines (optional)
(require 'nano-modeline)

;; Nano key bindings modification (optional)
(require 'nano-bindings)

;; WRITER MODE FOR COOL
(require 'nano-writer)


;; Compact layout (need to be loaded after nano-modeline)
(when (member "-compact" command-line-args)
  (require 'nano-compact))
  
;; Nano counsel configuration (optional)
;; Needs "counsel" package to be installed (M-x: package-install)
;; (require 'nano-counsel)

;; Welcome message (optional)
(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / N Λ N O edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

;; Splash (optional)
(unless (member "-no-splash" command-line-args)
  (require 'nano-splash))

;; Help (optional)
(unless (member "-no-help" command-line-args)
  (require 'nano-help))

(provide 'nano)

;; MELPA for EVIL

(require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("~/org/todo.org"))
 '(package-selected-packages
   '(key-chord yasnippet-classic-snippets yasnippet-snippets tree-sitter-langs tree-sitter company lsp-ui lsp-mode mini-frame ## evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'evil)
(evil-mode 1)

;; Minibuffer
;;(require 'nano-minibuffer)
(add-hook 'org-mode-hook 'writer-mode)

;; Programming stuff
(require 'tree-sitter)
(require 'tree-sitter-langs)
(require 'lsp-mode)

(add-hook 'c++-mode-hook #'lsp)
(add-hook 'c++-mode-hook 'tree-sitter-mode)
(add-hook 'c++-mode-hook 'yas-minor-mode)
(add-hook 'c++-mode-hook 'display-line-numbers-mode)

;;lsp-specific
(setq lsp-headerline-breadcrumb-enable nil)

;;remap jk to esc 
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;;shortcuts for agenda
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
