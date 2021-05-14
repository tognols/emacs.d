;;Melpa
(require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)

;;GUI
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(menu-bar-mode -1)


;;Evil mode
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)
(global-evil-leader-mode)

;; ICONS

(require 'all-the-icons)


;; MODELINE

;;Powerline theme
;;(require 'powerline)
;;(powerline-center-evil-theme)

;;Doom modeline
(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-bar-width 1)

;;Doom Dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-banner-logo-title "Welcome to Emacs!")
(setq dashboard-startup-banner 'logo)
;;(setq dashboard-center-content t)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-show-shortcuts nil)
(add-to-list 'dashboard-items '(agenda) t)

;;Colorscheme
;;(load-theme 'tango-dark t)	

;;Disable Startup Screen
(setq inhibit-startup-screen t)

;;Disable backup files
(setq make-backup-files nil)
(setq create-lockfiles nil)

;;Disable AutoSave
(setq auto-save-default nil)

;;Display numbers
;;(global-display-line-numbers-mode)
(global-linum-mode 1)

;;Nyan mode
(add-hook 'after-init-hook 'nyan-mode)
(setq nyan-animate-nyancat t)
(setq nyan-wavy-trail t)
;;
;; C++ STUFF
;;

;;Code highlight
(modern-c++-font-lock-global-mode t)

;; C-Like
(dolist (mode-iter '(c-mode c++-mode glsl-mode java-mode javascript-mode rust-mode))
  (font-lock-add-keywords
    mode-iter
    '(("\\([~^&\|!<>=,.\\+*/%-]\\)" 0 'font-lock-operator-face keep)))
  (font-lock-add-keywords
    mode-iter
    '(("\\([\]\[}{)(:;]\\)" 0 'font-lock-delimit-face keep)))
  ;; functions
  (font-lock-add-keywords
    mode-iter
    '(("\\([_a-zA-Z][_a-zA-Z0-9]*\\)\s*(" 1 'font-lock-function-name-face keep))))
;;Company mode
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)


;;LSP MODE

(require 'lsp-mode)
(add-hook 'c++-mode-hook #'lsp)
(setq lsp-enable-snippet t)
(setq lsp-headerline-breadcrumb-enable nil)
;;YASNIPPET
(yas-global-mode 1)

;; DASHBOARD STUFF
(add-hook 'dashboard-mode-hook (lambda () (progn
					   (setq left-margin-width 13)
					   (setq right-margin-width 0))))
;;
;; ORG CONFIGS
;;

;; TRUNCATE!
(add-hook 'org-mode-hook (lambda () (progn (toggle-truncate-lines))))
(global-visual-line-mode t)

;;NO LINUM
(add-hook 'org-mode-hook (lambda () (linum-mode 0)))

;; PADDING and SPACING
(add-hook 'org-mode-hook (lambda () (progn
  (setq header-line-format " ")
  (setq left-margin-width 2)
  (setq line-spacing 0.1)
  (setq right-margin-width 2)
  (set-window-buffer nil (current-buffer)))))

;;Bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 0)))

;; Poet and variable pitch mode
(add-hook 'text-mode-hook
           (lambda ()
             (variable-pitch-mode 1)))
(set-face-attribute 'default nil :family "PragmataPro" :foundry "outline" :slant 'normal :weight 'normal :height 120 :width 'normal)
(set-face-attribute 'fixed-pitch nil :family "PragmataPro" :foundry "outline" :slant 'normal :weight 'normal :height 120 :width 'normal)
(set-face-attribute 'variable-pitch nil :family "IBM Plex Serif")

;; KEYBINDINGS
(global-set-key (kbd "C-x w") 'writeroom-mode)

;; Windows performance tweaks
;;
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))
;; Set the buffer size to 64K on Windows (from the original 4K)
(when (boundp 'w32-pipe-buffer-size)
  (setq irony-server-w32-pipe-buffer-size (* 64 1024)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes '(vscode-dark-plus))
 '(custom-safe-themes
   '("f490984d405f1a97418a92f478218b8e4bcc188cf353e5dd5d5acd2f8efd0790" "35c096aa0975d104688a9e59e28860f5af6bb4459fd692ed47557727848e6dfe" "28a104f642d09d3e5c62ce3464ea2c143b9130167282ea97ddcc3607b381823f" "2d035eb93f92384d11f18ed00930e5cc9964281915689fa035719cab71766a15" "d44d470f27bd068eaa3b786e8ba241dad39b5c0db5602abc490276419a361f35" "8f567db503a0d27202804f2ee51b4cd409eab5c4374f57640317b8fcbbd3e466" default))
 '(fci-rule-color "#dedede")
 '(line-spacing 0.2)
 '(package-selected-packages
   '(writeroom-mode org-bullets all-the-icons-gnus doom-modeline yasnippet vscode-dark-plus-theme lsp-mode modern-cpp-font-lock ## company molokai-theme nyan-mode powerline evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "PragmataPro" :foundry "outline" :slant normal :weight normal :height 120 :width normal))))
 '(header-line ((t (:inherit nil :background "gray12" :foreground "#ffffff" :box nil)))))

