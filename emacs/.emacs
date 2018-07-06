(add-to-list 'load-path "~/.emacs.d/elisp")

;; https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
;; Set up package repositories so M-x package-install works.
(require 'package)
(setq package-list '(go-autocomplete
                     go-guru
                     flyspell
                     xcscope
                     json-mode
                     atom-dark-theme
                     whitespace
                     edit-indirect
                     deft
                     markdown-mode))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; activate all the packages (in particular autoloads)
(package-initialize)
;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
        (package-install package)))

;; https://johnsogg.github.io/emacs-golang
;; High level aesthetic stuff
(tool-bar-mode -1)                  ; Disable the button bar atop screen
(scroll-bar-mode -1)                ; Disable scroll bar
(setq inhibit-startup-screen t)     ; Disable startup screen with graphics
;; (set-default-font "Monaco 12")      ; Set font and size
(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs
(setq tab-width 2)                  ; Four spaces is a tab
(setq visible-bell nil)             ; Disable annoying visual bell graphic
(setq ring-bell-function 'ignore)   ; Disable super annoying audio bell

;; Make keyboard bindings not suck
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

(load-theme 'atom-dark t)       ; Color theme installed via melpa

;; Snag the user's PATH and GOPATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;; Define function to call when go-mode loads
(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))

  ;; guru settings
  (go-guru-hl-identifier-mode)                    ; highlight identifiers

  ;; Key bindings specific to go-mode
  (local-set-key (kbd "M-.") 'godef-jump)         ; Go to definition
  (local-set-key (kbd "M-*") 'pop-tag-mark)       ; Return from whence you came
  (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
  (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
  (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
  (local-set-key (kbd "M-[") 'previous-error)     ; Go to previous error or msg

  ;; Misc go stuff
  (auto-complete-mode 1))                         ; Enable auto-complete mode
;; Connect go-mode-hook with the function we just defined
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Ensure the go specific autocomplete is active in go-mode.
(with-eval-after-load 'go-mode
   (require 'go-autocomplete))

;; If the go-guru.el file is in the load path, this will load it.
(require 'go-guru)


;; Lines should never be more than 89 characters in length, and in general should be 80 characters or less.
(setq-default default-fill-column 80)
;; Indentation (tab stop) should be 3 spaces.
(setq standard-indent 3)

;; (setq line-number-display-limit 100000000)
(setq ring-bell-function 'ignore)

;; display time on mode-line
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-use-mail-icon t)
(setq display-time-interval 10)

;; backup setting
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 5)
(setq delete-old-versions t)
(setq backup-directory-alist '(("." . "~/var/tmp")))
(setq backup-by-copying t)

;; ido mode
(setq ido-enable-tramp-completion t)
(setq ido-save-directory-list-file nil)
(setq ido-max-directory-size 100000)
(ido-mode 1)

(setq visible-bell t)
(global-font-lock-mode 1)
(auto-compression-mode 1)
(column-number-mode 1)
(blink-cursor-mode -1)
(display-time-mode 1)
(show-paren-mode 1)
(icomplete-mode 1)
(transient-mark-mode t)

(require 'flyspell)

;; flyspell minor mode

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(setq flyspell-issue-message-flag nil)
(global-set-key "\M-n" 'flyspell-auto-correct-word)

;; cscope
(require 'xcscope)
(cscope-setup)

;; scons
(setq auto-mode-alist
      (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("SConscript" . python-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.sc$" . python-mode) auto-mode-alist))

;; json-mode
(require 'json-mode)

(setq-default show-trailing-whitespace t)
(require 'whitespace)

;; https://emacs.stackexchange.com/questions/147/how-can-i-get-a-ruler-at-column-80
(setq-default
 whitespace-line-column 80
  whitespace-style       '(face lines-tail))
(add-hook 'prog-mode-hook #'whitespace-mode)

(add-hook 'c-mode-common-hook
          (lambda ()
            (flyspell-prog-mode)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (edit-indirect atom-dark-theme xcscope json-mode go-guru go-autocomplete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
