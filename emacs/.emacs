(add-to-list 'load-path "~/.emacs.d/elisp")

;; https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
;; Set up package repositories so M-x package-install works.
(require 'package)
(setq package-list '(go-mode
                     lsp-mode
                     lsp-ui
                     eglot
                     exec-path-from-shell
                     company
                     flyspell
                     xcscope
                     json-mode
                     atom-dark-theme
                     yasnippet
                     whitespace
                     edit-indirect
                     deft
                     tramp
                     markdown-mode))
(add-to-list 'package-archives
             '("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/"))
(add-to-list 'package-archives
             '("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
;; activate all the packages (in particular autoloads)
(package-initialize)
;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
        (package-install package)))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;; High level aesthetic stuff
(tool-bar-mode -1)                  ; Disable the button bar atop screen
;; (scroll-bar-mode -1)                ; Disable scroll bar
(setq inhibit-startup-screen t)     ; Disable startup screen with graphics
;; (set-default-font "Monaco 12")      ; Set font and size
;; (setq-default indent-tabs-mode nil) ; Use spaces instead of tabs
;; (setq tab-width 4)                  ; Four spaces is a tab
(setq visible-bell nil)             ; Disable annoying visual bell graphic
(setq ring-bell-function 'ignore)   ; Disable super annoying audio bell

;; Make keyboard bindings not suck
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; (load-theme 'atom-dark t)       ; Color theme installed via melpa

;; Lines should never be more than 89 characters in length, and in general should be 80 characters or less.
;; (setq-default default-fill-column 80)
;; Indentation (tab stop) should be 3 spaces.
(setq standard-indent 4)

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

;; (setq-default c-default-style "c")
(add-hook 'c-mode-common-hook
          (lambda ()
            (flyspell-prog-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dtrt-indent-global-mode t)
 '(package-selected-packages
   '(counsel dtrt-indent lua-mode yaml-mode dockerfile-mode go-tag gotest flycheck lsp-mode go-mode cmake-mode meson-mode unicad protobuf-mode clang-format magit tramp edit-indirect atom-dark-theme xcscope json-mode go-guru go-autocomplete)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(set-face-attribute 'default nil :height 160)

(require 'tramp)
(setq tramp-default-method "ssh")


;; https://github.com/kljohann/clang-format.el/issues/5
;; (defun my-clang-format-region ()
;;   (interactive)
;;   (let ((start (if (use-region-p) (region-beginning) (point)))
;;         (end (if (use-region-p) (region-end) (point)))
;;         (assumed-filename (if (file-remote-p buffer-file-name)
;;                                (concat (getenv "HOME") "/" (file-name-nondirectory buffer-file-name))
;;                              buffer-file-name)))
;;     (clang-format-region start end clang-format-style assumed-filename)))
(global-set-key (kbd "C-M-,") 'clang-format-region)

(global-display-line-numbers-mode)

;;
;; use use-package
;;
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq gofmt-command "goimports")
(add-hook 'before-save-hook #'gofmt-before-save)

(when (eq system-type 'darwin)
  (setq exec-path-from-shell-arguments '("-l"))
  (add-hook 'after-init-hook #'exec-path-from-shell-initialize)
  (with-eval-after-load "go-mode"
    (with-eval-after-load "exec-path-from-shell"
      (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))))

(add-hook 'prog-mode-hook #'flymake-mode)
(with-eval-after-load "flymake"
  (define-key flymake-mode-map (kbd "C-c C-b") 'flymake-show-diagnostics-buffer)
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

(setq gofmt-show-errors nil)

(add-hook 'prog-mode-hook #'company-mode)
(setq company-tooltip-limit 10
      company-tooltip-align-annotations t
      company-tooltip-width-grow-only t
      company-abort-manual-when-too-short t
      company-require-match nil
      company-backends '(company-capf)
      company-tooltip-margin 0)
(with-eval-after-load "company"
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-n") #'company-select-next))


(add-hook 'go-mode-hook #'eglot-ensure)
(setq eglot-ignored-server-capabilites '(:documentHighlightProvider)
      read-process-output-max (* 1024 1024))

(setq eldoc-idle-dealy 2)

(add-hook 'prog-mode-hook #'yas-minor-mode)

(setq-default eglot-workspace-configuration
              '((gopls
                 (usePlaceholders . t))))

(defun my-c++-mode-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'innamespace [0]))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;;(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
