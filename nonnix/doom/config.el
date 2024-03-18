;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; BEHAVIOR
(setq confirm-kill-emacs nil) ;; making it so emacs doesnt beg for mercy when i try to kill her
(setq-default tab-width 2)

;; kiana helped me with these lines to make j and k based on visual lines
(setq evil-respect-visual-line-mode t)
(after! evil
  (evil-define-motion evil-next-line (count)
    :type line
    (let ((line-move-visual evil-respect-visual-line-mode))
      (evil-line-move (or count 1))))
  (evil-define-motion evil-previous-line (count)
    :type line
    (let ((line-move-visual evil-respect-visual-line-mode))
      (evil-line-move (- (or count 1))))))

;; VISUAL CHANGES
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 13))
(setq display-line-numbers 'relative)
(setq doom-theme 'catppuccin)
(setq catppuccin-flavor 'mocha)

;; KEYBINDS
(map!
 "C-c M-x" #'kill-buffer
 "C-c C-x" #'kill-current-buffer
 "C-c C-n" "noh<CR>"
 "C-b" ":noh<CR>")

(map! :map evil-normal-state-map
      "U" #'evil-redo
      "<backtab>" #'previous-buffer)

(map! :leader
      :desc "Dired"
      "e" #'dired)

(map! :map evil-motion-state-map
      "C-b" nil)

;; ORG MODE
(after! org
  (setq
   org-hide-emphasis-markers t
   org-hide-leading-stars nil))
