;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; BEHAVIOR
(setq warning-suppress-types '((lsp-mode)))

(setq confirm-kill-emacs nil) ;; making it so emacs doesnt beg for mercy when i try to kill her
(setq-default tab-width 4)

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
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 13)
      display-line-numbers 'relative
      doom-theme 'catppuccin
      catppuccin-flavor 'mocha)

;; KEYBINDS
(map!
 "C-c M-x" #'kill-buffer
 "C-c C-x" #'kill-current-buffer
 "C-c C-n" "noh<CR>"
 "C-b" ":noh<CR>"

 (:map evil-normal-state-map
       "U" #'evil-redo
       "<backtab>" #'centaur-tabs-backward
       "<tab>" #'centaur-tabs-forward)

 (:leader
  :desc "Dired"
  "d" #'dired
  :desc "Treemacs"
  "e" #'treemacs)

 (:map evil-motion-state-map
       "C-b" nil
       "<tab>" nil)

 (:map dired-mode-map
      :after dired
      :n "+" #'dired-create-empty-file
      :n "f" #'dired-create-directory
      :n "p" #'direc-up-directory

      :desc "Create directory"
      :n "C-f c" #'dired-create-directory
      :desc "Create file"
      :n "C-f n" #'dired-create-empty-file
      :desc "Delete file"
      :n "C-f d" #'dired-do-delete
      :desc "Go up a directory"
      :n "C-f u" #'dired-up-directory))

;; ORG MODE
(after! org
  (setq
   org-hide-emphasis-markers t
   org-hide-leading-stars nil

   org-todo-keywords
   '((sequence
      "TODO(t)"
      "HOMEWORK(h)"
      "DONE(d)"))
   org-todo-keyword-faces
   '(("DONE" . +org-todo-onhold)
     ("HOMEWORK" . +org-todo-active))))
