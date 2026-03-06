;;; miao-var.el --- Miao variables  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Internal variables and customizable variables.

;;; Code:


(defvar-local miao--current-state nil
  "A symbol represent current state.")

(defvar-local miao--leader-previous-state nil
  "A symbol represent current state.")

(defvar miao--leader-keys nil)
(defvar miao--prefix-arg nil)

(defvar miao-modeline-indicators '((normal . "N")
                                   (insert . "I")
                                   (leader . "L")
                                   (bypass . "B")))

(defvar miao-state-colors
  '((normal . "#51afef")
    (insert . "#98be65")
    (leader . "#da8548")
    (bypass . "gray")))

(defvar miao-leader-major-keymap-hash
  #s(hash-table))

(defvar miao-leader-mode-keys
  ;; ! → / (33, 47)
  ;; 0 → 9 (48, 57)
  ;; : → @ (58, 64)
  ;; A -> Z
  ;; ^ -> `
  ;; a -> z
  ;; { -> ~
  (mapcar
   (lambda (c)
     (if (numberp c)
       (char-to-string c)
       c))
   (append '("SPC")
           (number-sequence ?! ?/)
           (number-sequence ?0 ?9)
           (number-sequence ?: ?@)
           (number-sequence ?A ?Z)
           (number-sequence ?^ ?`)
           (number-sequence ?a ?z)
           (number-sequence ?\{ ?~)
           '("<up>" "<down>" "<left>" "<right>"))))

(defcustom miao-leader-describe-delay
  0.5
  "The delay in seconds before popup keybinding descriptions appear."
  :group 'miao
  :type 'number)

(defvar miao--leader-keymap-description-activated nil
  "Whether LEADER keymap description is already activated.")

(defvar miao-bypass-mode-list
  '(calc-mode
    dired-mode
    pdf-view-mode
    magit-status-mode
    ediff-mode
    mu4e-main-mode
    mu4e-headers-mode
    org-agenda-mode))

(defvar miao-bypass-keymap-hash
  #s(hash-table))

(defvar miao-bypass-mode-keys
  (append (number-sequence ?! ?/)
          (number-sequence ?0 ?9)
          (number-sequence ?: ?@)
          (number-sequence ?A ?Z)
          (number-sequence ?^ ?`)
          (number-sequence ?a ?z)
          (number-sequence ?\{ ?~)))


(defun mix-colors (color1 color2 &optional alpha)
  "Mix two colors COLOR1 and COLOR2.
The optional ALPHA argument controls the mix ratio (0.0 is all COLOR1, 1.0 is all COLOR2)."
  (let* ((rgb1 (color-values color1))
         (rgb2 (color-values color2))
         (r1 (nth 0 rgb1)) (g1 (nth 1 rgb1)) (b1 (nth 2 rgb1))
         (r2 (nth 0 rgb2)) (g2 (nth 1 rgb2)) (b2 (nth 2 rgb2))
         (alpha (or alpha 0.5))  ; Default to 50% mix
         (r (round (+ (* (- 1 alpha) r1) (* alpha r2))))
         (g (round (+ (* (- 1 alpha) g1) (* alpha g2))))
         (b (round (+ (* (- 1 alpha) b1) (* alpha b2)))))
    (format "#%02x%02x%02x" r g b)))

(defvar miao--miacro-overlays nil)
(defvar miao--miacro-fake-cursors nil)

(defvar miao--miacro-selection nil)
(defvar miao--macro-ignite-offset nil)

(defface miao-miacro-fake-cursor
  `((t :box (:line-width (1 . 1)
             :color ,(mix-colors (face-background 'cursor nil t) (face-background 'region nil t))
             :style flat-button)
       :background "transparent"))
  "Miao beacon fake cursor."
  :group 'miao)

(defface miao-miacro-fake-symbol
  `((t :background ,(mix-colors
                     (face-background 'cursor nil t)
                     (face-background 'region nil t))))
  "Miao beacon fake region."
  :group 'miao)

(defface miao-modeline-face
  '((t :weight bold))
  "Normal state indicator."
  :group 'miao)

(provide 'miao-var)
