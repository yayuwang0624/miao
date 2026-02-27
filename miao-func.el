;;; miao-func.el --- Miao variables  -*- lexical-binding: t; -*-

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


(defun miao-append ()
  (interactive)
  (miao-insert-mode t)
  (unless (eolp)
    (forward-char)))

(defun miao-insert-begin ()
  (interactive)
  (beginning-of-line-text)
  (miao-insert-mode))

(defun miao-insert-end ()
  (interactive)
  (end-of-line)
  (miao-insert-mode))

(defun miao-leader-quit ()
  "miao leader state quit and switch to previous state"
  (interactive)
  (when miao-leader-mode
    (miao-switch-to-previous-state)
    (setq miao--leader-previous-state nil)
    (setq miao--leader-keys nil)
    (setq overriding-local-map nil)))

(defun miao-mark-word ()
  (interactive)
  (when-let ((bound (bounds-of-thing-at-point 'word)))
    (goto-char (cdr bound))
    (set-mark (car bound))))

(defun miao-mark-symbol ()
  (interactive)
  (when-let ((bound (bounds-of-thing-at-point 'symbol)))
    (goto-char (cdr bound))
    (set-mark (car bound))))

(defun miao-mark-string ()
  (interactive)
  (when-let ((bound (bounds-of-thing-at-point 'string)))
    (goto-char (cdr bound))
    (set-mark (car bound))))

(defun miao-mark-string-inner ()
  (interactive)
  (when-let ((bound (bounds-of-thing-at-point 'string)))
    (goto-char (- (cdr bound) 1))
    (set-mark (+ 1 (car bound)))))

(defun miao-mark-list-inner ()
  (interactive)
  (let ((forward (or (not (region-active-p))
                     (equal (point) (region-end)))))
    (if (region-active-p)
        (if forward
            (forward-char)
          (backward-char)))
    (let ((begin
           (if (region-active-p)
               (condition-case nil
                   (or (backward-up-list 1 t t) (point))
                 (error (goto-char (- (region-beginning) (if forward 1 0)))))
             (cond ((looking-back "[\])}]") (backward-list))
                   ((looking-at "[\(\[\{]") (point))
                   (t (or (backward-up-list 1 t t)) (point)))))
          (end (scan-sexps (point) 1)))
      (if forward
          (progn
            (goto-char (- end 1))
            (set-mark (+ begin 1)))
        (goto-char (+ begin 1))
        (set-mark (- end 1))))))

(defun miao-mark-list ()
  (interactive)
  (let ((begin (if (region-active-p)
                   (or (backward-up-list 1 t t) (point))
                 (cond ((looking-back "[\])\{]") (backward-list))
                       ((looking-at "[\(\[\{]") (point))
                       (t (or (backward-up-list 1 t t)) (point)))))
        (end (scan-sexps (point) 1)))
    (goto-char end)
    (set-mark begin)))

(defun miao-toggle-mark-point ()
  (interactive)
  (if (region-active-p)
      (exchange-point-and-mark)))

(defun miao-delete-char ()
  (interactive)
  (delete-char 1))

(defun miao-quit-window ()
  (interactive)
  (quit-window))

(defun miao-delete-window ()
  (interactive)
  (delete-window))

(defun miao-mark-line ()
  (interactive)
  (if (region-active-p)
      (if (equal (point) (region-end))
          (progn (next-logical-line) (end-of-line))
        (progn (previous-logical-line) (beginning-of-line)))
    (let ((pos (point))
          (begin (pos-bol))
          (end (pos-eol)))
      (goto-char end)
      (set-mark begin))))

(defun miao-unmark-line ()
  (if (not miao-display-line-number)
      (display-line-numbers-mode -1)))

(defun miao-cursor-blink ()
  ;; TODO: use overlay instead of marking region
  (interactive)
  (let ((pos (point))
        (begin (pos-bol))
        (end (pos-eol)))
    (goto-char begin)
    (set-mark (+ 1 end))
    (run-with-idle-timer 0.5
                         nil
                         (lambda ()
                           (if (region-active-p)
                             (progn
                               (deactivate-mark)
                               (goto-char pos)))))))

(defun miao-till (ch &optional n)
  "Move point to the next occurrence of character CH.
If N is provided, move to the Nth occurrence. Defaults to 1.
Search is case-sensitive."
  (interactive "cTill: \nP")
  (let* ((case-fold-search nil)
         (ch-str (char-to-string ch))
         (count (or n 1))
         (pos (save-excursion
                (search-forward ch-str nil t count))))
    (when pos
      (goto-char (1- pos)))))

(defun miao--add-fake-cursor-at-point (pos)
  "Create an overlay to draw a fake cursor as miacro at POS."
  (let ((ov (make-overlay pos (1+ pos) nil t)))
    (overlay-put ov 'face 'miao-miacro-fake-cursor)
    (overlay-put ov 'miao-miacro-type 'cursor)
    ov))

(defun miao--add-overlay-symbol (begin end)
  "Create an overlay to draw a fake cursor as miacro at POS."
  (let ((ov (make-overlay begin end nil t)))
    (overlay-put ov 'face 'miao-miacro-fake-symbol)
    (overlay-put ov 'miao-miacro-type 'cursor)
    (push ov miao--miacro-overlays)))

(defun miao--miacro-check-in-overlay ()
  (let ((pos (point)))
    (catch 'found
      (dolist (ov miao--miacro-overlays)
        (when (and (overlay-start ov)
                   (overlay-end ov)
                   (<= (overlay-start ov) pos)
                   (<= pos (overlay-end ov)))
          (throw 'found ov)))
      nil)))

(defun miao--reset-fake-cursors ()
  (dolist (ov miao--miacro-fake-cursors)
    (delete-overlay ov))
  (setq miao--miacro-fake-cursors nil))

(defun miao-miacro-remove-overlays ()
  (miao--reset-fake-cursors)
  (dolist (ov miao--miacro-overlays)
    (delete-overlay ov))
  (setq miao--miacro-overlays nil))

(defun miao-miacro-ignite ()
  (interactive)
  (if-let* ((ov (miao--miacro-check-in-overlay)))
      (progn
        (setq miao--miacro-selection ov
              miao--miacro-ignite-offset (- (point) (overlay-start ov)))
        (miao--reset-fake-cursors)
        (message "offset %s" miao--miacro-ignite-offset)
        (dolist (line miao--miacro-overlays)
          (let ((fake-cursor (miao--add-fake-cursor-at-point
                              (+ miao--miacro-ignite-offset
                                 (overlay-start line)))))
            (push fake-cursor miao--miacro-fake-cursors)))
        (call-interactively 'kmacro-start-macro))
   (message "Miaocro has to be in a selection")))

(defun miao-miacro-go ()
  (interactive)
  (when defining-kbd-macro
    (kmacro-end-macro nil)
    (when last-kbd-macro
      (message "kbd macro defined")
      (atomic-change-group
        (save-window-excursion
          (save-mark-and-excursion
            (dolist (ov miao--miacro-overlays)
              (unless (eq ov miao--miacro-selection)
                (let ((start (overlay-start ov))
                      (end (overlay-end ov)))
                  (message "apply macro to %s %s %s" ov start end)
                  (when (and start end)
                      (save-restriction
                        (goto-char (+ start miao--miacro-ignite-offset))
                        (narrow-to-region start end)
                        (kmacro-call-macro 1)
                        (widen))))))))
        (miao-miacro-remove-overlays)
        (setq miao--miacro-selection nil)))))

(defun miao-next-region-item (direction)
  (if (region-active-p)
      (let ((length (- (region-end) (region-beginning)))
            (re (concat "\\_<" (regexp-quote (buffer-substring-no-properties (region-beginning) (region-end))) "\\_>")))
        (goto-char (if (> direction 0) (region-end) (region-beginning)))
        (setq next (re-search-forward re nil t direction))
        (set-mark (+ (point) (* length (- direction)))))))

(defun miao-mark-all-symbol ()
  (interactive)
  (miao-miacro-remove-overlays)
  (when-let* ((bounds (bounds-of-thing-at-point 'symbol))
              (begin (car bounds))
              (end (cdr bounds))
              (re (concat "\\_<"
                          (regexp-quote (buffer-substring-no-properties begin end))
                          "\\_>")))
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (miao--add-overlay-symbol (match-beginning 0) (match-end 0)))))))

(defun miao-mark-all-lines ()
  (interactive)
  (miao-miacro-remove-overlays)
  (when (region-active-p)
    (save-excursion
      (let ((start (region-beginning))
            (end (region-end)))
        (deactivate-mark)
        (goto-char start)
        (while (and (<= (point) end)
                    (not (eobp)))
          (let* ((line-start (line-beginning-position))
                 (line-end (line-end-position))
                 (ov (make-overlay line-start line-end)))
            (overlay-put ov 'face 'region)
            (push ov miao--miacro-overlays))
          (forward-line 1))))))


(defun miao-next-symbol-item (direction)
  (if (not (region-active-p))
      (when-let* ((bounds (bounds-of-thing-at-point 'symbol))
                  (begin (car bounds))
                  (end (cdr bounds)))
        (goto-char begin)
        (set-mark end)
        (miao-next-region-item direction))))

(defun miao-next-item ()
  (interactive)
  (if (region-active-p)
      (miao-next-region-item 1)
    (miao-next-symbol-item 1)))

(defun miao-prev-item ()
  (interactive)
  (if (region-active-p)
      (miao-next-region-item -1)
    (miao-next-symbol-item -1)))

(defun miao-setup-modeline ()
  "Setup indicator appending the return of function
`miao-indicator' to the modeline.

This function should be called after you setup other parts of the mode-line
 and will work well for most cases.

If this function is not enough for your requirements,
use `miao-indicator' to get the raw text for indicator
and put it anywhere you want."
  (unless (cl-find '(:eval (miao-indicator)) mode-line-format :test 'equal)
    (setq-default mode-line-format (append '((:eval (miao-indicator))) mode-line-format))))

(provide 'miao-func)
