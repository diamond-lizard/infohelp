;;; infohelp.el --- Make symbol names link to help
;;
;; Author: Lennart Borgman
;; Created: Tue Jun 13 22:49:39 2006
;; Version: 0.54
;; Last-Updated: Fri Jun 16 01:14:08 2006 (7200 +0200)
;; Keywords: help info
;; Compatibility: Requires Emacs 22 (not yet released)
;;
;; Features that might be required by this library:
;;
;;   `info'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is a quick fix to make functions, variables and faces enclosed
;; by `' clickable links in Info buffers. Clicking those links will
;; show the help for the symbo
;;
;; To use it do
;;
;;     (require 'infohelp)
;;     (infohelp-preload)
;;
;; When you want to have those links in the Info buffer type there
;;
;;     M-x infohelp
;;
;; or just pres the <+>-key.
;;
;; To always use this in Info buffers customize `infohelp-links-in-info'.
;;
;; Follow the links with <RET>. Use <TAB> as usual. Toggle infohelp
;; with <+> in Info buffers.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(defgroup infohelp nil
  "Customization of infohelp."
  :group 'info)

(defface infohelp-link-face
  `((((class color) (background light)) (:foreground "RGB:00/cc/00" :underline t))
    (((class color) (background dark)) (:foreground "green" :underline t)))
  "Face used to highlight attributes that are links."
  :group 'infohelp)

(defun infohelp-show-documentation()
  (interactive)
  (infohelp--show-documentation (point)))

(defun infohelp-show-documentation-mouse()
  (interactive)
  (goto-char (posn-point (event-start last-input-event)))
  (infohelp-show-documentation))

(defun infohelp--show-documentation(pos)
  (let* ((ovl (car (overlays-at pos)))
         (beg (overlay-start ovl))
         (end (overlay-end ovl))
         (name (buffer-substring-no-properties beg end))
         (sym (intern-soft name)))
    (if sym
        (cond ((fboundp sym)
               (describe-function sym))
              ((boundp sym)
               (describe-variable sym))
              ((facep sym)
               (describe-face sym))
              (t
               (message "Not a function, variable or face: %s" name)))
      (message "Not a symbol function, variable or face: %s" name))))

(defvar infohelp-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'infohelp-show-documentation)
    (define-key map [down-mouse-1] 'infohelp-show-documentation-mouse)
    map))

(defvar infohelp-old-keys nil)

(defun infohelp-save-old-keys()
  (unless infohelp-old-keys
    (let (
          (nexts (where-is-internal 'Info-next-reference Info-mode-map))
          (prevs (where-is-internal 'Info-prev-reference Info-mode-map)))
      (setq infohelp-old-keys (cons (cons 'nexts nexts) infohelp-old-keys))
      (setq infohelp-old-keys (cons (cons 'prevs prevs) infohelp-old-keys)))))

(defun infohelp-reset-old-keys()
  ;;(cl-assert infohelp-old-keys)
  (let ((nexts (cdr (assoc 'nexts infohelp-old-keys)))
        (prevs (cdr (assoc 'prevs infohelp-old-keys))))
    (setq infohelp-old-keys nil)
    (dolist (n nexts)
      (define-key Info-mode-map n 'Info-next-reference))
    (dolist (p prevs)
      (define-key Info-mode-map p 'Info-prev-reference))))

(defun infohelp-preload()
  (require 'info)
  (infohelp-save-old-keys)
  (define-key Info-mode-map [?+] 'infohelp))

(defun infohelp-set-keys()
  (infohelp-preload)
  (let ((nexts (cdr (assoc 'nexts infohelp-old-keys)))
        (prevs (cdr (assoc 'prevs infohelp-old-keys))))
    (dolist (n nexts)
      (define-key Info-mode-map n 'infohelp-Info-next-reference))
    (dolist (p prevs)
      (define-key Info-mode-map p 'infohelp-Info-prev-reference))))

(defun infohelp-next-ovl()
  (let ((start (point))
        res)
    (goto-char (next-overlay-change (point)))
    (while (and (not (eobp))
                (not (get-char-property (point) 'infohelp)))
      (goto-char (next-overlay-change (point))))
    (when (get-char-property (point) 'infohelp)
      (setq res (point)))
    (goto-char start)
    res))

(defun infohelp-prev-ovl()
  (let ((start (point))
        res)
    (goto-char (previous-overlay-change (point)))
    (while (and (not (bobp))
                (not (get-char-property (point) 'infohelp)))
      (goto-char (previous-overlay-change (point))))
    (when (get-char-property (point) 'infohelp)
      (setq res (point)))
    (goto-char start)
    res))

(defun infohelp-Info-next-reference (&optional recur)
  (interactive)
  (let ((start (point))
        ep
        help-ep
        help-pos)
    (setq help-pos (infohelp-next-ovl))
    (setq help-ep help-pos)
    (condition-case err
        (Info-next-reference recur)
      (error
       (message "%s" (error-message-string err))))
    (setq ep (point))
    (unless (>= (point) start)
      (setq ep (+ (point) (buffer-size)))
      (save-excursion
        (goto-char start)
        (setq help-pos (infohelp-next-ovl))
        (setq help-ep help-pos)
        (unless help-pos
          (goto-char (point-min))
          (setq help-pos (infohelp-next-ovl))
          (when help-pos
            (setq help-ep (+ help-pos (buffer-size)))))))
    (when (and help-pos
               (or (= ep start)
                   (> ep help-ep)))
      (goto-char help-pos))))

(defun infohelp-Info-prev-reference (&optional recur)
  (interactive)
  (let ((start (point))
        ep
        help-ep
        help-pos)
    (setq help-pos (infohelp-prev-ovl))
    (setq help-ep help-pos)
    (condition-case err
        (Info-prev-reference recur)
      (error
       (message "%s" (error-message-string err))))
    (setq ep (point))
    (unless (<= (point) start)
      (setq ep (- (point) (buffer-size)))
      (save-excursion
        (goto-char start)
        (setq help-pos (infohelp-prev-ovl))
        (setq help-ep help-pos)
        (unless help-pos
          (goto-char (point-max))
          (setq help-pos (infohelp-prev-ovl))
          (when help-pos
            (setq help-ep (- help-pos (buffer-size)))))))
    (when (and help-pos
               (or (= ep start)
                   (< ep help-ep)))
      (goto-char help-pos))))


(defvar infohelp-idle-timer nil)
(make-variable-buffer-local 'infohelp-idle-timer)

(defun infohelp-add-links-to-buffer(buffer)
  (interactive)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (infohelp-add-links (point-min) (point-max) (buffer-size)))
    (cancel-timer infohelp-idle-timer)
    (setq infohelp-idle-timer nil)))

(defun infohelp-add-links-to-buffer-when-idle()
  (infohelp-add-links-when-idle (point-min) (point-max) 0))

(defun infohelp-add-links-when-idle(beg end len)
  (when (and (null infohelp-idle-timer)
             (= beg (point-min))
             (= end (point-max)))
    ;;(message "will add links later...")(sit-for 1)
    (setq infohelp-idle-timer (run-with-idle-timer 0.1 nil 'infohelp-add-links-to-buffer (current-buffer)))))

(defun infohelp-add-links(beg end len)
  ;;(message "add-links %s %s %s" beg end len)(sit-for 2)
  (when (> len 0)
    (infohelp-remove-links beg end))
  (save-excursion
    (save-match-data
      (unless beg (setq beg (point-min)))
      (unless end (setq end (point-max)))
      (goto-char beg)
      ;; Note: no need to match all possible symbol names here:
      (let* ((sym-patt "\\([a-zA-Z0-9-+=*/]+\\)")
             (re (concat "\\(?:"
                         "[^`]\`" sym-patt "\'"
                         "\\|"
                         "\\(?:(\\|M-x\\s-+\\)" sym-patt "[[:space:]']"
                         "\\)"))
             )
        (while (re-search-forward re end t)
          (let* ((mb1 (match-beginning 1))
                 (me1 (match-end 1))
                 (mb2 (match-beginning 2))
                 (me2 (match-end 2))
                 (beg (if mb1
                          mb1
                        mb2))
                 (end (if me1
                          me1
                        me2))
                 (name (buffer-substring-no-properties beg end))
                 (sym (intern-soft name))
                 ovl
                 ovls (overlays-at beg))
            (when (and (not (overlays-at (point)))
                       sym
                       (or (fboundp sym)
                           (boundp sym)
                           (facep sym)))
              (dolist (o ovls)
                (when (overlay-get o 'infohelp)
                  (setq ovl o)))
              (unless ovl
                (setq ovl (make-overlay beg end nil t))
                (overlay-put ovl 'face 'infohelp-link-face)
                (overlay-put ovl 'mouse-face 'highlight)
                (overlay-put ovl 'infohelp t)
                (overlay-put ovl 'keymap infohelp-keymap)
                ))))))))

(defun infohelp-remove-links-from-buffer()
  (interactive)
  (save-restriction
    (widen)
    (infohelp-remove-links (point-min) (point-max))))

(defun infohelp-remove-links(beg end)
  (save-excursion
    (remove-overlays beg end 'infohelp t)))



(defun infohelp-setup()
  (cl-assert (eq major-mode 'Info-mode))
  (make-local-variable 'after-change-functions)
  (add-hook 'after-change-functions 'infohelp-add-links-when-idle)
  (infohelp-add-links-to-buffer-when-idle))
(defun infohelp-teardown()
  (cl-assert (eq major-mode 'Info-mode))
  (make-local-variable 'after-change-functions)
  (remove-hook 'after-change-functions 'infohelp-add-links-when-idle)
  (infohelp-remove-links-from-buffer))

(defun infohelp-after-change-major-mode()
  (when (eq major-mode 'Info-mode)
    (infohelp-setup)))

(defcustom infohelp nil
  "If non-nil add symbol help links to Info-mode buffers."
  :group 'infohelp
  :type 'boolean
  :set (lambda(symbol value)
         (set-default symbol value)
         (if value
             (progn
               (dolist (b (buffer-list))
                 (with-current-buffer b
                   (when (eq major-mode 'Info-mode)
                     (infohelp-setup))))
               (add-hook 'after-change-major-mode-hook 'infohelp-after-change-major-mode)
               (infohelp-set-keys))
           (dolist (b (buffer-list))
             (with-current-buffer b
               (when (eq major-mode 'Info-mode)
                 (infohelp-teardown))))
           (remove-hook 'after-change-major-mode-hook 'infohelp-after-change-major-mode)
           (infohelp-reset-old-keys))))

(defun infohelp()
  "Toggle `infohelp-links-in-info' temporary."
  (interactive)
  (customize-set-variable 'infohelp (not infohelp))
  (if infohelp
      (message "Turned on infohelp")
    (message "Turned off infohelp")))

(provide 'infohelp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; infohelp.el ends here
