;;; doc-em.el --- Auto-open related documentation while programming
;;;
;;; Documentation: README.org:Introduction

;; Copyright (C) 2019-2021  Janne Nykopp

;; Author: Janne Nykopp <newcup@iki.fi>
;; Version: 0.6.0
;; Package-requires: ((emacs "25.1"))
;; Keywords: tools docs
;; URL: https://peruna.fi/~newcup/doc-em

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Auto-open and auto-jump to the relevant section of related
;; documentation while programming.

;;; Code:

(require 'cl-lib)
(require 'org)


;;; ----- Code for (re)starting Doc-em mode -----
;;; Documentation: README.org:Starting Doc-em mode

(defvar doc-em--rst-locals nil
  "A list of variables to reset when the mode starts.

List that contains the local variable symbols and the values they
should be reset to when mode is turned off.  This list is
initialized by using macro `doc-em--defvar-local-rst'.")

(defmacro doc-em--defvar-local-rst (name value &optional docstring)
  "Define a local variable, add it to list of variables to reset.

Expands as `defvar-local', but before that inserts the NAME and
VALUE to the `doc-em--rst-locals' list, only if they are not
there already.  Note, this is supposed to be used on top level
only (caveats of `defvar' apply), and during load-time only.
Optional argument DOCSTRING is passed only to the internal
`defvar-local'."
  (cl-pushnew (cons name value) doc-em--rst-locals :key #'car)
  `(defvar-local ,name ,value ,docstring))

(defun doc-em--reset-local-state ()
  "Reset the locals that are in the `doc-em--rst-locals' list."
  (cl-dolist (pair doc-em--rst-locals)
    (set (car pair) (eval (cdr pair)))))

(doc-em--defvar-local-rst doc-em--prev-cmd-point -1
  ;; Documentation: README.org:Jump timer (re)start
  "`point' when previous command was issued.

With this, we avoid initializing or restarting timers in vain in
`doc-em--set-timer-if-moved' - timer is initialized only if
cursor has really moved.  Initialized with -1, so any position is
a new one when starting up.")

(doc-em--defvar-local-rst doc-em--last-doc-show-point -1
  ;; Documentation: README.org:Search and jump to documentation
  "`point' where documentation window was last updated.

This stores where previous `doc-em--search-and-update-doc' was
done.  The purpose is to avoid unnecessary jumping and unnecessary
confirmation questions - i.e. if user moves cursor but returns
back to where it was last time the documentation
updated.  Initialized with -1, so any position is a new one when
starting up.")

(doc-em--defvar-local-rst doc-em--chars-modified-tick (buffer-chars-modified-tick)
  ;; Documentation: README.org:Jump timer (re)start
  "Number of text changes done in the buffer since last command.

Holds a cache of output value from `buffer-chars-modified-tick'
Used in figuring out whether a command edited the buffer. If it
edited, we don't want to jump in documentation.")


;;; ----- Buffer local variables -----

(defvar doc-em--move-timer nil
  ;; Documentation: README.org:Hook function
  "Timer that runs the doc-em document window update.

There is only one timer per Emacs session (i.e. not buffer-local)
since simultaneous different jumps make no sense.")

(defvar doc-em--buffer-at-activation nil
  ;; Documentation: README.org:Hook function
  "Active buffer at the time an input initiated a jump.

The current buffer gets compared to this and jump is cancelled if
user changed buffers in the meanwhile.")

(defvar doc-em--buffer-of-last-jump nil
  ;; Documentation: README.org:Seeking to correct documentation location
  "In which buffer we jumped last time.

Used e.g. for undoing a jump by popping the mark.")


;;; ----- Settings and related code -----

(defmacro doc-em--set-custom (func)
  ;; Documentation: README.org:Settings
  "For `customize': set keybinding to FUNC.

Generates `customize' :set function for FUNC in
`doc-em-mode-map'.  Remove the old keybinding first for the old
keybinding value symbol if one exists."
  `(lambda (sym defs)
     (let ((old-val (and (boundp sym) (eval sym))))
       (custom-set-default sym defs)
       (when (and (boundp 'doc-em-mode-map) (keymapp doc-em-mode-map))
         (when old-val (define-key doc-em-mode-map old-val nil))
         (define-key doc-em-mode-map defs ,func)))))

(progn
  ;; Documentation: README.org:Settings
  (defgroup doc-em nil
    "Auto-open related documentation when programming"
    :prefix "doc-em-"
    :group 'tools)

  (defcustom doc-em-timeout 2
    "How many seconds to wait until updating the doc window."
    :group 'doc-em
    :type 'number)

  (defcustom doc-em-toplevel-separator
    (rx-to-string
     `(sequence
       space "-----" space (zero-or-more (or word punct space)) space "-----"))
    "Regex for top-level documentation separator.
Note, when used by doc-em, it will be automatically prepended
with `comment-start-skip' and and appended by `comment-end-skip'."
    :group 'doc-em
    :type 'string)

  (defcustom doc-em-tag "Documentation: "
    "The string that starts the documentation specification."
    :group 'doc-em
    :type 'string)

  (defcustom doc-em-separator ":"
    "The string that separates file and headline parts."
    :group 'doc-em
    :type 'string)

  (defcustom doc-em-autojump-p t
    "Should doc-em jump automatically."
    :group 'doc-em
    :type 'boolean)

  (defcustom doc-em-confirm-jump-p nil
    "Should doc-em ask interactively whether to jump or not."
    :group 'doc-em
    :type 'boolean)

  (defcustom doc-em-cancel-or-force-jump-keybinding (kbd "C-c _")
    "Keybinding for canceling a pending jump or for explicit jump.

If autojump is on (see `doc-em-autojump-p'), this keybinding
cancels any pending jump. If autojump is off, this keybinding
forces a jump."
    :group 'doc-em
    :type 'key-sequence
    :set (doc-em--set-custom 'doc-em-toggle-jump))

  (defcustom doc-em-pop-document-mark-keybinding (kbd "C-c &")
    "Keybinding for popping a mark in document window."
    :group 'doc-em
    :type 'key-sequence
    :set (doc-em--set-custom 'doc-em-win-pop-mark))

  (defun doc-em--make-keymap ()
    "Make the keymap for doc-em and return it."
    (let ((km (make-sparse-keymap)))
      (define-key km doc-em-cancel-or-force-jump-keybinding 'doc-em-toggle-jump)
      (define-key km doc-em-pop-document-mark-keybinding 'doc-em-win-pop-mark)
      km)))


;;; ----- Code for finding an anchor -----
;;; Documentation: README.org:Find and parse anchor

(defun doc-em--try-parse-comment-at-point ()
  "Try to parse the comment at the current point.

Return nil, if unsuccessful, otherwise cons (file . headline)."
  (let ((line (thing-at-point 'line t)))
    ;; (use rx-to-string as comment-start-skip and end, or doc-em-tag
    ;; etc. will change according to buffer's mode).
    (when (string-match
           (rx-to-string
            `(sequence (regexp ,comment-start-skip) space (eval doc-em-tag)
                       (group-n 1 (+? anything)) (eval doc-em-separator)
                       (group-n 2 (+? anything)) (regexp ,comment-end-skip)))
           line)
      (cons (match-string 1 line) (match-string 2 line)))))

(defun doc-em--comment-at-or-after-cursor-p ()
  "Is there a comment after the cursor?"
  (comment-only-p (point) (line-end-position)))

(defun doc-em--find-prev-toplevel-separator ()
  "Find the closest previous top level separator. Return the
point, or nil if not found."
  (re-search-backward
   (rx-to-string `(sequence bol (0+ space)
                            (regexp ,comment-start-skip)
                            (regexp ,doc-em-toplevel-separator)
                            (regexp ,comment-end-skip)))
   nil t))

(defun doc-em--search-doc-specifier ()
  "Search for the closest enclosing documentation specifier.

Return cons (filename . headline) if found, nil otherwise."
  (save-excursion
    (let (found)
      (cl-symbol-macrolet ((maybe-report-found
                            (when found
                              (message "Doc-em: Line %d: Anchor %s: %s."
                                       (1+ (count-lines (point-min) (point)))
                                       (car found) (cdr found))))
                           (maybe-update-found
                            (when (and (not found) (doc-em--comment-at-or-after-cursor-p))
                              (setf found (doc-em--try-parse-comment-at-point))
                              maybe-report-found)))
        (condition-case nil
            (let (curr-iteration-point)
              (while (not found)
                (setf curr-iteration-point (point))
                ;; look if anchor's at cursor
                maybe-update-found
                ;; the point is always placed at the scope-opening
                ;; character when returning from `backward-up-list',
                ;; so look one character after that
                (forward-char 1)
                maybe-update-found
                ;; look if it's on next line
                (forward-line 1)
                maybe-update-found
                (when (not found)
                  (goto-char curr-iteration-point)
                  (backward-up-list 1 t t))))
          ((user-error scan-error)
           ;; First try to search backwards for top level separators.
           (catch 'toplevel-sep-not-found
             (while (not found)
               (let ((prev-sep (doc-em--find-prev-toplevel-separator)))
                 (unless prev-sep
                   (throw 'toplevel-sep-not-found nil))
                 (forward-line)
                 maybe-update-found
                 (goto-char prev-sep)
                 (forward-line -1))))
           ;; Failing that, try to search from the file's first
           ;; comment lines.
           (goto-char (point-min))
           (while (and (not found) (doc-em--comment-at-or-after-cursor-p))
             (setf found (doc-em--try-parse-comment-at-point))
             maybe-report-found
             (forward-line)))))
      found)))


;;; ----- Code for documentation window handling -----

(defun doc-em--open-doc-window (buf)
  ;; Documentation: README.org:Opening documentation window
  "Open a documentation window for buffer BUF, if not yet open."
  (unless (get-buffer-window buf)
    (let ((win (or (window-in-direction 'right) (window-in-direction 'left)
                   (split-window-right))))
      (set-window-buffer win buf))))


;;; ----- Code for seeking in the documentation file -----
;;; Documentation: README.org:Seeking to correct documentation location

(defun doc-em--move-doc-win (buf pos)
  "Scroll the window displaying BUF to position POS."
  (set-window-point (get-buffer-window buf) pos))

(defun doc-em--org-jump-to-correct-location (buf location &optional dont-push-mark-p)
  "Search for the target LOCATION to jump to in buffer BUF.

If DONT-PUSH-MARK-P is nil (default), push the mark in mark ring.

Works only with `org-mode' documents."
  (with-current-buffer buf
    (let* ((outline-path (ignore-errors (org-get-outline-path t)))
           (curr-heading (car (last outline-path)))
           (orig-point (point)))
      (unless (string-equal curr-heading location)
        (let ((pos (org-find-exact-headline-in-buffer location)))
          (if pos (progn (unless dont-push-mark-p (push-mark orig-point t)
                                 (setf doc-em--buffer-of-last-jump buf))
                         (doc-em--move-doc-win buf pos))
            (message "Doc-em: Heading '%s' was not found" location)))))))

(defun doc-em-win-pop-mark ()
  "Return to the point of jump in the documentation window.

Change cursor back to the point where it was before a jump in the
documentation window where the latest jump was done.  Can also be
used to jump back in previous marks."
  (interactive)
  (when doc-em--buffer-of-last-jump
    (with-current-buffer doc-em--buffer-of-last-jump
      (exchange-point-and-mark)
      (doc-em--move-doc-win doc-em--buffer-of-last-jump (point))
      (pop-mark))))

(defun doc-em--search-and-update-doc ()
  ;; Documentation: README.org:Search and jump to documentation
  "Search for an anchor and jump there in other frame."
  (when (eq (current-buffer) doc-em--buffer-at-activation)
    (let ((p (point)))
      (unless (= p doc-em--last-doc-show-point)
        ;; This code uses newcomment.el heavily, so make sure it's
        ;; properly set up with `comment-normalize-vars'.
        (comment-normalize-vars)
        (let ((fn-and-heading (doc-em--search-doc-specifier)))
          (when fn-and-heading
            (let* ((filename (car fn-and-heading))
                   (heading (cdr fn-and-heading))
                   (buf (find-file-noselect filename)))
              (when (or (not doc-em-confirm-jump-p)
                        (y-or-n-p (format "Doc-em: Jump to %s: %s? " filename heading)))
                (setf doc-em--last-doc-show-point p)
                (doc-em--open-doc-window buf)
                (doc-em--org-jump-to-correct-location buf heading)))))))))


;;; ----- Hook, timers and code related to them -----

(defun doc-em-toggle-jump ()
  ;; Documentation: README.org:Jump timer canceling
  "Abort a scheduled jump, or force a jump when autojump is off."
  (interactive)
  ;; If autojump is on, then the mere activation of this command, as
  ;; it doesn't move cursor, cancels the pending jump.
  (if doc-em-autojump-p
      (message "Doc-em: Upcoming jump cancelled")
    (doc-em--search-and-update-doc)))

(defun doc-em--set-timer-if-moved ()
  ;; Documentation: README.org:Hook function
  "If command moved cursor, (re)set doc-em timer, else cancel timer.

If cursor was moved from previous command, initialize or restart
a timer which will execute a function for doing a jump to the
documentation after `doc-em-timeout' seconds.

If last command didn't move the cursor or modified
buffer (e.g. ‘self-insert-command’, yank), cancel the existing
timer."
  (when doc-em-autojump-p
    (let ((p (point))
          (old-bcm doc-em--chars-modified-tick)
          (bcm (buffer-chars-modified-tick)))
      (setf doc-em--chars-modified-tick (buffer-chars-modified-tick))
      (if (or (= doc-em--prev-cmd-point p)
              (< old-bcm doc-em--chars-modified-tick))
          ;; Buffer was modified or cursor didn't move - cancel timer
          (cancel-timer doc-em--move-timer)
        ;; Cursor moved - set variables, restart timer
        (progn
          ;; Documentation: README.org:Jump timer (re)start
          (setf doc-em--prev-cmd-point p
                doc-em--buffer-at-activation (current-buffer))
          (when doc-em--move-timer
            (cancel-timer doc-em--move-timer))
          (setf doc-em--move-timer
                (run-with-idle-timer
                 doc-em-timeout nil 'doc-em--search-and-update-doc)))))))


;;; ----- Mode definition -----

;;;###autoload
(define-minor-mode doc-em-mode
  ;; Documentation: README.org:Technical description
  "Auto-open related documentation when programming.

When cursor moves in buffer, search for comments with anchors
that define a document and a location inside the document. Open
automatically a window that displays the documentation centered
in the location."
  :init-value nil
  :lighter " DocEm"
  :keymap (doc-em--make-keymap)
  :group 'doc-em
  (if (not doc-em-mode)
      (progn
        (cancel-timer doc-em--move-timer)
        (remove-hook 'post-command-hook 'doc-em--set-timer-if-moved :local))
    (doc-em--reset-local-state)
    (add-hook 'post-command-hook 'doc-em--set-timer-if-moved t :local)))

(provide 'doc-em)

;;; doc-em.el ends here
