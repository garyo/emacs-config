;;; gco-xwidget-search.el --- Incremental search in Cocoa xwidget-webkit -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs implements the WebKit search primitives (`xwidget-webkit-search',
;; `xwidget-webkit-next-result', `xwidget-webkit-previous-result' and
;; `xwidget-webkit-finish-search') only for the GTK build.  On the Cocoa
;; build they are no-ops, so `xwidget-webkit-isearch-mode' (C-s in an
;; xwidget-webkit buffer) prompts for text but never finds anything.
;;
;; This overrides those primitives with the DOM's `window.find', which
;; selects and scrolls to each match.  The stock isearch key flow is left
;; untouched: C-s/C-r start and step, DEL erases, RET/C-g exit.

;;; Code:

(require 'xwidget)

(defconst gco-xwidget-search--js "
(function (query, caseSensitive, backwards, wrap, restart) {
  var sel = window.getSelection();
  if (!query) { sel.removeAllRanges(); return true; }
  if (restart && sel.rangeCount) {
    var r = sel.getRangeAt(0);
    if (backwards) sel.collapse(r.endContainer, r.endOffset);
    else sel.collapse(r.startContainer, r.startOffset);
  }
  return window.find(query, caseSensitive, backwards, wrap, false, false, false);
})(%s, %s, %s, %s, %s)"
  "JavaScript running one search step.
`window.find' searches onward from the current selection, so RESTART
collapses the selection back to the edge of the current match first.
That keeps the match in place while the query is being extended.")

(defun gco-xwidget-search--js-bool (value)
  (if value "true" "false"))

(defun gco-xwidget-search--report (query found)
  "Flag the echo-area prompt when QUERY was not FOUND."
  (unless (or found (string-empty-p query))
    (let ((message-log-max nil))
      (message "%s"
               (concat (propertize "Search contents: " 'face 'minibuffer-prompt)
                       query
                       (propertize "  [no match]" 'face 'error))))))

(defun gco-xwidget-search--run (xw query case-insensitive backwards wrap restart)
  "Run one search step for QUERY in XW.
CASE-INSENSITIVE, BACKWARDS and WRAP mirror `xwidget-webkit-search'.
RESTART re-anchors at the current match instead of moving past it."
  (xwidget-webkit-execute-script
   xw
   (format gco-xwidget-search--js
           (json-serialize query)
           (gco-xwidget-search--js-bool (not case-insensitive))
           (gco-xwidget-search--js-bool backwards)
           (gco-xwidget-search--js-bool wrap)
           (gco-xwidget-search--js-bool restart))
   (lambda (found) (gco-xwidget-search--report query found))))

(defun gco-xwidget-search--search (query xw &optional case-insensitive backwards wrap)
  "Replacement for `xwidget-webkit-search'."
  (xwidget-put xw 'gco-search (list query case-insensitive wrap))
  (gco-xwidget-search--run xw query case-insensitive backwards wrap t))

(defun gco-xwidget-search--step (xw backwards)
  "Move XW to the next match, or the previous one when BACKWARDS."
  (pcase (xwidget-get xw 'gco-search)
    (`(,query ,case-insensitive ,wrap)
     (gco-xwidget-search--run xw query case-insensitive backwards wrap nil))
    (_ (error "Widget has no ongoing search operation"))))

(defun gco-xwidget-search--next (xw)
  "Replacement for `xwidget-webkit-next-result'."
  (gco-xwidget-search--step xw nil))

(defun gco-xwidget-search--previous (xw)
  "Replacement for `xwidget-webkit-previous-result'."
  (gco-xwidget-search--step xw t))

(defun gco-xwidget-search--finish (xw)
  "Replacement for `xwidget-webkit-finish-search'.
The selection stays on the last match, like point after isearch."
  (xwidget-put xw 'gco-search nil))

(when (and (featurep 'xwidget-internal) (featurep 'ns))
  (advice-add 'xwidget-webkit-search :override #'gco-xwidget-search--search)
  (advice-add 'xwidget-webkit-next-result :override #'gco-xwidget-search--next)
  (advice-add 'xwidget-webkit-previous-result :override #'gco-xwidget-search--previous)
  (advice-add 'xwidget-webkit-finish-search :override #'gco-xwidget-search--finish))

(provide 'gco-xwidget-search)
;;; gco-xwidget-search.el ends here
