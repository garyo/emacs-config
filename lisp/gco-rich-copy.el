;;; gco-rich-copy.el --- Copy Markdown or web selections as rich text -*- lexical-binding: t -*-

;;; Commentary:

;; Put HTML on the macOS pasteboard, with a plain-text fallback, so it
;; pastes into mail and other rich-text apps with links and lists intact.
;;
;; `gco-rich-copy-markdown' renders the region (or buffer) with pandoc.
;; `gco-rich-copy-xwidget-selection' copies the selection in an
;; xwidget-webkit buffer, such as a markdown-xwidget preview.  The Cocoa
;; build never hands the page's selection to the pasteboard itself: M-w
;; there copies the buffer's hidden placeholder text instead.

;;; Code:

(require 'json)

(declare-function xwidget-webkit-current-session "xwidget")
(declare-function xwidget-webkit-execute-script "xwidget")

(defconst gco-rich-copy--pasteboard-js "
ObjC.import('AppKit');
const input = $.NSFileHandle.fileHandleWithStandardInput.readDataToEndOfFile;
const {html, text} = JSON.parse(
  $.NSString.alloc.initWithDataEncoding(input, $.NSUTF8StringEncoding).js);
const pb = $.NSPasteboard.generalPasteboard;
pb.clearContents;
pb.setStringForType(html, $.NSPasteboardTypeHTML);
pb.setStringForType(text, $.NSPasteboardTypeString);"
  "JXA script that reads {html, text} JSON on stdin into the pasteboard.")

(defconst gco-rich-copy--selection-js "
(function () {
  var sel = window.getSelection();
  if (sel.isCollapsed) return null;
  var range = sel.getRangeAt(0);
  var html = range.cloneContents();
  var node = range.commonAncestorContainer;
  if (node.nodeType !== Node.ELEMENT_NODE) node = node.parentNode;
  for (; node && node !== document.body; node = node.parentNode) {
    var wrapper = node.cloneNode(false);
    wrapper.appendChild(html);
    html = wrapper;
  }
  var div = document.createElement('div');
  div.appendChild(html);
  return JSON.stringify({html: div.innerHTML, text: sel.toString()});
})()"
  "JavaScript returning the page selection as {html, text} JSON.
The selected fragment is wrapped in copies of its ancestors, so that
list items stay inside their list and link text inside its link.")

(defun gco-rich-copy-set-clipboard (html text)
  "Put HTML on the system clipboard, with TEXT as its plain-text form."
  (unless (eq window-system 'ns)
    (user-error "Rich-text copy needs the macOS pasteboard"))
  (with-temp-buffer
    (insert (json-serialize (list :html html :text text)))
    (let ((coding-system-for-write 'utf-8))
      (unless (eq 0 (call-process-region
                     (point-min) (point-max) "osascript" t t nil
                     "-l" "JavaScript" "-e" gco-rich-copy--pasteboard-js))
        (error "Setting the clipboard failed: %s"
               (string-trim (buffer-string)))))))

(defun gco-rich-copy--markdown-to-html (markdown)
  "Render MARKDOWN to an HTML fragment with pandoc."
  (with-temp-buffer
    (insert markdown)
    (let ((coding-system-for-read 'utf-8)
          (coding-system-for-write 'utf-8))
      (unless (eq 0 (call-process-region (point-min) (point-max)
                                         "pandoc" t '(t nil) nil "-t" "html"))
        (error "pandoc could not render the Markdown")))
    (buffer-string)))

(defun gco-rich-copy-markdown (beg end)
  "Copy the Markdown between BEG and END as rich text.
Interactively, copy the region if active, else the whole buffer."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (let ((markdown (buffer-substring-no-properties beg end)))
    (gco-rich-copy-set-clipboard (gco-rich-copy--markdown-to-html markdown)
                                 markdown))
  (deactivate-mark)
  (message "Copied %s as rich text"
           (if (and (= beg (point-min)) (= end (point-max))) "buffer" "region")))

(defun gco-rich-copy-xwidget-selection ()
  "Copy the selection in the current xwidget-webkit page as rich text."
  (interactive)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session) gco-rich-copy--selection-js
   (lambda (json)
     (if (not (stringp json))
         (message "Nothing selected")
       (let ((selection (json-parse-string json :object-type 'plist)))
         (gco-rich-copy-set-clipboard (plist-get selection :html)
                                      (plist-get selection :text))
         (message "Copied selection as rich text"))))))

(provide 'gco-rich-copy)
;;; gco-rich-copy.el ends here
