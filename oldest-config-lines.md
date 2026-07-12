# Oldest Surviving Lines in Emacs Config

Lines that have survived verbatim from the earliest git commits, traced through
renames and the 2024 refactor that split the monolithic `init.el` into `lisp/init-*.el` files.

## From the Very First Commit (d6d0ff1, 2012-12-19)

The original `init.el` was 969 lines. ~130+ substantive lines survive verbatim
across 11 current files.

### `init.el` (3 lines)

```elisp
(put 'eval-expression 'disabled nil)    ; orig line 891
(put 'narrow-to-region 'disabled nil)   ; orig line 892
(put 'narrow-to-page 'disabled nil)     ; orig line 898
```

### `lisp/init-languages.el` (~40 lines)

The entire C/C++ mode configuration is essentially untouched:

- `my-semicolon-criteria` function (orig lines 383–387)
- `my-c-mode-hook` function: all `c-set-offset` calls, `fill-column`,
  `c-hanging-semi&comma-criteria`, `c-hanging-braces-alist`,
  `c-cleanup-list` (orig lines 391–436)
- `(add-hook 'c-mode-common-hook 'my-c-mode-hook)` (orig line 438)
- `java-mode-hook` with `compile` keybinding (orig lines 441, 446)
- `auto-mode-alist` entries for `.pl` (cperl), SConstruct (python),
  `.vb`/`.bas` (visual-basic), `.cp` (c++), `.tjp` (taskjuggler),
  `.lua` (orig lines 250–259)

### `lisp/init-settings.el` (~40 lines)

The big `setq-default` block and `custom-set-variables`:

- `backup-by-copying-when-linked t` (orig line 843)
- `font-lock-maximum-decoration t` (orig line 844)
- `delete-old-versions t` (orig line 850)
- `diff-switches "-up"` (orig line 851)
- `enable-recursive-minibuffers t` (orig line 852)
- `fill-column 78` (orig line 854)
- `find-file-existing-other-name t` (orig line 855)
- `inhibit-startup-message t` (orig line 857)
- `initial-scratch-message ""` (orig line 858)
- `kept-old-versions 1` (orig line 861)
- `mark-even-if-inactive t` (orig line 865)
- `mouse-drag-copy-region t` (orig line 866)
- `require-final-newline t` (orig line 869)
- `next-line-add-newlines nil` (orig line 870)
- `scroll-step 1`, `scroll-conservatively 1`, `search-highlight t` (orig lines 871–873)
- `tags-revert-without-query t` (orig line 876)
- `truncate-partial-width-windows nil` (orig line 878)
- `vc-make-backup-files t` (orig line 881)
- `version-control t` (orig line 884)
- `custom-set-variables` entries: `align-to-tab-stop`, `ecb-*`, `egg-*`,
  `ido-auto-merge-delay-time`, `ido-enable-flex-matching`,
  `rng-nxml-auto-validate-flag`, `taskjuggler-command`,
  `vc-dired-recurse` (orig lines 905–961)

### `emacs-orgmode.el` (~35 lines)

The color/bgcolor org-link-type definitions, lambda handlers,
and CSS style export blocks (orig lines 129–184).

### `lisp/init-misc.el` (~25 lines)

- `(global-visual-line-mode 0)` (orig line 84)
- `(blink-cursor-mode -1)` (orig line 204)
- `(global-font-lock-mode 1)` (orig line 206)
- `(setq-default indicate-empty-lines t)` (orig line 231)
- `(require 'uniquify)` and uniquify config (orig lines 234–235)
- `bf-pretty-print-xml-region` — the entire function including
  `(message "Ah, much better!")` (orig lines 366–379)
- `ibuffer-formats` setq block (orig lines 785–790)
- `end-of-buffer-right-way` function (orig lines 348–353)
- `completion-ignored-extensions` removal block (orig lines 837–840)

### `lisp/init-shell.el` (~19 lines)

- `(setq shell-file-name explicit-shell-file-name)` (orig line 78)
- `shell-hilight-face` defface and `my-shell-extra-keywords` (orig lines 215–225)
- `(pcomplete-shell-setup)` (orig line 227)
- `shell-pushd-regexp`, `shell-pushd-dextract`, `shell-pushd-dunique` (orig lines 497–499)
- `explicit-bash-args`, `explicit-zsh-args` (orig lines 510, 512)
- `comint-completion-addsuffix`, `comint-eol-on-send`,
  `comint-input-ignoredups`, `comint-input-ring-size` (orig lines 515–519)
- Eshell mode hook with `(setq cursor-type '(bar . 10))` (orig lines 526, 531)

### `lisp/init-error-checker.el` (~14 lines)

- `process-error-filename` function (orig lines 617–632)
- `fix-win-path` function (orig lines 647–648)
- `compilation-parse-errors-filename-function` and
  `compilation-mode-font-lock-keywords` (orig lines 687–693)

### Other files (1–2 lines each)

- `lisp/init-fonts-and-frames.el`: `(tool-bar-mode 0)` (orig line 203)
- `lisp/init-keybindings.el`: "Move down/up 10 lines" docstrings (orig lines 822, 826)
- `lisp/init-misc-packages.el`: `(recentf-mode t)` (orig line 82)
- `lisp/init-system-open.el`: `open-folder-in-explorer` defun and
  `[f12]` keybinding (orig lines 536, 551)

---

## From Early Commits (2013–2014)

### 92b7424 (2014-02-13) "improve load-path setting"

- `lisp/init-misc.el`: `(winner-mode 1)` with restore-window-config comment
- `lisp/init-settings.el`: `line-number-mode t`, `ecb-layout-window-sizes`,
  `py-python-command`, `safe-local-variable-values`, `speedbar-tag-hierarchy-method`
- `emacs-orgmode.el`: `org-babel-load-languages`, `org-latex-packages-alist`,
  `org-odt-convert-processes`

### 2bfe604 (2014-05-05) "enable melpa, magit config"

- `init.el`: `(put 'set-goal-column 'disabled nil)`

### 3fcb7a7 (2014-07-24) "Simplified with maybe-require"

- `lisp/initial-utils.el`: `(defun maybe-require (feature) ...)`
- `lisp/init-misc.el`: `windmove-default-keybindings` block

### a32c90a (2014-11-21) "Various dotfile updates"

- `lisp/init-options.el`: `(set-charset-priority 'unicode)`,
  `(setq default-process-coding-system '(utf-8-unix . utf-8-unix))`
- `lisp/init-version-control.el`: `(remove-hook 'magit-refs-sections-hook 'magit-insert-tags)`
- `lisp/init-misc.el`: `(global-auto-revert-mode t)`
- `lisp/init-settings.el`: `git-commit-summary-max-length 64`
- `emacs-orgmode.el`: `org-export-with-sub-superscripts`, `org-use-speed-commands`

---

*Generated 2026-03-23 by tracing `git show d6d0ff1:init.el` through the full
commit history and matching against current files.*
