# RuboCop.el

## Synopsis

A simple Emacs interface for [RuboCop](https://github.com/rubocop-hq/rubocop).

## Installation

Please, note that the current version of `RuboCop.el` requires `RuboCop` 0.9.0 or later.

### Manual

Just drop `rubocop.el` somewhere in your `load-path`. I
favour the folder `~/.emacs.d/vendor`:

```lisp
(add-to-list 'load-path "~/.emacs.d/vendor")
(require 'rubocop)
```

### MELPA

If you're an Emacs 24 user or you have a recent version of package.el
you can install rubocop.el from the [MELPA](http://melpa.org/) and
[MELPA Stable](http://stable.melpa.org/) repositories.

## Usage

Command                                         | Description                                             | RuboCop mode binding
------------------------------------------------|---------------------------------------------------------|--------------------
<kbd>M-x rubocop-check-project</kbd>            | Runs RuboCop on the entire project                      | `C-c C-r p`
<kbd>M-x rubocop-check-directory</kbd>          | Prompts from a directory on which to run RuboCop        | `C-c C-r d`
<kbd>M-x rubocop-check-current-file</kbd>       | Runs RuboCop on the currently visited file              | `C-c C-r f`
<kbd>M-x rubocop-autocorrect-project</kbd>      | Runs auto-correct on the entire project                 | `C-c C-r P`
<kbd>M-x rubocop-autocorrect-directory</kbd>    | Prompts for a directory on which to run auto-correct    | `C-c C-r D`
<kbd>M-x rubocop-autocorrect-current-file</kbd> | Runs auto-correct on the currently visited file.        | `C-c C-r F`
<kbd>M-x rubocop-format-project</kbd>           | Runs format on the entire project                       | `C-c C-r X`
<kbd>M-x rubocop-format-directory</kbd>         | Prompts for a directory on which to run format          | `C-c C-r y`
<kbd>M-x rubocop-format-current-file</kbd>      | Runs format on the currently visited file.              | `C-c C-r x`


If you use them often you might want to enable `rubocop-mode` which will added some keybindings for them:

```lisp
(add-hook 'ruby-mode-hook #'rubocop-mode)
```

By default `rubocop-mode` uses the prefix `C-c C-r` for its commands, but you can change this if you wish:

``` emacs-lisp
(setq rubocop-keymap-prefix (kbd "C-c C-x"))
```

## Configuration

There are a couple of configuration variables that you can use to adjust RuboCop.el's behavior.

The variable `rubocop-autocorrect-on-save` controls whether to auto-correct automatically files on save when
`rubocop-mode` is active. It's disabled by default, but you can easily change this:

``` emacs-lisp
(setq rubocop-autocorrect-on-save t)
```

You can change the shell command used by `rubocop-check-*` commands via `rubocop-check-command`:

``` emacs-lisp
;; let's run only lint cops
(setq rubocop-check-command "rubocop --lint --format emacs")
```

You can change the shell command used by `rubocop-autocorrect-*` commands via `rubocop-autocorrect-command`:

``` emacs-lisp
;; let's run all auto-corrections possible (by default it's only the safe ones)
(setq rubocop-autocorrect-command "rubocop -A --format emacs")
```

You can change the shell command used by `rubocop-format-*` commands via `rubocop-format-command`.

## Alternatives

Flycheck and Flymake provide more sophisticated integration with various lint tools, including RuboCop.

## Known issues

Check out the project's
[issue list](https://github.com/rubocop-hq/rubocop-emacs/issues?sort=created&direction=desc&state=open)
a list of unresolved issues. By the way - feel free to fix any of them
and send me a pull request. :-)

## Contributors

Here's a [list](https://github.com/rubocop-hq/rubocop-emacs/contributors) of all the people who have contributed to the
development of RuboCop.el.

## Bugs & Improvements

Bug reports and suggestions for improvements are always
welcome. GitHub pull requests are even better! :-)

Cheers,<br/>
[Bozhidar](http://twitter.com/bbatsov)
