## Synopsis

A simple Emacs interface for [RuboCop](https://github.com/bbatsov/rubocop).

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

### Emacs Prelude

`rubocop.el` is naturally part of
[Emacs Prelude](https://github.com/bbatsov/prelude). If you're a Prelude
user - `rubocop.el` is already properly configured and ready for
action.

## Usage

Command                                         | Description                                             | RuboCop mode binding
------------------------------------------------|---------------------------------------------------------|--------------------
<kbd>M-x rubocop-check-project</kbd>            | Runs RuboCop on the entire project                      | `C-c C-r p`
<kbd>M-x rubocop-check-directory</kbd>          | Prompts from a directory on which to run RuboCop        | `C-c C-r d`
<kbd>M-x rubocop-check-current-file</kbd>       | Runs RuboCop on the currently visited file              | `C-c C-r f`
<kbd>M-x rubocop-autocorrect-project</kbd>      | Runs auto-correct on the entire project                 | `C-c C-r P`
<kbd>M-x rubocop-autocorrect-directory</kbd>    | Prompts for a directory on which to run auto-correct    | `C-c C-r D`
<kbd>M-x rubocop-autocorrect-current-file</kbd> | Runs auto-correct on the currently visited file.        | `C-c C-r F`


If you use them often you might want to enable `rubocop-mode` which will added some keybindings for them:

```lisp
(add-hook 'ruby-mode-hook #'rubocop-mode)
```

## Known issues

Check out the project's
[issue list](https://github.com/bbatsov/rubocop-emacs/issues?sort=created&direction=desc&state=open)
a list of unresolved issues. By the way - feel free to fix any of them
and send me a pull request. :-)

## Contributors

Here's a [list](https://github.com/bbatsov/rubocop-emacs/contributors) of all the people who have contributed to the
development of rubocop.el.

## Bugs & Improvements

Bug reports and suggestions for improvements are always
welcome. GitHub pull requests are even better! :-)

Cheers,<br/>
[Bozhidar](http://twitter.com/bbatsov)
