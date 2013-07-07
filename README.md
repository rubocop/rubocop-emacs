# Synopsis

A simple Emacs interface for [RuboCop](https://github.com/bbatsov/rubocop).

# Installation

Please, note that the current version of `RuboCop.el` requires `RuboCop` 0.9.0 or later.

## Manual

Just drop `rubocop.el` and `dash.el`. somewhere in your `load-path`. I
favour the folder `~/.emacs.d/vendor`:

```lisp
(add-to-list 'load-path "~/emacs.d/vendor")
(require 'rubocop)
```

## Marmalade

If you're an Emacs 24 user or you have a recent version of package.el
you can install rubocop.el from the [Marmalade](http://marmalade-repo.org/) repository.

## MELPA

If you're an Emacs 24 user or you have a recent version of package.el
you can install rubocop.el from the [MELPA](http://melpa.milkbox.net/) repository.

## Emacs Prelude

`rubocop.el` is naturally part of the
[Emacs Prelude](https://github.com/bbatsov/prelude). If you're a Prelude
user - `rubocop.el` is already properly configured and ready for
action.

# Usage

<kbd>M-x rubocop-run-on-project</kbd>

<kbd>M-x rubocop-run-on-directory</kbd>

<kbd>M-x rubocop-run-on-current-file</kbd>

# Known issues

Check out the project's
[issue list](https://github.com/bbatsov/rubocop-emacs/issues?sort=created&direction=desc&state=open)
a list of unresolved issues. By the way - feel free to fix any of them
and send me a pull request. :-)

# Contributors

Here's a [list](https://github.com/bbatsov/rubocop-emacs/contributors) of all the people who have contributed to the
development of rubocop.el.

# Bugs & Improvements

Bug reports and suggestions for improvements are always
welcome. GitHub pull requests are even better! :-)

Cheers,<br/>
[Bozhidar](http://twitter.com/bbatsov)
