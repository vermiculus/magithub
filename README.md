# Magithub

[![Melpa Status](http://melpa.milkbox.net/packages/magithub-badge.svg)](http://melpa.milkbox.net/#/magithub)
[![Melpa Stable Status](http://melpa-stable.milkbox.net/packages/magithub-badge.svg)](http://melpa-stable.milkbox.net/#/magithub)

Magit interfaces for GitHub.

## Installation

The package can be installed from Melpa.  Otherwise, simply place
`magithub.el` in your `load-path` and `(require 'magithub)`.

If you use [use-package][gh-use-package], you should instead alter
your `magit` form to `(use-package magithub)`:

```elisp
(use-package magit
  :config (use-package magithub))
```

For now, Magithub requires the `hub` utility to work -- before trying
to use Magithub, follow the installation instructions
at [hub.github.com][hub].  To force `hub` to authenticate, you can use
`hub browse` in a terminal (inside a GitHub repo).

## Screenshots

![Dispatch](images/scr1.png)|![Creating](images/scr2.png)
:-------------------------:|:-------------------------:
![Forking](images/scr3.png)|![Pushing](images/scr4.png)

[hub]: //hub.github.com
[gh-use-package]: //github.com/jwiegley/use-package
