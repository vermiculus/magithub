# Magithub

Magit interfaces for [the `hub` git extension](hub.github.com).

## Installation

The package can be installed from Melpa.  Otherwise, simply place
`magithub.el` in your `load-path` and `(require 'magithub)`.

If you use [use-package][gh-use-package], you should instead alter
your `magit` form to `(require 'magithub)`:

    (use-package magit
      :config (require 'magithub))

## Screenshots

![Dispatch](images/scr1.png)

![Creating](images/scr2.png)

![Forking](images/scr3.png)

![Pushing](images/scr4.png)

[gh-use-package]: https://github.com/jwiegley/use-package
