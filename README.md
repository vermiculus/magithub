<a href="screenshots.md"><img align="right" src="https://github.com/vermiculus/magithub/raw/master/images/status.png" width="50%" alt="Overview -- the status buffer"/></a>

# Magithub

[![MELPA Status](http://melpa.milkbox.net/packages/magithub-badge.svg)](http://melpa.milkbox.net/#/magithub)
[![Build Status](https://travis-ci.org/vermiculus/magithub.svg?branch=master)](https://travis-ci.org/vermiculus/magithub)
[![Gitter](https://badges.gitter.im/vermiculus/magithub.svg)](https://gitter.im/vermiculus/magithub)
[![MELPA Stable Status](http://melpa-stable.milkbox.net/packages/magithub-badge.svg)](http://melpa-stable.milkbox.net/#/magithub)
[![GitHub Commits](https://img.shields.io/github/commits-since/vermiculus/magithub/0.1.2.svg)](//github.com/vermiculus/magithub/releases)

Magithub is a collection of interfaces to GitHub.

Integrated into [Magit][magit] workflows, Magithub allows easy GitHub
repository management.  Supported actions from the status buffer
include:

 - `H H` open the current repo in the browser
 - `H c` push brand-new local repositories up to GitHub
 - `H f` create forks of existing repositories
 - `H p` submit pull requests upstream
 - `H i` create issues
 - `L` on an issue (or pull request) updates its labels
 - `RET` on an issue to open that issue in GitHub
 - `RET` on the CI header to open your CI dashboard

For when you're on the run, you can set `magithub-cache` to `t` to
activate 'offline mode' (or use `H O` from the status buffer).  This
will inhibit all API requests and instead rely on cached data.

Happy hacking!

## Installation

The package can be installed from MELPA.  Otherwise, simply place
`magithub.el` in your `load-path` and `(require 'magithub)`.  Use the
function `magithub-feature-autoinject` to add full Magit workflow
integration.

If you use [use-package][gh-use-package], you should instead use:

```elisp
(use-package magithub
  :after magit
  :config (magithub-feature-autoinject t))
```

To authenticate `ghub`, [see its README][ghub].  However, since its
authentication approach is still in flux, you might consider setting
the following variables appropriately until it's stable:
```elisp
ghub-base-url     ;; base API url; customize if GitHub Enterprise
ghub-username     ;; your username
ghub-token        ;; your personal access token
```
See [GitHub's settings][token] for information on how to create tokens.

For some advanced trickery features, Magithub still requires the `hub`
utility to work -- so before trying to use those features, follow the
installation instructions at [hub.github.com][hub].  To force `hub` to
authenticate, you can use `hub browse` in a terminal (inside a GitHub
repo).

## Support

I'm gainfully and happily employed with a company that frowns on
moonlighting, so unfortunately I can't accept any donations myself.
Instead, [please direct any and all support to Magit itself][magit-donate]!

## Note

There used to be another `magithub`: [nex3/magithub][old-magithub].
It's long-since unsupported and apparently has many issues
(see [nex3/magithub#11][old-magithub-11]
and [nex3/magithub#13][old-magithub-13]) and
was [removed from MELPA][melpa-1126] some years ago.  If you have it
installed or configured, you may wish to remove/archive that
configuration to avoid name-clash issues.  Given that the package has
been defunct for over three years and is likely abandoned, the present
package's name will not be changing.

[magit]: //www.github.com/magit/magit
[magit-donate]: https://magit.vc/donate
[ghub]: //github.com/tarsius/ghub
[hub]: //hub.github.com
[token]: https://github.com/settings/tokens
[gh-use-package]: //github.com/jwiegley/use-package
[old-magithub]: //github.com/nex3/magithub
[old-magithub-11]: //github.com/nex3/magithub/issues/11
[old-magithub-13]: //github.com/nex3/magithub/issues/13
[melpa-1126]: //github.com/melpa/melpa/issues/1126
