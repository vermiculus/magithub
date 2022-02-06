# Maintainer's Note

I have not had as much time for extra-curriculars as I once had.
At this point, I will never catch up to the obvious successor of
this package, Forge, nor would I really want to :smile:

Please check out that project instead!  I believe there are things
a GitHub-specific package could do that don't make sense to generalize,
but Forge has served me well for the 95%.

If you have a vision and would like to take over maintainership,
reach out!

---

<a href="screenshots.md"><img align="right" src="https://github.com/vermiculus/magithub/raw/master/images/status.png" width="50%" alt="Overview -- the status buffer"/></a>

# Magithub

[![MELPA Status](http://melpa.milkbox.net/packages/magithub-badge.svg)](http://melpa.milkbox.net/#/magithub)
[![Build Status](https://travis-ci.org/vermiculus/magithub.svg?branch=master)](https://travis-ci.org/vermiculus/magithub)
[![Gitter](https://badges.gitter.im/vermiculus/magithub.svg)](https://gitter.im/vermiculus/magithub)
[![MELPA Stable Status](http://melpa-stable.milkbox.net/packages/magithub-badge.svg)](http://melpa-stable.milkbox.net/#/magithub)
[![GitHub Commits](https://img.shields.io/github/commits-since/vermiculus/magithub/latest.svg)](//github.com/vermiculus/magithub/releases)

Magithub is a collection of interfaces to GitHub integrated into
[Magit][magit] workflows:

- Push new repositories
- Fork existing ones
- List and create issues and pull requests
- Keep offline notes for your eyes only
- Write comments
- Manage labels and assignees
- Stay up-to-date with status checks (e.g., CI) and notifications
- ...

as well as support for working offline.

Happy hacking!

## Quick Start

GitHub rate-limits unauthenticated requests heavily, so Magithub does
not support making such requests.  Consequently, `ghub` must be
authenticated before using Magithub -- [see its README][ghub] for
those instructions.

```elisp
(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/github"))
```

See [the full documentation][magithub-org] for more details.

## Getting Help

See [the FAQ][magithub-org-faq] in the full documentation.  If your
question isn't answered there, [drop by the Gitter
room]((https://gitter.im/vermiculus/magithub)).

## Support

I'm gainfully and happily employed with a company that frowns on
moonlighting, so unfortunately I can't accept any monetary support.
Instead, [please direct any and all support to Magit
itself][magit-donate]!

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
[magithub-org]: https://github.com/vermiculus/magithub/blob/master/magithub.org
[magithub-org-faq]: https://github.com/vermiculus/magithub/blob/master/magithub.org#faq
