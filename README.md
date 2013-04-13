litable
=======

On-the-fly evaluation/substitution of emacs lisp code

Inspired by Light Table similar feature.

You should only use this with "pure functions", that is functions that do not touch the buffers, file systems, open network connections or any such thing. This is becuase litable will evaluate the form under the point repeatedly and this can lead to very unfortunate accidents (especially if you try eval `shell command "rm -rf"` ;). We'll try to implement some sort of safeguard system in the future.

To start this up, simply enable the litable minor mode in the buffer by calling `M-x litable-mode`.

Not ment to be used in production yet, be warned!

In action
=======

1. Presentation of the basic *prototype*: http://www.youtube.com/watch?v=TgHvRcbYJ-8 [2:32] \(you don't have to watch this\)
2. New features, less slow awkward typing: https://www.youtube.com/watch?v=mNO-vgq3Avg [1:50]

Contribute
=======

* If you feel like contributing, there are **TODO** annotations in the code. Mostly basic/trivial stuff, good exercise for people starting with elisp.
* If you have more substantial ideas, start an issue so we can discuss it. I'm open to all ideas, this is simply a precaution for people to not work on the same feature.
* If you want, you can [throw a couple bucks my way](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=CEYP5YVHDRX8C) \(we have a long way to beat that $300k goal people!\).

*note: the link is for [smartparens](https://github.com/Fuco1/smartparens) donations, but don't worry, I'm the same guy ;)*
