litable [![Paypal logo](https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=TAWNECQR3TTUY)
=======

On-the-fly evaluation/substitution of emacs lisp code

Inspired by Light Table's similar feature.

To start this up, simply enable the litable minor mode in the buffer by calling `M-x litable-mode`.

Litable keeps a list of pure functions as a safeguard for unwanted evaluations. A function must first be accepted into this list (using `M-x litable-accept-as-pure`) before it can be evaluated on-the-fly. You should take care of what function you accept as pure to avoid any unfortunate accidents. Also, note that the pure functions list persists across sessions.

Not meant to be used in production yet, be warned!

In action
=======

1. Presentation of the basic *prototype*: http://www.youtube.com/watch?v=TgHvRcbYJ-8 [2:32] \(you don't have to watch this\)
2. New features, less slow awkward typing: https://www.youtube.com/watch?v=mNO-vgq3Avg [1:50]

Contribute
=======

* If you feel like contributing, there are **TODO** annotations in the code. Mostly basic/trivial stuff, good exercise for people starting with elisp.
* If you have more substantial ideas, start an issue so we can discuss it. I'm open to all ideas, this is simply a precaution for people to not work on the same feature.
* If you want, you can [throw a couple bucks my way](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=TAWNECQR3TTUY) \(we have a long way to beat that $300k goal people!\).
