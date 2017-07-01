[[!meta title="What is Code Review?"]]
[[!meta author="Tyler Cipriani"]]
[[!meta date="2016-09-15T16:37:22-07:00"]]
[[!meta copyright="""
Copyright &copy; 2017 Tyler Cipriani
"""]]
[[!meta license="""
[[Creative Commons Attribution-ShareAlike License|https://creativecommons.org/licenses/by-sa/4.0/]]
"""]]
[[!tag software rambling]]

A jargon-less definition of code review is a difficult thing to pin-
down. Code review is what happens when someone submits some code to a
software project and a different person, who has knowledge about that
project, checks to see if there are any problems in the submitted code
before making the submitted code a part of the project.

Code review is based around the simple idea that with enough eyes all
bugs are shallow.

> Given a large enough beta-tester and co-developer base, almost every
> problem will be characterized quickly and the fix obvious to someone.
>
> --  Linus's Law, ESR, [The Cathedral and the Bazaar](http://www.catb.org/~esr/writings/cathedral-bazaar/cathedral-bazaar/ar01s04.html)

This is all part of a **collaborative model** of software development. A
model in which anyone can submit code or feedback to an open project.
This model is the engine that drives the development of some of the
world's largest and most successful software projects. It's an accepted
Good Thing™. It is tested. It works. As a result, almost every software
project uses code review for almost every code change—big or small.

Code review is meant to ensure that only high-quality code can be merged
into a project. Code review may be used for many other purposes, some
good and some bad.

Review as a pedagogical tool
----------------------------

Code review at its best is a Socratic teaching style.

Many software projects are large, tangled, and, sometimes, confusing. A
sweeping change can create regressions, or be disruptive rather than
helpful. Software functions as an interconnected system, and to learn to
work within that system you must learn how that system came into being.

> A complex system that works is invariably found to have evolved from a
> simple system that worked. A complex system designed from scratch
> never works and cannot be patched up to make it work. You have to
> start over with a working simple system.
>
> -- Gall's Law, John Gall, Systemantics: How Systems Really Work and How They Fail

Through code review a newcomer learns about the design of the project—to
work with the project's design rather than against it. And by working
with the design of a system, to work more effectively within it.

When review is done well, the person who's code is being reviewed may
learn about software design, general debugging, the management of open
projects, the underlying language, or the underlying language's
implementation.

Review as culture
-----------------

For better or worse, every project has a culture.

The culture that is conveyed through review may be completely innocuous.
The propensity of a particular project to favor third-party libraries
versus maintaining their own implementations in the project. How much
trust a particular project puts in third-party entities (Jira, Github,
Gitlab, etc.). The tools and best-practices for a project also often
become obvious during code review. For instance, whether a project
accepts pull requests via Github [or not](https://github.com/torvalds/linux/pull/17).

Meaningful cultural norms are also conveyed via code review. A project
may have a culture that is generous and helpful, or it may be openly
hostile to outside contribution. Ultimately, the person reviewing code
has power over the person submitting code. This power dynamic can become
even larger when the person submitting code is a member of a
marginalized group in the society of the person reviewing the code.

The person who is reviewing (hopefully) has some sense of the power
imbalance that is inherent in the code review process. It is often
dishearteningly uncomfortable to give code review—particularly when you
disagree with some of the code's implementation. Code reviewers should
(and often do) work hard to offer constructive feedback and to avoid
hostile interactions.

Code review, from the perspective of the person who submits code, is a
situation where you're interacting with a person who has no real
incentive to be nice. Code review is a situation that can tell you a lot
about a person.

Mindful review
--------------

I'm far from a master when it comes to code review.

I've done my share of review, and I've had my share of reviews—both good
and bad. These are the things I try to remain mindful of during the
review process—both as patch-submitter and as a reviewer.

* It takes time

    It takes a LOT of time. I spend so much time on each review because I
    want to offer helpful and meaningful feedback. I could easily spend my
    entire working day reviewing code (and sometimes, I do).

    I've seen rules and guidelines for how much time you should take for
    each SLOC in a review, but I think that's meaningless. If I haven't
    reasonably considered the consequences of a change—I'm not done
    reviewing the code.

    When you submit a patch, you should be mindful that review takes time.
    If this is a one-line change that you want rubber-stamped, then you
    should be explicit about that. Honestly, I'm pretty OK rubber-stamping
    the work of peers that I trust; however, asking for code review on that
    one line means that I will spend at least 15 minutes mentally shifting
    gears, examining, running, and otherwise poking-at the code.

    There is no better way to exhaust the patience and goodwill of a
    software project than to continually submit obviously untested changes.
    Have you run the code? If not, the code is not ready for review.

* If everything is OK, then everything is OK

    Sometimes, I can't find anything wrong with a large patch that was just
    submitted. If that's the case, I try to not to penalize that patch—I
    don't go back through the patch and **really** scrutinize the method
    names, I don't suggest libraries that do the same thing but in a way I
    slightly prefer, I don't enumerate the approaches that could have been
    taken. I just post, "+1 lgtm" and that's all I have to show for the hour
    of work I just put in reviewing, but I think that's OK.

    I feel like this is part of project culture. If a person you don't know
    submits a good patch, that should be OK.

* Say something nice

    This is probably my own weird insecurity. For each review, I go through
    and make sure to leave comments about some of the things I like in the
    patch. I know that whomever I am reviewing is my intellectual equal (or
    often intellectual better), and that they've just put a lot of time into
    this patch. I also know that when I've put a lot of time into something,
    and all of its redeeming qualities are ignored it stinks, and sometimes
    it fucks up my day.

    I try not to fuck up anyone's day.

* Nitpicks and show-stoppers

    When I review things, I try to be explicit if I can—"Fix this problem,
    and I'll merge the patch." There are times I may feel strongly that
    a piece of code can be improved, but not strongly enough to reject the
    patch—"Nitpick: could use a comment here". I'll reply overall with,
    "Some nitpicks inline, but overall lgtm" and then ask in IRC if they're
    fine with the patch merging as-is.

    The difference between a nitpick and a show-stopper is important when
    there are both types of problems in one patch. How do I know what
    **really** needs fixing versus (what really amounts to) the way the
    reviewer would have written the same patch?

A blatant disregard for best practice, A.K.A. JFDI.
---------------------------------------------------

Code review is slow (by design) and hard to get right. Also (CAUTION:
controversy ahead), some changes don't **need** review. That is, they're
not important to the overall code function, and can be corrected easily.
Adding a section to a README, or adding to the docs: JFDI. If there's a
spelling error, well, someone will fix it.

When you have to deploy a hot-fix to production because production is
**completely broken**: JFDI.

I don't like being a roadblock for things like this. There are many
code-review systems that force you to gate changes like this.
Ultimately, I think this creates something akin to alarm-fatigue but for
code review. It hurts everyone involved, it hurts the process, and could
cause some resentment.

Conclusions
-----------

I suppose the **tl;dr** here is: Code review is good when its done well,
and bad when it's done poorly. Earth shaking stuff, believe me—I know.

My big, bold, and important message about code review is—as with all
processes that involve people—staying mindful that everyone involved is,
in fact, a person is paramount to the process's success.
