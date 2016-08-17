I recently contributed a fix to the
[bootstrap framework](https://github.com/twbs/bootstrap/issues/9855)
that detects on-screen scrollbars to determine whether or not the body
should be padded when a modal window is opened to prevent shifting of
background contents. Detecting on-screen scrollbars turned out to be a bit
more involved than I initially anticipated.

The tl;dr, semi-näive version:

[[!pygments lexer=javascript linenos=yes content="""
var hasScrollbar = window.innerWidth > document.documentElement.clientWidth
"""]]

This works for most browsers. Basically it checks to see if the width of
the `window` element (read: including scrollbars) is greater than the width
 of the root element of the page (read: without scrollbars). If the width
of the page with scrollbars is greater than the width of a page without
scrollbars it would stand to reason that the extra width _is_ a scrollbar.

This solution behaves correctly when IE10+ has
`@-ms-viewport { width: device-width; }` set (as it is in the bootstrap
framework), which seems to result in scrollbars being auto-hidden.
This solution also works for Chrome on the Mac where the scrollbars
are automagically hidden.

This certainly seems to function as expected for IE9+; however,
[IE8 is our newest anchor browser](http://www.paulirish.com/2011/browser-market-pollution-iex-is-the-new-ie6/)
so IE8 should be addressed in any ostensibly &#8220;cross-browser&#8221; approaches.

`window.innerWidth` doesn&#8217;t exist on IE8. Any workarounds you see utilizing
`document.documentElement` will not include scrollbars in the reported
width, so `document.docutmentElement` will not be an adequate substitute
in < IE9.

One thing to check is the `scrollHeight`. If the `scrollHeight` of the root
element is greater than the `clientHeight` of the root element, then that
root element is going to need to scroll to show the overflowing content:

[[!pygments lexer=javascript linenos=yes content="""
var hasScrollbar

if (typeof window.innerWidth === 'number')
  hasScrollbar = window.innerWidth > document.documentElement.clientWidth

hasScrollbar = hasScrollbar ||
  document.documentElement.scrollHeight > document.documentElement.clientHeight
"""]]

Again, this is an oversimplification. The `overflow` property of the root
element can modify the appearance of scrollbars (to create on-screen _faux_ llbars).
Of course, once again, IE and modern browsers differ about how they&#8217;ve
implemented the javascript api for accessing element styles. We can account
for this difference and grab the overflow property like this:

[[!pygments lexer=javascript linenos=yes content="""
var overflowStyle

if (typeof document.documentElement.currentStyle !== 'undefined')
  overflowStyle = document.documentElement.currentStyle.overflow

overflowStyle = overflowStyle || window.getComputedStyle(document.documentElement, '').overflow
"""]]

The two values of the `overflow` or `overflow-y` properties that will
create scrollbars are `visible` and `auto` provided that the `scrollHeight`
is greater than the `clientHeight`. A value of `scroll` for the `overflow`
or `overflow-y` properties will always cause a scrollbar.

This is, once again, a bit of a simplification.

In quirksmode in IE8 `document.documentElement.clientHeight` is 0. The root
element is `document.body`. This won&#8217;t affect most people reading this, but
just to be on the safe side let&#8217;s add it into our solution.

The final solution looks like this:

[[!pygments lexer=javascript linenos=yes content="""
var hasScrollbar = function() {
  // The Modern solution
  if (typeof window.innerWidth === 'number')
    return window.innerWidth > document.documentElement.clientWidth

  // rootElem for quirksmode
  var rootElem = document.documentElement || document.body

  // Check overflow style property on body for fauxscrollbars
  var overflowStyle

  if (typeof rootElem.currentStyle !== 'undefined')
    overflowStyle = rootElem.currentStyle.overflow

  overflowStyle = overflowStyle || window.getComputedStyle(rootElem, '').overflow

    // Also need to check the Y axis overflow
  var overflowYStyle

  if (typeof rootElem.currentStyle !== 'undefined')
    overflowYStyle = rootElem.currentStyle.overflowY

  overflowYStyle = overflowYStyle || window.getComputedStyle(rootElem, '').overflowY

  var contentOverflows = rootElem.scrollHeight > rootElem.clientHeight
  var overflowShown    = /^(visible|auto)$/.test(overflowStyle) || /^(visible|auto)$/.test(overflowYStyle)
  var alwaysShowScroll = overflowStyle === 'scroll' || overflowYStyle === 'scroll'

  return (contentOverflows && overflowShown) || (alwaysShowScroll)
}
"""]]

If I missed something, or if _this_ solution is a bit of an oversimplification
(le sigh), please let me know in the comments.
[[!meta date="2014-07-12"]][[!meta author="Tyler Cipriani"]][[!meta license="""
[[Creative Commons Attribution-ShareAlike License|https://creativecommons.org/licenses/by-sa/4.0/]]
"""]][[!meta copyright="""
Copyright &copy; 2016 Tyler Cipriani
"""]][[!meta title="Cross-Browser JavaScript Scrollbar Detection"]]
