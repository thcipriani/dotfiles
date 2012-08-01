---
layout: post
title: CSS display inline-block Extra Margin/Space
---
**TL;DR**: The margin isn&#8217;t a margin&#8212;it&#8217;s a space. Try 
using display: block; float: left; instead. 

**ALSO:** Checkout the amazing list of techniques to combat inline-block 
space-margins at [CSS Tricks](http://css-tricks.com/fighting-the-space-between-inline-block-elements/ "Fighting the Space Between inline-block Elements")

<hr>

I&#8217;ve made a weird discovery about using the inline-block display. 
I use inline-block elements on my 
[homepage](http://www.tylercipriani.com, "Tyler Cipriani") 
in the _Recent Work_ section.

I noticed that the math for these block elements wasn&#8217;t quite adding 
up&#8212;the element width + padding + margin + border should&#8217;ve enabled
three items on one line; however, the elements were wrapping.

Here&#8217;s an example of what I&#8217;m talking about:
<iframe style="margin: 1em 0; width: 100%; height: 300px;" src="http://jsfiddle.net/thcipriani/r7egr/embedded/result,html,css/" allowfullscreen="allowfullscreen" frameborder="0"> </iframe>

Even though the margin of each element is set to zero there is seemingly, a margin, between each element.

This problem could be easily solved by removing the display property, floating each gallery item and using a clearfix on the parent:
<iframe style="margin: 1em 0; width: 100%; height: 300px;" src="http://jsfiddle.net/thcipriani/r7egr/2/embedded/result,html,css/" allowfullscreen="allowfullscreen" frameborder="0"> </iframe>

That solution, however, ignores <em>why</em> that space is being added.

The inline-block display property treats block level elements 
(e.g. &lt;div&gt;) as an inline element (e.g. &lt;span&gt;) <span class="amp">&amp;</span>,
just like if you had a line break between two &lt;span&gt; elements, the 
line-break between the &lt;div&gt;s is creating a space between the &lt;div&gt;s.
**That extra margin is actually a space&#8212;not a margin**.

Knowing what is creating an inline-block displayed &lt;div&gt;&#8217;s 
&#8220;margin&#8221; enables you to create a couple of different solutions.
* **First**, the dumb and breakable solution, you could remove the spaces 
and line-breaks in your html
* **Second**, directly address the space by setting a negative right margin or negative word-spacing for those elements
* Or, **Finally**, just use display: block; float: left;

Here&#8217;s an example of what the second solution would look like:
<iframe style="margin: 1em 0; width: 100%; height: 300px" src="http://jsfiddle.net/thcipriani/r7egr/3/embedded/result,html,css/" allowfullscreen="allowfullscreen" frameborder="0"> </iframe>
[[!meta date="2012-08-01"]][[!meta author="Tyler Cipriani"]][[!meta license="""
[[Creative Commons Attribution-ShareAlike License|https://creativecommons.org/licenses/by-sa/4.0/]]
"""]][[!meta copyright="""
Copyright &copy; 2016 Tyler Cipriani
"""]][[!meta title="display-inline-block-extra-margin.md"]]