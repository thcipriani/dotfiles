[[!if test="enabled(sidebar)" then="""
[[!sidebar]]
""" else="""
[[!inline pages=sidebar raw=yes]]
"""]]

[[!inline pages="page(./posts/*) and !*/Discussion" show="10"
actions=yes rootpage="posts"]]


This blog is powered by [ikiwiki](http://ikiwiki.info).
