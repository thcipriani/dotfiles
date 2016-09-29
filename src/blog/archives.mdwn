[[!if test="archives/*" then="""
Browse through blog archives by year:
[[!map pages="./archives/* and !./archives/*/* and !*/Discussion"]]
"""
else="""
You need to use the `ikiwiki-calendar` program to generate calendar-based
archive pages.
"""]]
