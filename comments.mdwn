[[!sidebar content="""
[[!inline pages="comment_pending(./posts/*)" feedfile=pendingmoderation
description="comments pending moderation" show=-1]]
Comments in the [[!commentmoderation desc="moderation queue"]]:
[[!pagecount pages="comment_pending(./posts/*)"]]
"""]]

Recent comments on posts in the [[blog|index]]:
[[!inline pages="./posts/*/Discussion or comment(./posts/*)"
template="comment"]]
