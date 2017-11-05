

# should have allowImmediateRematch = false
@defrules canonicalise allowImmediateRematch=false onMatch=:deeper

@defrule canonicalise (+a)   --> a

@defrule canonicalise (*(a)) --> a
@defrule canonicalise (a*(-b)) --> - (a*b) # TODO: make this work better
@defrule canonicalise  a^1   --> a # after doing this on subexpressions
@defrule canonicalise (+a)   --> a # terms in a sum may look equal
