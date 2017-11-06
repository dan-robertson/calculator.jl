

# should have allowImmediateRematch = false
@defrules canonicalise

# these should happen inside the canonicalise function now.
# but we leave them there for now
#@defrule canonicalise (+a)   --> a

#@defrule canonicalise (*(a)) --> a

# @defrule canonicalise ((a::Number)*x) + ((b::Number)*x) + c --> $(a+b)*$x+$c
# @defrule canonicalise x + ((a::Number)*x) + c --> $(a+one(a))*$x+$c
# @defrule canonicalise x - ((a::Number)*x) + c --> $(-a+one(a))*$x+$c
# @defrule canonicalise (-x) + ((a::Number)*x) + c --> $(a-one(a))*$x+$c
# @defrule canonicalise ((a::Number)*x) - ((b::Number)*x) + c --> $(a-b)*$x+$c

# @defrule canonicalise ((a::Number)*x) + ((b::Number)*x) --> $(a+b)*$x
# @defrule canonicalise x + ((a::Number)*x) --> $(a+one(a))*$x
# @defrule canonicalise x - ((a::Number)*x) --> $(-a+one(a))*$x
# @defrule canonicalise (-x) + ((a::Number)*x) --> $(a-one(a))*$x
# @defrule canonicalise ((a::Number)*x) - ((b::Number)*x) --> $(a-b)*$x
@defrule canonicalise (a*(-b)) --> - (a*b) # TODO: make this work better
#@defrule canonicalise  a^1   --> a # after doing this on subexpressions
#@defrule canonicalise (+a)   --> a # terms in a sum may look equal
