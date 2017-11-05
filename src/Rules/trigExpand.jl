
@defrules trigExpand

@defrule trigExpand sin(a+b) --> sin(a)*cos(b) + cos(a)*sin(b)
@defrule trigExpand sin(a+a) --> 2*sin(a)*cos(a)
@defrule trigExpand cos(a+b) --> cos(a)*cos(b) - sin(a)*sin(b)
@defrule trigExpand cos(b+b) --> cos(a)^2 - sin(a)^2
# todo: others
@defrule trigExpand cos(-a)    --> cos(a)
@defrule trigExpand cos(-1*a)  --> cos(a)
@defrule trigExpand sin(-a)    --> -sin(a)
@defrule trigExpand sin(-1*a)  --> -sin(a)

