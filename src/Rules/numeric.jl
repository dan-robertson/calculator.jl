

@defrules numeric

@defrule numeric x::Number + y::Number --> $(x+y)
@defrule numeric x::Number * y::Number --> $(x*y)
@defrule numeric x::AbstractFloat / y::AbstractFloat --> $(x/y)
@defrule numeric x::Union{Rational,Integer} / y::Union{Rational,Integer} --> $(x // y)
@defrule numeric x::Number^(y::Integer) --> $(x^y)
