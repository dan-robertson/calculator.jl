baremodule StandardOperations

# we want almost everything from Base
import Base
using Base: +, -, *, /, ==, <, >, <=, >=, !, !=, =>
using Base: AbstractVector, BigFloat, BigInt
using Base: binomial, display, fill, gensym, getindex, length, push!, string
#using Base:

using Calc: FlagButton, Button
using Calc: Multibutton, OperationDescription, StandardOperation, SplattingOperation
using Calc: InsertionOperation
using Calc: NegationOperation, BackspaceOperation, EnterOperation
using Calc: swrap, SymbolicWrapper
import Rewrite

export inverse, hyperbolic
inverse = FlagButton(:inverse, "Inv", 'I')
hyperbolic = FlagButton(:hyperbolic, "Hyp", 'H')

mop(arity, name) = StandardOperation(getfield(Base.Math, name), arity)
function nameop(name, str, i, h)
    if !i && !h
        (string(name), str, mop(1, name))
    elseif i && !h
        ("arc" * string(name), "inverse " * str, mop(1, Symbol(:a, name)))
    elseif !i && h
        (string(name) * "h", "hyperbolic " * str, mop(1, Symbol(name, :h)))
    elseif i && h
        ("ar" * string(name) * "h", "inverse hyperbolic " * str, mop(1, Symbol(:a, name, :h)))
    end
end
trig(name, str, char) = Multibutton(trig(name, str), char)
trig(name, str=string(name)) = Multibutton(
    (                      ) => OperationDescription(nameop(name, str, false, false)...),
    (:inverse,             ) => OperationDescription(nameop(name, str, true,  false)...),
    (          :hyperbolic,) => OperationDescription(nameop(name, str, false, true )...),
    (:inverse, :hyperbolic ) => OperationDescription(nameop(name, str, true,  true )...))

# trig

export sin, cos, tan, sec, cosec, cot
sin = trig(:sin, "sine", 'S')
cos = trig(:cos, "cosine", 'C')
tan = trig(:tan, "tangent", 'T')
sec = trig(:sec, "secant")
cosec = trig(:csc, "cosecant")
cot = trig(:cot, "cotangent")

macro family(arity, description)
    :(@family $arity $(nothing) $description)
end

macro family(arity, char, description)
    if description.head != :vcat || length(description.args) != 4
        error("Format @family arity, {char}, [default ; inv ; hyp ; inv hyp] where rows are either 
'function [name [description]]' or '(symbol, function) [name [description]]' and if arity is 0, function is an operation");
    end
    rows = Tuple{Any, Any, String, String}[] # first arg should really be Union{Symbol, Void}
    for rowish in description.args
        if isa(rowish, Expr) && rowish.head == :row
            fst = rowish.args[1]
            if isa(fst, Expr) && fst.head == :tuple
                if length(fst.args) != 2
                    error("Not a pair: $fst")
                end
                f1 = fst.args[1]
                f2 = fst.args[2]
            else
                f1 = fst
                f2 = f1
            end
            if length(rowish.args) > 1
                if isa(rowish.args[2], String)
                    n = rowish.args[2]
                    if length(rowish.args) > 2
                        if isa(rowish.args[3], String)
                            d = rowish.args[3]
                        else
                            error("Not a string: $rowish.args[3]")
                        end
                    else
                        d = ""
                    end
                else
                    error("Not a string: $rowish.args[2]")
                end
            elseif isa(rowish.args[1], Symbol)
                n = string(rowish.args[1])
                d = ""
            else
                error("Can't choose name for $rowish")
            end
            push!(rows, (f1, f2, n, d))
        elseif isa(rowish, Expr) && rowish.head == :tuple && length(rowish.args) == 2 && isa(rowish.args[1], Symbol)
            push!(rows, (rowish.args[1], rowish.args[2], string(rowish.args[1]), ""))
        elseif isa(rowish, Symbol)
            push!(rows, (rowish, rowish, string(rowish), ""))
        else
            error("Can't choose name for $rowish")
        end
    end
    ods = [gensym(), gensym(), gensym(), gensym()]
    function od(odsym, tup)
        (sym, x, n, d) = tup
        if isa(x, Expr) && x.head == :globalref
            x = x.args[2]
        end
        expr = isa(x, Symbol) ? :(Base.Math.$x) : x
        if arity == 0
            :($odsym = OperationDescription($n, $d, $expr))
        else
            :($odsym = OperationDescription($n, $d, StandardOperation($expr, $arity)))
        end
    end
    function ops(indices)
        name = rows[indices[1]][1]
        if isa(name, Symbol) || (isa(name, Expr) && name.head == :globalref)
            if isa(name, Expr)
                name = name.args[2]
            end
            if indices[1] != 1 || char == nothing || isa(char, Expr) && char.head == :globalref && char.args[2] == :nothing
                :(global $name = Multibutton(
                    () => $(ods[indices[1]]), (:inverse,) => $(ods[indices[2]]),
                    (:hyperbolic,) => $(ods[indices[3]]), (:hyperbolic, :inverse) => $(ods[indices[4]])))
            else
                :(global $name = Multibutton(Multibutton(
                    () => $(ods[indices[1]]), (:inverse,) => $(ods[indices[2]]),
                    (:hyperbolic,) => $(ods[indices[3]]), (:hyperbolic, :inverse) => $(ods[indices[4]])), $char))
            end
        end
    end
    defns = Expr(:block, ops([1,2,3,4]), ops([2,1,4,3]), ops([3,4,1,2]), ops([4,3,2,1]))
    r = Expr(:let, defns, od.(ods, rows)...)
    r
end
i(x) = x # we use this to stop @family from treating symbols in a special way
macro i(x)
    if isa(x, Expr) && x.head == :globalref
        x = x.args[2]
    end
    if isa(x, Symbol)
        :(Base.Math.$x)
    else
        x
    end
end

# constants
export pi, e, phi, gamma
@family 0 'P' [(pi,    InsertionOperation(Base.Math.π)) "π" "the constant π=3.14159..."
               (gamma, InsertionOperation(Base.Math.γ)) "γ" "the constant γ=0.57721..."
               (e,     InsertionOperation(Base.Math.e)) "e" "the constant e=2.71828..."
               (phi,   InsertionOperation(Base.Math.φ)) "φ" "the constant φ=1.61803..."]

# exponential functions
export exp, log, exp10, log10
@family 1 'E' [exp "exp" "the exponential function, e^(1:)"
               log "log" "natural logarithm"
               exp10 "10^"
               log10 "lg" "log base 10" ]

log = Multibutton(log, 'L')
# square roots and cube roots

export sqrt, cbrt

@family 1 'Q' [sqrt "sqrt" "square root"
               (x -> x*x) "^2" "square"
               cbrt "cbrt" "cube root"
               (x -> x*x*x) "^3" "cube" ]

# basic arithmetic

export divide, mult, subtract, add, pow
divide = Button(OperationDescription("÷", "", StandardOperation(Base.Math.:/,2)), '/')
mult = Button(OperationDescription("×", "", StandardOperation(Base.Math.:*,2)), '*')
subtract = Button(OperationDescription("−", "", StandardOperation(Base.Math.:-,2)), '-')
add = Button(OperationDescription("+", "", StandardOperation(Base.Math.:+,2)), '+')
pow = Button(OperationDescription("^", "", StandardOperation(Base.Math.:^,2)), '^')

# other simple operations
export abs, inv
abs = Button(OperationDescription("abs", "the magnitude of (1:)", StandardOperation(Base.abs, 1)), 'A')
inv = Button(OperationDescription("^-1", "the multiplicative inverse of (1:)", StandardOperation(Base.inv, 1)), '&')

# rounding functions
export round, floor, ceil
roundo = StandardOperation(x -> Base.round(BigInt, x), 1)
flooro = StandardOperation(x -> Base.floor(BigInt, x), 1)
ceilo =  StandardOperation(x -> Base.ceil(BigInt, x), 1)
@family 0 [(round, i(roundo)) "round" "round to the nearest integer"
           (round, i(roundo)) "round" "round to the nearest integer"
           (floor, i(flooro)) "floor" "round to the next integer towards negative infinity"
           (ceil,  i(ceilo))  "ceil"  "round to the next integer towards positive infinity"]
round = Button(round, 'R')
floor = Button(floor, 'F')
# stack operations

export enter, backspace
backspace = Button(OperationDescription("⌫", "backspace", BackspaceOperation()), '\b')
enter = Button(OperationDescription("Enter", "", EnterOperation()), '\r')

export swap, roll3up, roll3down
@family 0 '\t' [(swap,      SplattingOperation((x,y) -> Any[y,x],2))     "swap"      "swap the top two elements on the stack"
                            SplattingOperation((x,y) -> Any[y,x],2)      "swap"      "swap the top two elements on the stack"
                (roll3up,   SplattingOperation((x,y,z) -> Any[y,z,x],3)) "roll up"   "shift the top item of the stack down two places"
                (roll3down, SplattingOperation((x,y,z) -> Any[z,x,y],3)) "roll down" "pull item (3:) to the top of the stack"]

# vectors

using Calc: CommaOperation, openingBracket, ClosingBracketOperation, PackOperation, vector_index

export comma, openbracket, closebracket, pack, unpack, index, build
comma = Button(OperationDescription(",", "extend the closest partial vector", CommaOperation()), ',')
openbracket = Button(OperationDescription("[", "start a vector", openingBracket), '[')
closebracket = Button(OperationDescription("]", "finish a vector", ClosingBracketOperation()), ']')

packo = PackOperation()
unpacko = SplattingOperation((x::AbstractVector) -> x, 1)
@family 0 [(pack, i(packo))     "pack"   "pack the top (1:) items into a vector"
           (unpack, i(unpacko)) "unpack" "unpack a vector onto the stack"
           i(packo)             "pack"   "pack the top (1:) items into a vector"
           i(unpacko)           "unpack" "unpack a vector onto the stack"]

index = Button(OperationDescription("index", "the vector of natural numbers 1, 2, 3, ..., (1:)",
                                    StandardOperation(vector_index, 1)))
build = Button(OperationDescription("build", "the vector containing (2:) repeated (1:) times",
                                    StandardOperation(fill, 2)))

## mapping, reducing, etc
using Calc: MetaOperation, MapContext, ReduceContext
export map, reduce
map = Button(OperationDescription("map", "do an operation pointwise to a vector",
                                  MetaOperation(MapContext)))
reduce = Button(OperationDescription("reduce", 
                                     "combine all the elements of a vector together with an operation",
                                     MetaOperation(ReduceContext)))

# number theory

export gcd, lcm

@family 2 [gcd    "gcd" "greatest common divisor of (1:) and (2:)"
           @i(gcd) "gcd" "greatest common divisor of (1:) and (2:)" # todo extended gcd?
           lcm    "lcm" "lowest common multiple of (1:) and (2:)"
           @i(lcm) "lcm" "lowest common multiple of (1:) and (2:)"]

# combinatorics

import Combinatorics
export choose, factorial, doublefactorial

choose = Button(OperationDescription("choose", "the binomial coefficient (2:) choose (1:)", StandardOperation(binomial, 2)))

function myFactorial(n :: BigFloat)
    try
        Combinatorics.factorial(n) # the implementation for bigfloat only accepts integers
    catch e
        Base.Math.gamma(n + one(n))
    end
end
myFactorial(n) = Combinatorics.factorial(n)

factorialo = OperationDescription("!", "the factorial of (1:); the number of purmutations of (1:) elements", StandardOperation(myFactorial, 1))
doublefactorialo = OperationDescription("!!", "the double factorial of (1:); the product of all numbers up to (1:) with the same parity as (1:)", StandardOperation(Combinatorics.doublefactorial, 1))
# maybe do multi/hyper/sub/etc factorial. Combinatorics.jl does not extend these to ℂ.
factorial = Button(Multibutton(() => factorialo, (:inverse,) => factorialo, (:hyperbolic,) => doublefactorialo, (:inverse,:hyperbolic) => doublefactorialo), '!')
doublefactorial = Multibutton(() => doublefactorialo, (:inverse,) => doublefactorialo, (:hyperbolic,) => factorialo, (:inverse,:hyperbolic) => factorialo)


# various trancendental functions
# TODO: sort out implementations for bessel functions etc for complex bigfloats
export gamma, beta
gamma = Button(OperationDescription("Γ", "the gamma function", StandardOperation(Base.Math.gamma, 1)))
beta = Button(OperationDescription("β", "the beta function", StandardOperation(Base.Math.beta, 2)))



# algebra

export expand, trigExpand
macro swrap(fun)
    :((x) -> swrap($fun(convert(SymbolicWrapper, x).val)))
end
expand = Button(OperationDescription("expand", "expand products",
                                     StandardOperation(@swrap(Rewrite.Rules.expand), 1)))
trigExpand = Button(OperationDescription("trigExpand", "expand trig functions",
                                         StandardOperation(@swrap(Rewrite.Rules.trigExpand), 1)))


end
