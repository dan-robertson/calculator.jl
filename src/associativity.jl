abstract Associativity
immutable LeftAssociative <: Associativity end
immutable RightAssociative <: Associativity end
immutable FullyAssociative <: Associativity end

function associativity(op :: Operation)
    try
        so = extractStandardOperation(op)
        associativity(op)
    catch e
        FullyAssociative()
    end
end

function associativity(op :: StandardOperation)
    associativity(op.func)
end

function associativity(unknown)
    FullyAssociative()
end

# we do this in a module where we can define @l, @r, @f
module Assocs

import Calc
import Calc: associativity, LeftAssociative, RightAssociative, FullyAssociative

dequote(s::Symbol) = s
dequote(s::QuoteNode) = dequote(s.value)
dequote(t::Tuple) = map(dequote, t)
function dequote(e::Expr)
    if e.head == :quote 
        e.args[1]
    elseif e.head == :globalref
        dequote(e.args[2])
    else 
        error("not a symbol: $e")
    end
end

macro assoc(t, sym)
    sym = dequote(sym)
    t = dequote(t)
    if isa(sym, Tuple{Vararg{Symbol}})
        if length(sym) > 1
            return :(@assoc $t $(sym[1]) ; @assoc $t $(sym[2:end]))
        else
            return :(@assoc $t $(sym[1]))
        end
    end
    assert(isa(sym, Symbol))
    funt = typeof(eval(Calc,sym))
    if t == :left || t == :LeftAssociative || t == :(LeftAssociative())
        :(associativity(::$funt) = $(LeftAssociative()))
    elseif t == :right || t == :RightAssociative || t == :(RightAssociative())
        :(associativity(::$funt) = $(RightAssociative()))
    elseif t == :assoc || t == :FullyAssociative || t == :(FullyAssociative())
        :(associativity(::$funt) = $(FullyAssociative()))
    else
        error("not right: $t")
    end
end

macro l(s...)
    :(@assoc left $s)
end
macro r(s...)
    :(@assoc right $s)
end
macro a(s...)
    :(@assoc assoc $s)
end

@r :^
@a :* :+
@l :/
@a gcd lcm
@l fill

end
