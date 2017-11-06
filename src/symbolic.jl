import Rewrite

immutable SymbolicWrapper <: Number # slightly cheaty
    val
end

swrap(x::SymbolicWrapper) = x
swrap(x) = SymbolicWrapper(x)
function maybeUnwrap(x::SymbolicWrapper)
    if isa(x.val, Number)
        x.val
    else
        x
    end
end
function simplify(simp::Simplification, x::SymbolicWrapper)
    maybeUnwrap(x)
end
function simplify(simp::Canonicalisation, x::SymbolicWrapper)
    maybeUnwrap(swrap(Rewrite.Rules.canonicalise(x.val)))
end
function simplify(simp::NumericSimplification, x::SymbolicWrapper)
    maybeUnwrap(swrap(Rewrite.Rules.canonicalise(x.val)))
end

Base.show(io::IO, rw::SymbolicWrapper) = show(io, rw.val)
Base.display(d::Display, m, rw::SymbolicWrapper) = display(d, m, rw.val)

symbolic(x) = swrap(x)

varOp(x) = InsertionOperation(symbolic(x))

for unary in [:sin, :cos, :tan,
              :sinh, :cosh, :tanh,
              :asin, :acos, :atan,
              :asinh, :acosh, :atanh,
              :sec, :csc, :cot,
              :sech, :csch, :coth,
              :asec, :acsc, :acot,
              :asech, :acsch, :acoth,
              :exp, :log, :abs, :conj, :factorial,
              :floor, :round, :sign, :log10, :sqrt,
              :gamma, :lgamma, :factorize]
    qn = QuoteNode(unary)
    quote
        function Base.$unary(x::SymbolicWrapper)
            SymbolicWrapper(Expr(:call, $qn,
                                 x.val))
        end
    end |> eval
end

for binary in [:min, :max, :beta,
               :besseli, :besselj, :besselk, :bessely,
               :polygamma, :^, :+, :-, :*, :/]
    qn = QuoteNode(binary)
    quote
        function Base.$binary(x::SymbolicWrapper, y::SymbolicWrapper)
            SymbolicWrapper(Expr(:call, $qn,
                                 x.val, y.val))
        end
        function Base.$binary(x::SymbolicWrapper, y::Number)
            $binary(x,convert(SymbolicWrapper,y))
        end
        function Base.$binary(x::Number, y::SymbolicWrapper)
            $binary(convert(SymbolicWrapper,x),y)
        end
    end |> eval
end

Base.promote_rule{T<:Number}(::Type{SymbolicWrapper},::Type{T}) = SymbolicWrapper
Base.convert{S}(::Type{SymbolicWrapper},::Irrational{S}) = symbolic(S)
Base.convert(::Type{SymbolicWrapper},x::SymbolicWrapper) = x
# function Base.convert(::Type{SymbolicWrapper},x::AbstractFloat)
#     rat = rationalize(BigInt, x)
#     n = rat.num
#     d = rat.den
#     SymbolicWrapper(:($n // $d))
# end
Base.convert(::Type{SymbolicWrapper},x) = symbolic(x)
