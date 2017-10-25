import Reduce

immutable ReduceWrapper <: Number
    val
end

rwrap(x::Reduce.RExpr) = rwrap(parse(x))
rwrap(x) = ReduceWrapper(x)
function simplify(simp::Simplification, x::ReduceWrapper)
    isanumberish(x :: Number) = true
    function isanumberish(x :: Expr)
        x.head == :macrocall &&
            (x.args[1] == :(@big_str) || x.args[1] == :(@int128_str))
    end
    isanumberish(x) = false
    if isa(x.val, Number)
        simplify(simp, x.val)
    elseif isanumberish(x.val)
        simplify(simp, eval(x.val))
    elseif isa(x.val, Expr) &&
           x.val.head == :call &&
           x.val.args[1] == :// &&
           isanumberish(x.val.args[2]) &&
           isanumberish(x.val.args[3])
        simplify(simp, eval(x.val))
    else
        x
    end
end


Base.show(io::IO, rw::ReduceWrapper) = show(io, rw.val)
Base.display(d::Display, m, rw::ReduceWrapper) = display(d, m, rw.val)

symbolic(x) = rwrap(Reduce.RExpr(x))

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
              :gamma, :factorize]
    qn = QuoteNode(unary)
    quote
        function Base.$unary(x::ReduceWrapper)
            ReduceWrapper(Reduce.rcall(Expr(:call, $unary,
                                            x.val)))
        end
    end |> eval
end

function Base.lgamma(x::ReduceWrapper)
    log(gamma(x))
end

for binary in [:min, :max, :beta,
               :besseli, :besselj, :besselk, :bessely,
               :polygamma, :^, :+, :-, :*, :/]
    qn = QuoteNode(binary)
    quote
        function Base.$binary(x::ReduceWrapper, y::ReduceWrapper)
            ReduceWrapper(Reduce.rcall(Expr(:call, $qn,
                                            x.val, y.val)))
        end
        function Base.$binary(x::ReduceWrapper, y::Number)
            $binary(x,convert(ReduceWrapper,y))
        end
        function Base.$binary(x::Number, y::ReduceWrapper)
            $binary(convert(ReduceWrapper,x),y)
        end
    end |> eval
end

Base.promote_rule{T<:Number}(::Type{ReduceWrapper},::Type{T}) = ReduceWrapper
Base.convert{S}(::Type{ReduceWrapper},::Irrational{S}) = symbolic(S)
Base.convert(::Type{ReduceWrapper},x::ReduceWrapper) = x
function Base.convert(::Type{ReduceWrapper},x::AbstractFloat)
    rat = rationalize(BigInt, x)
    n = rat.num
    d = rat.den
    ReduceWrapper(:($n // $d))
end
Base.convert(::Type{ReduceWrapper},x) = symbolic(x)

