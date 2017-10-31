immutable MetaOperation{ContextType} <: Operation
    con :: ContextType
end
MetaOperation{T <: OperationContext}(::Type{T}, args...) = MetaOperation{T}(T(args...))

# maybe this should not happen?
function combineOperationContext{T <: OperationContext}(c1 :: T, c2 :: T)
    DefaultContext()
end

function combineOperationContext(c1 :: OperationContext, c2 :: OperationContext)
    c2
end

contextEnabled{T <: OperationContext}(::Type{T}, opc :: T) = true
contextEnabled(::Type, opc :: OperationContext) = false

function doOperation{C <: OperationContext}(con :: OperationContext,
                                            s :: CalcState, op :: MetaOperation{C})
    s.operationContext = combineOperationContext(con, op.con)
end

# and now some standard meta operations

immutable MapContext <: OperationContext end

function toMap(op :: StandardOperation)
    func = op.func
    function mapped(as...)
        broadcast(func, as...)
    end
    StandardOperation(mapped, op.arity)
end

immutable ReduceContext <: OperationContext end

function toReduce(op :: StandardOperation, assoc :: Associativity = Associative())
    if op.arity != 2
        error("Can only reduce binary operations")
    end
    func = op.func
    if assoc == LeftAssociative()
        function reducedl{T}(x :: Array{T,1})
            foldl(func,x)
        end
        reduced = reducedl
    elseif assoc == RightAssociative()
        function reducedr{T}(x :: Array{T,1})
            foldr(func,x)
        end
        reduced = reducedr
    else
        function reduceda{T}(x :: Array{T,1})
            reduce(func,x)
        end
        reduced = reduceda
    end
    StandardOperation(reduced, 1)
end

function doOperation(c :: Union{MapContext,ReduceContext}, s :: CalcState, unknownOp :: Operation)
    so = nothing
    try
        so = extractStandardOperation(unknownOp)
    catch e
        op = isa(c, MapContext) ? "map" : "reduce"
        displayError(s, "Don't know how to $op this ($(typeof(unknownOp)))")
        return
    end
    s2 = so :: StandardOperation
    m = isa(c, MapContext) ? toMap(s2) : toReduce(s2, associativity(unknownOp))
    doFullOperation(DefaultContext(), s, m)
end

function doOperation(c::Union{MapContext, ReduceContext}, s :: CalcState, b :: BackspaceOperation)
    s.operationContext = DefaultContext()
end

function operationPostscript(::Union{MapContext, ReduceContext}, ::CalcState, ::MetaOperation) end
function operationPostscript(::Union{MapContext, ReduceContext}, s::CalcState, op::Operation)
    s.operationContext = DefaultContext()
    operationPostscript(s.operationContest, s, op)
end

