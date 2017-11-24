# OperationContext defined in state.jl, near CalcState
immutable DefaultContext <: OperationContext end
# Operation defined in rendering.jl, near Button
abstract type CommittingOperation <: Operation end
abstract type EntrySpecialOperation <: Operation end # special behaviour when there is an entry
immutable StandardOperation <: CommittingOperation
    func
    arity :: Int64
end
immutable SplattingOperation <: CommittingOperation
    func
    arity :: Int64
end
immutable EntryOperation <: Operation
    initial
    extend
    function EntryOperation(x)
        new(x,x)
    end
    function EntryOperation(initial, extension)
        new(initial, extension)
    end
end
immutable NegationOperation <: EntrySpecialOperation end
immutable BackspaceOperation <: Operation end
immutable EnterOperation <: Operation end
immutable MakeComplexOperation <: EntrySpecialOperation end
immutable FlagOperation{Flag} <: Operation end
FlagOperation(flag :: Symbol) = FlagOperation{flag}()
immutable InsertionOperation <: CommittingOperation
    value
end

function doFullOperation(c :: OperationContext, s :: CalcState, op)
    try
        operationPrelude(c, s, op)
        doOperation(c, s, op)
    catch e
        displayError(s, e)
    end
    try
        operationPostscript(c, s, op)
    catch e
        displayError(s, e)
    end
end
doFullOperation(s :: CalcState, op) = doFullOperation(s.operationContext, s, op)

function operationPrelude(con :: OperationContext, s :: CalcState, op)
    operationPrelude(s, op)
end
function doOperation(con :: OperationContext, s :: CalcState, op)
    doOperation(s, op)
end

function operationPrelude(s :: CalcState, unknownOperation)
    # do nothing
end
function operationPrelude(s :: CalcState, op :: CommittingOperation)
    commitInput(s)
end
function operationPrelude(con :: OperationContext, s :: CalcState, op :: OperationDescription)
    operationPrelude(con, s, op.operation)
end

function operationPostscript(::OperationContext, s::CalcState,::Operation)
    clearModifiers(s)
end
function operationPostscript(::OperationContext,::CalcState,::FlagOperation) end
function operationPostscript(c::OperationContext,s::CalcState,op::OperationDescription)
    operationPostscript(c, s, op.operation)
end

function doOperation(s :: CalcState, unknownOperation)
    displayError(s, "Don't know how to do " * string(typeof(unknownOperation)))
end

function doOperation(s :: CalcState, op :: DeadButton)
end
function doOperation(con :: OperationContext, s :: CalcState, op :: OperationDescription)
    doOperation(con, s, op.operation)
end

function doOperation(s :: CalcState, op :: Union{StandardOperation, SplattingOperation})
    if hasMethod(s.stack, op.func, op.arity)
        try
            stack2 = applyOp(s.stack, op.func, op.arity, splat=isa(op, SplattingOperation))
            s.stack = stack2
        catch e
            displayError(s, e)
        end
    else
        displayError(s, "Invalid Types")
    end
end

extractStandardOperation(op :: StandardOperation) = op
extractStandardOperation(op :: OperationDescription) = extractStandardOperation(op.operation)
extractStandardOperation(op :: NegationOperation) = StandardOperation(-, 1)
extractStandardOperation(op :: MakeComplexOperation) = StandardOperation(complexish, 2)
doStandardOperation(s :: CalcState, op) = doOperation(s, extractStandardOperation(op))

function doOperation{Flag}(s :: CalcState, op :: FlagOperation{Flag})
    s.modifiers = Modifiers(s.modifiers; Flag => !(Flag ∈ s.modifiers))
end

function doOperation(s :: CalcState, op :: EntryOperation)
    s.currentEntry = doEntry(s.currentEntry, op.initial, op.extend)
end

function doOperation(s :: CalcState, op :: NegationOperation)
    if s.currentEntry == nothing
        clearModifiers(s)
        # negate
        doStandardOperation(s, op)
    else
        s.currentEntry = doEntry(s.currentEntry, Negation(), Negation())
    end
end

complexish(re :: Real, ip :: Real) = Complex(re,ip)
function complexish(re :: Number, ip :: Number) 
    x, i, y = promote(re, im, ip)
    x + i*y
end
function doOperation(s :: CalcState, op :: MakeComplexOperation)
    if s.currentEntry == nothing
        clearModifiers(s)
        # combine to make complex
        doStandardOperation(s, op)
    else
        s.currentEntry = doEntry(s.currentEntry,
                                 ComplexEntry{:imaginary}(IntegerEntry(""),IntegerEntry("")),
                                 ComplexPartSwitch())
    end
end

function doBackspace(s :: CalcState, thing) #by default we delete
    nothing
end

function doOperation(s :: CalcState, op :: BackspaceOperation)
    function bop(x)
        b = doBackspace(s, x)
        b == nothing ? [] : [b]
    end
    if s.currentEntry == nothing
        if length(s.stack.items) > 0
            s.stack = applyOp(s.stack, bop, 1, splat=true)
        end
    else
        s.currentEntry = doEntry(s.currentEntry, nothing, Backspace())
    end
end

function doOperation(s :: CalcState, op :: EnterOperation)
    if s.currentEntry == nothing
        clearModifiers(s)
        if length(s.stack.items) > 0
            s.stack = push(s.stack, s.stack.items[end])
        end
    else
        commitInput(s)
    end
end

function doOperation(s :: CalcState, op :: InsertionOperation)
    s.stack = push(s.stack, op.value)
end
