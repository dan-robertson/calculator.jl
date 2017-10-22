# Stack and Stack based methods

# todo this changes to struct in other versions of julia
immutable Stack
    items::Array{Any, 1}
end
Base.length(s::Stack) = length(s.items)

function hasMethod(stack::Stack, op, arity=2)
    if length(stack) < arity
        false
    else
        types = map(typeof, stack.items[end-arity+1:end])
        tuple = Tuple{types...}
        ms = methods(op, tuple)
        length(ms) > 0
    end
end
function applyOp(stack::Stack, op, arity=2; splat=false)
    try
        newItem = op(stack.items[end-arity+1:end]...)
        toPush = splat ? length(newItem) : 1
        newStack = if arity < toPush
            if splat
                [stack.items[1:end-arity] ; newItem]
            else
                [stack.items[1:end-arity] ; [newItem]]
            end
        elseif splat
            ns = stack.items[1:end-arity+toPush]
            ns[end-toPush+1:end] = newItem
            ns
        else
            ns = stack.items[1:end-arity+1]
            ns[end] = newItem
            ns
        end
        Stack(newStack)
    catch y
        show(y)
        stack
    end
end

function push(stack::Stack, vals...; simplification=NumericSimplification())
    simp(x) = simplify(simplification, x)
    Stack([stack.items ; [map(simp, vals)...]])
end
