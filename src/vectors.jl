

# we implement a partial vector as a linked list of vectors
immutable PartialVectorPart
    contents :: Array{Any, 1}
    next :: Nullable{PartialVectorPart}
    function PartialVectorPart{T<:Any}(next :: Nullable{PartialVectorPart}, as :: Array{T,1})
        new(as, next)
    end
    function PartialVectorPart{T<:Any}(next :: PartialVectorPart, as :: Array{T,1})
        new(as, Nullable{PartialVectorPart}(next))
    end
    function PartialVectorPart{T<:Any}(as :: Array{T,1})
        new(as, Nullable{PartialVectorPart}())
    end
end

immutable PartialVector
    parts :: Nullable{PartialVectorPart}
    function PartialVector(ps :: PartialVectorPart)
        new(Nullable{PartialVectorPart}(ps))
    end
    function PartialVector(ps :: Nullable{PartialVectorPart})
        new(ps)
    end
    function PartialVector(as...)
        if length(as) == 0
            new(Nullable{PartialVectorPart}())
        else
            pvpush(new(Nullable{PartialVectorPart}()), as...)
        end
    end
end

const pvpmax = 16

function pvpush(pv :: PartialVector, items...)
    if isnull(pv.parts)
        PartialVector(pvpush(PartialVectorPart([]), items...))
    elseif length(items) == 0
        pv
    else
        PartialVector(pvpush(get(pv.parts), items...))
    end
end

function pvpush(pvp :: PartialVectorPart, items...)
    if length(items) == 0
        return pvp
    end
    if length(pvp.contents) < pvpmax
        stop = pvpmax-length(pvp.contents)
        next = PartialVectorPart(pvp.next, [pvp.contents ; items[1:min(stop, end)]...])
        rest = items[stop+1:end]
    else
        next = PartialVectorPart(pvp, [items[1:min(pvpmax, end)]...])
        rest = length(items) < pvpmax ? () : items[pvpmax+1:end]
    end
    pvpush(next, rest...)
end

function pvpop(pv :: PartialVector)
    if isnull(pv.parts)
        pv
    else
        PartialVector(pvpop(get(pv.parts)))
    end
end

function pvpop(pvp :: PartialVectorPart)
    if length(pvp.contents) > 0
        PartialVectorPart(pvp.next, pvp.contents[1:end-1])
    elseif isnull(pvp.next)
        pvp.next
    else
        pvpop(get(pvp.next))
    end
end

function toVector(pv :: PartialVector)
    # compute length and a type for the elements
    len = 0
    c = pv.parts
    T = Union{}
    while !isnull(c)
        c2 = get(c)
        len += length(c2.contents)
        if T != Any
            for elem in c2.contents
                # T = promote_type(T, typeof(elem))
                T = typejoin(T, typeof(elem))
            end
        end
        c = c2.next
    end
    result = Array{T,1}(len)
    i = len
    c = pv.parts
    while !isnull(c)
        c2 = get(c)
        for j=length(c2.contents):-1:1
            result[i] = c2.contents[j]
            i = i - 1
        end
        c = c2.next
    end
    result
end

function Base.show(io :: IO, pv :: PartialVector)
    function showparts(p)
        if isnull(p)
            return
        else
            p2 = get(p)
            showparts(p2.next)
            for x in p2.contents
                show(io, x)
                write(io, ", ")
            end
        end
    end
    write(io, "[");
    showparts(pv.parts)
    write(io, "...");
    nothing
end

# operations for Partial Vector

openingBracket = InsertionOperation(PartialVector())

# pushes things onto vectors
type CommaOperation <: CommittingOperation end
type ClosingBracketOperation <: CommittingOperation end
type PackOperation <: CommittingOperation end


function pvpos(s :: Stack) # the number of stack elements before the first pv
    items = s.items
    for i=length(items):-1:1
        if isa(items[i], PartialVector)
            return length(items) - i + 1
        end
    end
    return nothing
end

function doOperation(s :: CalcState, op :: CommaOperation; canDup=true)
    commitInput(s)
    pos = pvpos(s.stack)
    if pos == 1 && canDup
        pv = s.stack.items[end]
        if !isnull(pv.parts) && length(get(pv.parts).contents) > 0
            last = get(pv.parts).contents[end]
            s.stack = applyOp(s.stack, v -> pvpush(v, last), 1)
        else
            return true
        end
    elseif pos != nothing
        s.stack = applyOp(s.stack, pvpush, pos)
        return true
    else
        displayError(s, "No vector to extend")
    end
    return false
end

function doOperation(s :: CalcState, op :: ClosingBracketOperation)
    if doOperation(s, CommaOperation(), canDup=false)
        s.stack = applyOp(s.stack, toVector, 1)
    end
end

function doBackspace(s :: CalcState, it :: PartialVector)
    p2 = pvpop(it)
    if isnull(p2.parts)
        nothing
    else
        p2
    end
end

function doOperation(s :: CalcState, op :: PackOperation)
    if length(s.stack) == 0
        error("Need a number of elements.")
    end
    num = s.stack.items[end]
    if !isa(num, Integer) || num < 0
        error("Need a non-negative integer number of elements. Got $num")
    end
    if num + 1 > length(s.stack)
        error("Too few elements on stack.")
    end
    function toVector(xs...)
        T = Union{}
        for i = 1:num
            T = typejoin(T, typeof(xs[i]))
        end
        r = Array{T,1}(num)
        for i = 1:num
            r[i] = xs[i]
        end
        r
    end
    s.stack = applyOp(s.stack, toVector, num+1)
end

function vector_index{T<:Integer}(n :: T)
    collect(T,1:n)
end
