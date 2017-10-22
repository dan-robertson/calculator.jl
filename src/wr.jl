# primitive rendering via wrui

"An object to which various display list items are pushed"
immutable DLB
    p::Ptr{Void}
end

# Is this horrid?
abstract MouseEvent
abstract MouseButtonEvent <: MouseEvent
abstract MouseButtonPressedEvent <: MouseButtonEvent
abstract MouseButtonReleasedEvent <: MouseButtonEvent
immutable LeftMouseButtonPressed <: MouseButtonPressedEvent
    x::Float32
    y::Float32
end
immutable RightMouseButtonPressed <: MouseButtonPressedEvent
    x::Float32
    y::Float32
end
immutable MiddleMouseButtonPressed <: MouseButtonPressedEvent
    x::Float32
    y::Float32
end
immutable OtherMouseButtonPressed <: MouseButtonPressedEvent
    x::Float32
    y::Float32
    button::UInt32
end
immutable LeftMouseButtonReleased <: MouseButtonReleasedEvent
    x::Float32
    y::Float32
end
immutable RightMouseButtonReleased <: MouseButtonReleasedEvent
    x::Float32
    y::Float32
end
immutable MiddleMouseButtonReleased <: MouseButtonReleasedEvent
    x::Float32
    y::Float32
end
immutable OtherMouseButtonReleased <: MouseButtonReleasedEvent
    x::Float32
    y::Float32
    button::UInt32
end
immutable MouseMoveEvent <: MouseEvent
    x::Float32
    y::Float32
end
function mouseEvent(p, b)
    (x,y) = p
    if b == 0
        MouseMoveEvent(x,y)
    elseif b > 0 # pressed
        if b == 1
            LeftMouseButtonPressed(x,y)
        elseif b == 2
            RightMouseButtonPressed(x,y)
        elseif b == 3
            MiddleMouseButtonPressed(x,y)
        else
            OtherMouseButtonPressed(x,y,b-4)
        end
    else # b < 0   released
        if b == -1
            LeftMouseButtonReleased(x,y)
        elseif b == -2
            RightMouseButtonReleased(x,y)
        elseif b == -3
            MiddleMouseButtonReleased(x,y)
        else
            OtherMouseButtonReleased(x,y,4+b)
        end
    end
end

module SuperCheatyClosures end
"Given a closure make a new global function which does the same thing.
Return that global function"
function superCheatyGlobaliseClosure(closure, argTypes...)
    closureSym = gensym("SuperCheatyGlobalClosure")
    makingGlobal = :(global $closureSym = $closure)
    globFunSym = gensym("SuperCheatyGlobalFunction")
    argnames = map((x) -> gensym("arg"), argTypes)
    argdefs = map((n,t) -> :($n :: $t), argnames, argTypes)
    defcall = Expr(:call, globFunSym, argdefs...) # gfs(a1::t1, a2::t2, ...)
    thencall = Expr(:call, closureSym, argnames...) # cs(a1, a2, ...)
    funDef = :($defcall = $thencall)
    whole = :($makingGlobal; $funDef)
    eval(SuperCheatyClosures, whole)
end

"""
create a new window with specified title, width and height, and render and event functions.
`render` should take a DLB and a (width, height) float tuple. It can return whatever it likes.
`event` should take a MouseEvent and can return anything.
"""
function newWindow(title::String, size::Tuple{Int, Int}, render, event)
    (width, height) = size
    lastmouse = (0.0, 0.0)
    function update(x,y)
        if x > -99990 || y > -99990
            lastmouse = (x,y)
        end
    end
    function saferender(dlb::Ptr{Void}, x::Float32, y::Float32)
        try
            render(DLB(dlb), (x,y))
        catch e
            showerror(STDERR, e)
            println(STDERR, "")
            show(STDERR, catch_stacktrace())
            println(STDERR, "")
        end
        nothing
    end
    function safeevent(x::Float32, y::Float32, b::Int32)
        try
            update(x,y)
            event(mouseEvent(lastmouse, b))
        catch e
            showerror(STDERR, e)
            println(STDERR, "")
            show(STDERR, catch_stacktrace())
            println(STDERR, "")
        end
        nothing
    end
    ccall((:new_window, "libwrui"), Void, (Cstring, UInt32, UInt32, Ptr{Void}, Ptr{Void}),
          title, width, height,
          cfunction(superCheatyGlobaliseClosure(saferender, Ptr{Void}, Float32, Float32), Void, (Ptr{Void}, Float32, Float32)),
          cfunction(superCheatyGlobaliseClosure(safeevent, Float32, Float32, Int32), Void, (Float32, Float32, Int32)))
end
immutable Rect
    x :: Float32
    y :: Float32
    w :: Float32
    h :: Float32
end
immutable Colour
    r :: Float32
    g :: Float32
    b :: Float32
    a :: Float32
    function Colour(r,g,b,a)
        new(r,g,b,a)
    end
    function Colour(r,g,b)
        new(r,g,b,1.0)
    end
    function Colour(x)
        new(x,x,x,1.0)
    end
end
function pushRect!(dlb::DLB, r::Rect, c::Colour)
    ccall((:display_list_builder_push_rect, "libwrui"), Void,
          (Ptr{Void}, Float32, Float32, Float32, Float32, Float32, Float32, Float32, Float32),
          dlb.p, r.x, r.y, r.w, r.h, c.r, c.g, c.b, c.a)
end
immutable ShapedRunInfo
    width :: Float32
    is_whitespace_ :: Cint
end
type ShapedText
    p :: Ptr{Void}
    info :: Vector{ShapedRunInfo}
end
isWhitespace(s::ShapedRunInfo) = s.is_whitespace_ != 0
function shapeText(dlb::DLB, text)
    # shape the text
    p = ccall((:display_list_builder_shape_text, "libwrui"), Ptr{Void},
              (Ptr{Void}, Cstring), dlb.p, text)
    # now get the widths
    len = Ref{Csize_t}(0)
    sri = ccall((:shaped_text_get_widths, "libwrui"), Ptr{ShapedRunInfo},
                (Ptr{Void}, Ref{Csize_t}), p, len)
    arr = unsafe_wrap(Array{ShapedRunInfo}, sri, len.x)
    # we need to put the pointer into an object (non-bits type) to finalize it.
    # First the finalizer for the array
    finalizer(arr, (a) -> ccall((:shaped_text_widths_free, "libwrui"), Void,
                                (Ptr{ShapedRunInfo}, Csize_t), a, length(a)))
    st = ShapedText(p, arr)
    finalizer(st, (x) -> ccall((:shaped_text_free, "libwrui"), Void,
                               (Ptr{Void},), x.p))
    st
end
immutable LaidOutText
    s :: ShapedText
    positions :: Vector{NTuple{2, Float32}}
    """A representation of laid out text. The vector of positions corresponds
to the baseline-left corner of each part of the word in the same order as the info vector.
The positions are relative to the rectangle the text is laid out in."""
    function LaidOutText(shaped::ShapedText, positions::Vector{NTuple{2,Float32}})
        assert(length(positions) <= length(shaped.info))
        new(shaped, positions)
    end
end
"""Push some text inside a rectangle of a certain colour.
The origin is the origin of the first character (i.e. the left edge of its box at the baseline),
relative to the rectangle passed in"""
function pushText!(dlb::DLB, r::Rect, c::Colour, origin::NTuple{2}, text::AbstractString)
    ccall((:display_list_builder_push_text, "libwrui"), Void,
          (Ptr{Void}, Float32, Float32, Float32, Float32, Float32, Float32, Float32, Float32,
           Float32, Float32, Cstring), dlb.p, r.x, r.y, r.w, r.h, c.r, c.g, c.b, c.a,
          r.x + origin[1], r.y + origin[2], text)
end
"""Push some laid out text inside a rectangle of a certain colour"""
function pushText!(dlb::DLB, r::Rect, c::Colour, l::LaidOutText)
    ccall((:display_list_builder_push_shaped_text, "libwrui"), Void,
          (Ptr{Void}, Float32, Float32, Float32, Float32, Float32, Float32, Float32, Float32,
           Ptr{Void}, Ref{NTuple{2,Float32}}, Csize_t), dlb.p, r.x, r.y, r.w, r.h,
          c.r, c.g, c.b, c.a, l.s.p, l.positions, length(l.positions))
end

# returns (width, laidOut) where the laid out text starts at (0,20)
function simpleTextLayout(st::ShapedText; includeTrailingWhitespace=true)
    x = 0
    y = 20
    w = x
    positions = Array{Tuple{Float32, Float32}, 1}(length(st.info))
    for (i,elem) in enumerate(st.info)
        positions[i] = (x,y)
        x += elem.width
        if includeTrailingWhitespace || !isWhitespace(elem)
            w = x
        end
    end
    (w, LaidOutText(st,positions))
end
