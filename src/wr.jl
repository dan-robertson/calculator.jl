# primitive rendering via wrui

"An object representing a handle to a window"
mutable struct Window
    p::Ptr{Void}
end
"An object to which various display list items are pushed"
mutable struct DLB
    p::Ptr{Void}
end
"An object representing a hanlde to a font"
mutable struct Font
    p::Ptr{Void}
    size::Float32
end

mutable struct FontRequest
    family::String
    italic::Bool
    weight::Int
    stretch::Int
    size::Float32
    font::Nullable{Font}
    FontRequest(family, size; italic=false, weight=400, stretch=5) =
        new(family, italic, weight, stretch, size, Nullable{Font}())
end


module EventParse

export Event, NoEvent, MouseEvent, MouseButtonEvent
export MouseButtonPressedEvent, MouseButtonReleasedEvent
export LeftMouseButtonPressed, LeftMouseButtonReleased
export MiddleMouseButtonPressed, MiddleMouseButtonReleased
export RightMouseButtonPressed, RightMouseButtonReleased
export OtherMouseButtonPressed, OtherMouseButtonReleased
export MouseMoveEvent
export ResizeEvent, FocusEvent, BlurEvent, ClosedEvent
export KeyboardEvent, ReceivedCharacter, KeyPressEvent, KeyReleaseEvent
# Is this horrid?
immutable NoEvent end
abstract Event
abstract MouseEvent <: Event
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
immutable ResizeEvent <: Event
    width::Int64
    height::Int64
end
immutable FocusEvent <: Event end
immutable BlurEvent <: Event end
immutable ClosedEvent <: Event end
abstract KeyboardEvent <: Event
immutable ReceivedCharacter <: KeyboardEvent
    char::Char
end
immutable KeyPressEvent{Sym} <: Event
    scanCode::Int64
end
immutable KeyReleaseEvent{Sym} <: Event
    scanCode::Int64
end

NIL = nothing
T = true
RESIZED = ResizeEvent
REFRESH = nothing
FOCUSED(x) = x == true ? FocusEvent() : BlurEvent()
mouseLast = (0,0) # should probably make this better
function MOUSEMOVED(x, y) 
    global mouseLast = (x,y)
    MouseMoveEvent(x, y)
end
LEFT = :LEFT
RIGHT = :RIGHT
MIDDLE = :middle
PRESSED = :press
RELEASED = :release
MOUSEINPUT(typ, but::Symbol, pos::Void) = MOUSEINPUT(typ, but, mouseLast)
function MOUSEINPUT(typ, but::Symbol, pos)
    func = if typ == :press
        if but == :LEFT
            LeftMouseButtonPressed
        elseif but == :RIGHT
            RightMouseButtonPressed
        elseif but == :middle
            MiddleMouseButtonPressed
        end
    elseif typ == :release
        if but == :LEFT
            LeftMouseButtonReleased
        elseif but == :RIGHT
            RightMouseButtonReleased
        elseif but == :middle
            MiddleMouseButtonReleased
        end
    end
    func(pos[1], pos[2])
end
CLOSED = ClosedEvent()
RECEIVEDCHARACTER = ReceivedCharacter
# what to do when button is NIL?
function KEYBOARDINPUT(typ, sc, button::Symbol)
    if typ == :press
        KeyPressEvent{button}(sc)
    elseif typ == :release
        KeyReleaseEvent{button}(sc)
    else
        error("unknown keyboard event type")
    end
end

end # end of module

importall .EventParse

"""
create a new window with specified title, width and height, and render and event functions.
`render` should take a DLB and a (width, height) float tuple. It can return whatever it likes.
`event` should take a MouseEvent and can return anything.
"""
function newWindow(title::String, size::Tuple{Int, Int})
    (width, height) = size
    p = ccall((:wrui_new_window, "libwrui"), Ptr{Void}, (Cstring, UInt32, UInt32),
              title, width, height)
    win = Window(p)
    finalizer(win, (w) -> ccall((:wrui_close_window, "libwrui"), Void, (Ptr{Void},), w.p))
    win
end

function closeWindow(win::Window)
    finalize(win)
end

function parseEvent(s::AbstractString)
    try
        e = SExpression.read(EventParse, s)
        if e == nothing
            return NoEvent()
        else
            return e
        end
    end
    warn("unknown event $s")
    return nothing
end

function getEvent(win::Window, block::Bool)
    str = ccall((:wrui_get_event_sexp, "libwrui"), Cstring, (Ptr{Void}, Int32),
                win.p, block ? 1 : 0)
    string = unsafe_string(str)
    r = parseEvent(string)
    ccall((:wrui_free_event, "libwrui"), Void, (Cstring,), str)
    return r
end
waitEvent(win) = getEvent(win, true)
pollEvent(win) = getEvent(win, false)

function getBuilder(win::Window)
    width = Ref{Float32}(0)
    height = Ref{Float32}(0)
    p = ccall((:wrui_display_list_builder, "libwrui"), Ptr{Void},
              (Ptr{Void}, Ref{Float32}, Ref{Float32}),
              win.p, width, height)
    dlb = DLB(p)
    (dlb, (width.x, height.x))
end

function buildFrame(win::Window, dlb::DLB)
    ccall((:display_list_builder_build, "libwrui"), Void,
          (Ptr{Void}, Ptr{Void}), win.p, dlb.p)
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

function getFont(dlb::DLB, family::String, size::Number;
                 italic::Bool=false, weight::Number=400, stretch::Number=5)
    p = ccall((:wrui_find_font, "libwrui"), Ptr{Void},
              (Ptr{Void}, Cstring, Float32, Int32, UInt16, UInt8),
              dlb.p, family, size, italic ? 1 : 0, weight, stretch)
    f = Font(p, size)
    finalizer(f, (font) -> ccall((:wrui_free_font, "libwrui"), Void, (Ptr{Void},), font.p))
    f
end

function getFont(dlb::DLB, font::FontRequest)
    if isnull(font.font)
        font.font = Nullable{Font}(getFont(dlb, font.family, font.size,
                                           italic=font.italic, weight=font.weight,
                                           stretch=font.stretch))
    end
    get(font.font)
end

immutable ShapedRunInfo
    width :: Float32
    is_whitespace_ :: Cint
end
type ShapedText
    p :: Ptr{Void}
    fontSize :: Float32
    info :: Vector{ShapedRunInfo}
end
isWhitespace(s::ShapedRunInfo) = s.is_whitespace_ != 0

function shapeText(font::Font, text)
    # shape the text
    p = ccall((:wrui_shape_text, "libwrui"), Ptr{Void},
              (Ptr{Void}, Cstring), font.p, text)
    # now get the widths
    len = Ref{Csize_t}(0)
    sri = ccall((:shaped_text_get_widths, "libwrui"), Ptr{ShapedRunInfo},
                (Ptr{Void}, Ref{Csize_t}), p, len)
    arr = unsafe_wrap(Array{ShapedRunInfo}, sri, len.x)
    # we need to put the pointer into an object (non-bits type) to finalize it.
    # First the finalizer for the array
    finalizer(arr, (a) -> ccall((:shaped_text_widths_free, "libwrui"), Void,
                                (Ptr{ShapedRunInfo}, Csize_t), a, length(a)))
    st = ShapedText(p, font.size, arr)
    finalizer(st, (x) -> ccall((:wrui_free_shaped_text, "libwrui"), Void,
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
function pushText!(dlb::DLB, r::Rect, f::Font, c::Colour, origin::NTuple{2}, text::AbstractString)
    ccall((:display_list_builder_push_text, "libwrui"), Void,
          (Ptr{Void}, Float32, Float32, Float32, Float32, Ptr{Void},
           Float32, Float32, Float32, Float32,
           Float32, Float32, Cstring), dlb.p, r.x, r.y, r.w, r.h, f.p, c.r, c.g, c.b, c.a,
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
    y = st.fontSize
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
