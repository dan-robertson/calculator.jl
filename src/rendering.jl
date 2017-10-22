# rendering the various parts of the calculator


# render for stack
# should return new value for bottom
# default: call show and don't line wrap
function renderStackItem!(b::DLB, item, x, bottom, w)
    buf = IOBuffer()
    stream = IOContext(buf, compact = true, limit = true)
    show(stream, item) # Maybe use display instead
    shown = takebuf_string(buf)
    lines = split(shown, r"\r?\n") # get lines
    for line in lines[end:-1:1]
        shape = shapeText(b, line)
        (w,lot) = simpleTextLayout(shape)
        y = bottom-30
        pushText!(b, Rect(x,y,w,30), Colour(0), lot)
#        pushText!(b, Rect(x,y,w,30), Colour(0), (0,20), line)
        bottom = y
    end
    bottom
end

function renderStack!(b, stack::Stack, bottom, width, entry=nothing)
    x = 50
    x1 = 45
    width = width-x
    if entry != nothing
        newbottom = renderStackItem!(b, entry, x, bottom, width)
        bottom = newbottom
    end
    for (n,item) in enumerate(stack.items[end:-1:1])
        newbottom = renderStackItem!(b, item, x, bottom, width)
        # generate a number for the item
        (w,lot) = simpleTextLayout(shapeText(b, dec(n) * ":"))
        pushText!(b, Rect(x1-w, newbottom, w, 30), Colour(0.6), lot)
        if newbottom <= 0
            break
        end
        bottom = newbottom
    end
end


# Buttons
abstract Operation

type OperationDescription
    name :: String
    description :: String
    operation :: Operation
end

abstract Button

type DeadButton <: Button end # a disabled button with no text
type Button1 <: Button
    op :: OperationDescription
    w :: Int64
    h :: Int64
    function Button1(op :: OperationDescription)
        new(op, 1, 1)
    end
    function Button1(op :: OperationDescription, w :: Integer, h :: Integer)
        new(op,convert(Int64, w),convert(Int64, h))
    end
    function Button1(b :: Button1, w :: Integer, h :: Integer)
        new(b.op,convert(Int64, w),convert(Int64, h))
    end
end

Button(op :: OperationDescription) = Button1(op)
Button(op :: OperationDescription, w :: Integer, h :: Integer) = Button1(op, w, h)
Button(b :: Button1, w :: Integer, h :: Integer) = Button(b.op, w, h)

buttonHint(op :: OperationDescription, s) = op.description
buttonHint(b :: Button1, s) = buttonHint(b.op, s)
buttonHint(x, s) = ""

buttonSize(b) = (b.w, b.h)
buttonSize(b :: Button1) = (b.w, b.h)

function renderButtons(b, state, r::Rect, buttons::Array{Button, 2}, highlightedButton=nothing)
    # draw a background for all the buttons
    pushRect!(b, r, Colour(0.95))
    # now the buttons
    rendered = falses(buttons)
    (rows,cols) = size(buttons)
    cellw = r.w / cols
    cellh = r.h / rows
    for row=1:rows
        for col=1:cols
            button = buttons[row, col]
            if rendered[row, col]
                # check that this button matches above/left of it
                if !((row == 1 || button == buttons[row-1,col])
                     || (col == 1 || button == buttons[row,col-1]))
                    error("Bad button layout at ["*dec(row)*", "*dec(col)*"]")
                end
            else
                (bw, bh) = buttonSize(button)
                # check it will fit
                if bw+col-1 > cols
                    error("Button too wide at ["*dec(row)*", "*dec(col)*"]")
                elseif bh+row-1 > rows
                    error("Button too tall at ["*dec(row)*", "*dec(col)*"]")
                end
                # render
                renderButton(b, state, button,
                             Rect(r.x+(col-1)*cellw, r.y+(row-1)*cellh,
                                  cellw*bw, cellh*bh),
                             button == highlightedButton)
                # mark as rendered
                for ro=row:row+bh-1
                    for co=col:col+bw-1
                        rendered[ro,co] = true
                    end
                end
            end
        end
    end
end

function renderButton(b, state, button::Button1, r, highlighted)
    renderButton(b, state, button.op, r, highlighted)
end

function renderButton(b, state, description::OperationDescription, r::Rect, highlighted)
    renderButton(b, state, description.name, r, Colour(highlighted ? 0.8 : 0.93))
end

function renderButton(b, state, text::String, r::Rect, colour::Colour)
    pushRect!(b, Rect(r.x+1,r.y+1, r.w-2, r.h-2), colour)
    shape = shapeText(b, text)
    (w,lot) = simpleTextLayout(shape)
    x = r.x + (r.w - w) / 2
    y = r.y + (r.h - 30) / 2
    pushText!(b, Rect(x, y, w, 30), Colour(0.2), lot)
end

function renderButton(b, state, button::DeadButton, r, highlighted)
    renderButton(b, state, "", r, Colour(0.94))
end

# Messages

type Message
    text :: String
    doneBy :: UInt64 # from time_ns. We don't care about wrapping as it happens every 5 years
    function Message(text; timeoutms=500)
        if timeoutms == -1 then
            new(text, 0xFFFF_FFFF_FFFF_FFFF)
        else
            Message(text, UInt64(timeoutms*1000000))
        end
    end
    function Message(text,timeoutns :: UInt64)
        doneBy = time_ns() + timeoutns
        new(text, doneBy)
    end
    function Message(text, timeout :: Dates.FixedPeriod)
        Message(text, timeoutms=Dates.toms(timeout))
    end
end

function renderMessage(b, rect, messages, colour=Colour(0))
    if length(messages) == 0
        return
    end
    now = time_ns()
    top = messages[end]
    while top.doneBy < now
        pop!(messages)
        if length(messages) == 0
            return
        end
        top = messages[end]
    end
    pushRect!(b, rect, Colour(1))
    pushText!(b, rect, colour, (30,20), top.text)
end
