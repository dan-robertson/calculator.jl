immutable Modifiers
    inverse :: Bool
    hyperbolic :: Bool
    Modifiers(inverse :: Bool, hyperbolic :: Bool) = new(inverse, hyperbolic)
    Modifiers(flags :: Vararg{Symbol}) = new(:inverse ∈ flags, :hyperbolic ∈ flags)
    Modifiers(;inverse=false, hyperbolic=false) = new(inverse, hyperbolic)
    Modifiers(m :: Modifiers; inverse=m.inverse, hyperbolic=m.hyperbolic) = new(inverse, hyperbolic)
end
function Base.:∈(flag :: Symbol, m :: Modifiers)
    if flag == :inverse
        m.inverse
    elseif flag == :hyperbolic
        m.hyperbolic
    else
        false
    end
end
abstract type OperationContext end # e.g. mapping/reducing over a vector

type CalcState
    mainButtons :: Array{Button,2} # The main set of buttons.
    buttons :: Array{Array{Button,2}, 1} # a stack of buttons. We display the top element(s)
    stack :: Stack
    currentEntry
    highlightedButton
    pressedButton
    statusMessage :: Array{Message, 1} # a stack of messages where we display the top valid one
    errorMessage
    modifiers :: Modifiers
    operationContext :: OperationContext
    function CalcState()
        mainButtons = ButtonLayout.mainButtons
        extraButtons = ButtonLayout.extraButtons
        stack = Stack(Any[])
        new(mainButtons, [extraButtons],
            stack, nothing, nothing, nothing, Message[], nothing, Modifiers(),
            DefaultContext())
    end
end


function openWindow(s :: CalcState)
    size = (600,800)
    win = newWindow("Calculator", size)
    while true
        b, size = getBuilder(win)
        renderCalc(b, size, s)
        buildFrame(win, b)

        shouldUpdate = false
        canUpdate = false
        while !(shouldUpdate && canUpdate)
            if canUpdate
                event = getEvent(win, !shouldUpdate);
            else
                event = pollEvent(win)
                if event == NoEvent()
                    canUpdate = true
                    continue
                end
            end
            if event isa ClosedEvent
                closeWindow(win)
                return
            elseif event isa ResizeEvent
                shouldUpdate = true
                size = (event.width, event.height)
            elseif event != nothing
                shouldUpdate = handleEvent(s, size, event) || shouldUpdate
            end
        end
    end
end

buttonWidthRatio = 1/3
function positionButtons(s :: CalcState, winsize) # returns (h, [(rect, buttons)...]) where h is the height of the stack
    mb = s.mainButtons
    (w,h) = winsize
    if length(s.buttons) == 0
        []
        # decide stack height
        buttonWidth = w / size(mb)[2]
        idealButtonHeight = buttonWidth * buttonWidthRatio
        idealHeight = idealButtonHeight * size(mb)[1]
        height = min(idealHeight, h * 2/3)
        y = h - height
        (y, [(Rect(0,y,w,height), mb)])
    else
        top = s.buttons[end]
        vIdealHeight = (w / size(mb)[2] * buttonWidthRatio * size(mb)[1]) +
                       (w / size(top)[2]* buttonWidthRatio * size(top)[2]) # height if we stack buttons on top of eachother
        w2 = w / (1 + (size(mb)[1]*size(top)[2]^2)/(size(top)[1]*size(mb)[2]^2))
        w1 = w - w2
        hIdealHeight = w1 / size(mb)[2] * buttonWidthRatio * size(mb)[1] # height if we stack buttons side by side
        vdif = abs(clamp(vIdealHeight, h*1/3, h*2/3) - vIdealHeight)
        hdif = abs(clamp(hIdealHeight, h*1/3, h*2/3) - hIdealHeight)
        if hdif <= vdif
            height = clamp(hIdealHeight, h*1/3, h*2/3)
            y = h - height
            (y,[(Rect(0,y,w2,height), mb), (Rect(w2,y,w1,height), top)])
        else
            height = clamp(vIdealHeight, h*1/3, h*2/3)
            h2 = height / (1 + (size(mb)[1]*size(top)[2])/(size(top)[1]*size(mb)[2]))
            h1 = height - h2
            y = h - height
            (y,[(Rect(0,y,w,h2), top), (Rect(0,y+h2,w,h1), mb)])
        end
    end
end

function renderCalc(b, size, s :: CalcState)
    (w,h) = size
    pushRect!(b, Rect(0,0,w,h), Colour(1)) # white background
    sh, buttonspositions = positionButtons(s, size)
    renderStack!(b, s.stack, sh, w, s.currentEntry)
    for (rect, buttons) in buttonspositions
        renderButtons(b, s, rect, buttons, s.highlightedButton)
    end
    # maybe display errors or status
    renderMessage(b, Rect(0,0,w,40), s.statusMessage)
    hint = buttonHint(s.highlightedButton, s)
    if isa(hint, String) && length(hint) > 0
        renderMessage(b, Rect(0,0,w,40), [Message(hint)])
    end
    em = s.errorMessage == nothing ? [] : [s.errorMessage]
    renderMessage(b, Rect(0,0,w,40), em, Colour(0.6,0.1,0.1))
end

function handleEvent(s :: CalcState, size, e) false end
function handleEvent(s :: CalcState, size, e :: MouseEvent)
    handleMouseEvent(s, size, e)
    true
end

function handleMouseEvent(s :: CalcState, size, e :: MouseEvent)
    if isa(e, MouseMoveEvent)
        s.highlightedButton = nothing
    end
    for (r, buttons) in positionButtons(s, size)[2]
        if r.x <= e.x && e.x < r.x + r.w && r.y <= e.y && e.y < r.y + r.h
            subMenu = length(s.buttons) > 1 && buttons == s.buttons[end]
            handleMouseEvent(buttons, s, r, e, subMenu)
        end
    end
end

function handleMouseEvent(bs :: Array{Button, 2}, s, r, e :: MouseEvent, isSubMenu)
    # find the button
    (rows,cols) = size(bs)
    cellw, cellh = r.w / cols, r.h / rows
    row = Int64((e.y - r.y) ÷ cellh)
    col = Int64((e.x - r.x) ÷ cellw)
    button = bs[row + 1, col + 1]
    # check that mouse is within padding
    rect = Rect(col*cellw + r.x + 1, row*cellh + r.y + 1, cellw - 2, cellh - 2)
    if (rect.x <= e.x || (col > 0 && bs[row+1,col] == button)) && 
        (e.x < rect.x + rect.w || (col + 1 < cols && bs[row+1,col+2] == button)) && 
        (rect.y <= e.y || (row > 0 && bs[row,col+1] == button)) && 
        (e.y < rect.y + rect.h || (row + 1 < rows && bs[row+2,col+1] == button))
        handleMouseEvent(button, s, e, isSubMenu)
    end
end

function handleMouseEvent(b :: Button, s, e :: MouseMoveEvent, isSubMenu)
    s.highlightedButton = b
end

function handleMouseEvent(b :: Button1, s, e :: MouseMoveEvent, isSubMenu)
    s.highlightedButton = b
end

function handleMouseEvent(b :: Button, s, e :: LeftMouseButtonPressed, isSubMenu)
    s.pressedButton = b
end

function handleMouseEvent(b :: Button, s, e :: LeftMouseButtonReleased, isSubMenu)
    if s.pressedButton == b
        buttonClicked(s, b)
        if isSubMenu && length(s.buttons) > 1 && shouldCloseSubMenus(s, b)
            s.buttons = s.buttons[1:1] # pop everything
        end
    end
end

function displayError(s :: CalcState, message :: String)
    print("Error: " * message)
    s.errorMessage = Message(message, Dates.Second(3))
end
function displayError(s :: CalcState, e :: Exception)
    buf = IOBuffer()
    showerror(buf, e)
    displayError(s, takebuf_string(buf))
end

function displayMessage(s :: CalcState, message, duration = 500)
    message = Message(message, timeoutms=duration)
    while length(s.statusMessage) > 1 && s.statusMessage[end-1].doneBy <= message.doneBy
        pop!(s.statusMessage)
    end
    if length(s.statusMessage) == 0
        push!(s.statusMessage, message)
    else
        top = s.statusMessage[end]
        if top.doneBy <= message.doneBy
            s.statusMessage[end] = message
        else
            push!(s.statusMessage, message)
        end
    end
end

function clearModifiers(s :: CalcState)
    s.modifiers = Modifiers()
end

function buttonClicked(s :: CalcState, b :: Button1)
    doFullOperation(s, b.op.operation)
end
function buttonClicked(s :: CalcState, b :: DeadButton)
end
