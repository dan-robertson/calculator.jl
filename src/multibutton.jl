type Multibutton <: Button
    buttons :: Dict{Modifiers, OperationDescription}
    w :: Int64
    h :: Int64
    char :: Union{Void, Char}
    Multibutton(op :: OperationDescription) = new(Dict(Modifiers() => op), 1, 1,nothing)
    Multibutton(op :: OperationDescription, w :: Integer, h :: Integer, char=nothing) = Button(Multibutton(op),w,h,char)
    Multibutton(m :: Dict{Modifiers, OperationDescription}, w :: Integer, h :: Integer) = new(m,w,h,nothing)
    Multibutton(ops :: (Pair{T, OperationDescription} where {T})...) =
        new(Dict{Modifiers,OperationDescription}(Modifiers(x[1]...) => x[2] for x in ops), 1, 1, nothing)
    Multibutton(b :: Multibutton, char :: Char) = new(b.buttons, b.w, b.h, char)
end
type FlagButton{Flag} <: Button
    op :: FlagOperation{Flag}
    name :: String
    w :: Int64
    h :: Int64
    char :: Union{Void, Char}
end
FlagButton(flag :: Symbol, name :: String, w=1, h=1, char=nothing) = FlagButton{flag}(name, w, h, char)
FlagButton(flag :: Symbol, name :: String, char :: Char) = FlagButton{flag}(name, 1, 1, char)
function (::Type{FlagButton{Flag}}){Flag}(name :: String, w, h, char)
    FlagButton{Flag}(FlagOperation{Flag}(), name, w, h, char)
end

Button(m :: Multibutton, w::Integer, h::Integer) = Multibutton(m.buttons, w, h, m.char)
Button(m :: Multibutton, char :: Char) = Multibutton(m, char)
buttonSize(m :: Multibutton) = (m.w, m.h)
buttonSize(m :: FlagButton) = (m.w, m.h)

buttonChar(m :: Multibutton) = m.char
buttonChar(m :: FlagButton) = m.char

function multiChoose(button :: Multibutton, modifiers :: Modifiers)
    modifiers ∈ keys(button.buttons) ? button.buttons[modifiers] : DeadButton()
end


function renderButton(b, state, button::Multibutton, r, highlighted)
    toDraw = multiChoose(button, state.modifiers)
    renderButton(b, state, toDraw, r, highlighted)
end

function buttonClicked(s :: CalcState, b :: Multibutton)
    doFullOperation(s, multiChoose(b, s.modifiers))
end

buttonHint(b :: Multibutton, s) = buttonHint(multiChoose(b, s.modifiers), s)

function buttonClicked{Flag}(s :: CalcState, b :: FlagButton{Flag})
    doFullOperation(DefaultContext(), s, b.op)
end

function renderButton{Flag}(b, state, button :: FlagButton{Flag}, r, highlighted)
    if Flag ∈ state.modifiers
        if highlighted
            c = Colour(0.5)
        else
            c = Colour(0.6)
        end
    else
        if highlighted
            c = Colour(0.8)
        else
            c = Colour(0.93)
        end
    end
    renderButton(b, state, button.name, r, c)
end
