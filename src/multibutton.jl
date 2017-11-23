type Multibutton <: Button
    buttons :: Dict{Modifiers, OperationDescription}
    w :: Int64
    h :: Int64
    Multibutton(op :: OperationDescription) = new(Dict(Modifiers() => op), 1, 1)
    Multibutton(op :: OperationDescription, w :: Integer, h :: Integer) = Button(Multibutton(op),w,h)
    Multibutton(m :: Dict{Modifiers, OperationDescription}, w :: Integer, h :: Integer) = new(m,w,h)
    Multibutton(ops :: (Pair{T, OperationDescription} where {T})...) =
        new(Dict{Modifiers,OperationDescription}(Modifiers(x[1]...) => x[2] for x in ops), 1, 1)
end
type FlagButton{Flag} <: Button
    op :: FlagOperation{Flag}
    name :: String
    w :: Int64
    h :: Int64
end
FlagButton(flag :: Symbol, name :: String, w=1, h=1) = FlagButton{flag}(name, w, h)
function (::Type{FlagButton{Flag}}){Flag}(name :: String, w, h)
    FlagButton{Flag}(FlagOperation{Flag}(), name, w, h)
end

Button(m :: Multibutton, w::Integer, h::Integer) = Multibutton(m.buttons, w, h)
Button(m :: FlagButton, w::Integer, h::Integer) = FlagButton(m.op, w, h)
buttonSize(m :: Multibutton) = (m.w, m.h)
buttonSize(m :: FlagButton) = (m.w, m.h)

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
