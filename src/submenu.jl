type CloseSubMenuOperation <: Operation end

type SubMenuOperation <: Operation
    layout :: Array{Button,2}
    function SubMenuOperation(layout :: Array{Button,2})
        # check that there is a back-button
        hasBackButton = false
        for button in layout
            if isa(button, Button1)
                if isa(button.op.operation, CloseSubMenuOperation)
                    hasBackButton = true
                    break
                end
            end
        end
        if !hasBackButton
            error("Layout must contain a back button!")
        end
        new(layout)
    end
end

function shouldCloseSubMenus(s :: CalcState, b :: Button)
    if b ∈ s.mainButtons || (length(s.buttons)) > 0 && b ∈ s.buttons[1]
        false
    else
        true
    end
end

# Don't want to interfere with normal prelude/postscript
function doFullOperation(c :: OperationContext, s :: CalcState, op :: CloseSubMenuOperation)
    if length(s.buttons) > 1
        pop!(s.buttons)
    else
        displayError(s, "Nothing to go back to!")
    end
end

function doFullOperation(c :: OperationContext, s :: CalcState, op :: SubMenuOperation)
    push!(s.buttons, op.layout)
end
