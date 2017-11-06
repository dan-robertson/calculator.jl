baremodule ButtonLayout

using Base: hcat, vcat, hvcat, string
using Calc.StandardOperations
using Calc: DigitButtons, Button
import Calc
SO = Calc.StandardOperations

export editOps, standardOps, mainButtons

# Algebra buttons
algebraicButtons = let l(x) = Button(Calc.OperationDescription(string(x), "", Calc.varOp(x))),
                       deadb = Calc.DeadButton(),
                       backb = Button(Calc.OperationDescription("back", "close this submenu",
                                                                Calc.CloseSubMenuOperation()))
    [backb expand trigExpand deadb deadb
     l(:a) l(:b) l(:c) l(:d) l(:e)
     l(:f) l(:g) l(:h) deadb deadb
     l(:i) l(:j) l(:k) l(:m) l(:n)
     l(:p) l(:q) l(:r) l(:s) l(:t)
     l(:u) l(:v) l(:w) deadb deadb
     l(:x) l(:y) l(:z) deadb deadb]
end

algebraButton = Button(Calc.OperationDescription("algebra", "",
                                                 Calc.SubMenuOperation(algebraicButtons)))

# MAIN BUTTONS

editOps = let b = Button(enter, 2, 1)
    [b b DigitButtons.plusi backspace]
end
standardOps = Button[divide ; mult ; subtract ; add]
mainButtons = Button[editOps ; DigitButtons.digits standardOps]

expButtons = [exp log exp10 log10 sqrt]
trigButtons = [exp log sin cos tan]
firstRowButtons = [swap pi sqrt inverse hyperbolic]

vectorButtons = [openbracket comma closebracket pack unpack
                 index map reduce build SO.gcd]

combinatoricsButtons = [choose factorial abs inv round
                        gamma beta floor algebraButton pow] # TODO: more of these

extraButtons = [combinatoricsButtons ; vectorButtons ; trigButtons ; firstRowButtons]



end
