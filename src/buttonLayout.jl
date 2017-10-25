baremodule ButtonLayout

using Base: hcat, vcat, hvcat
using Calc.StandardOperations
using Calc: DigitButtons, Button
import Calc
SO = Calc.StandardOperations

export editOps, standardOps, mainButtons

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

x = Button(Calc.OperationDescription("x", "", Calc.varOp(:x)))

combinatoricsButtons = [choose factorial abs inv round
                        gamma beta floor x pow] # TODO: more of these

extraButtons = [combinatoricsButtons ; vectorButtons ; trigButtons ; firstRowButtons]

end
