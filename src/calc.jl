
include("Rewrite.jl")

module Calc

export test

include("stack.jl") # Stack type, push, hasMethod, applyOp
include("se.jl") # s-expression parser for events
include("wr.jl") # primitive rendering
include("rendering.jl")
include("state.jl") # also how to render CalcState
include("operation.jl") # How to do 
include("associativity.jl")
include("entry.jl") # How to enter new values
include("multibutton.jl") # Buttons with hyperbolic / inverse options
include("submenu.jl") # Button reveals a new grid of buttons to click
include("meta-operation.jl") # e.g. map, reduce
include("simplify.jl") # controls semantics of what happens between completing operation and pushing to stack
include("vectors.jl") # vectors that can be built on the stack.
include("symbolic.jl") # algebra via Rewrite.jl
include("symbolic-layout.jl") # drawing fractions, etc. in 2d
include("standardOperations.jl")
include("buttonLayout.jl")

function test()
    setprecision(BigFloat, 64)
    openWindow(CalcState())
end

# end of module
end
