
immutable LayoutData{T}
    height::Float32 # size above baseline
    depth::Float32  # size below baseline
    width::Float32
    data::T
end

vsize(x::LayoutData) = x.height + x.depth
vlinesize(x::LayoutData) = max(x.height,0) + max(x.depth,0)

"""render a laid-out item.
(x,y) is the position of the left edge at baseline height.
"""
render(b::DLB, x::Float32, y::Float32, z::LayoutData) = MethodError(render,x,y,z)

function layoutSpace(width,height=0,depth=0)
    LayoutData{Void}(height,depth,width,nothing)
end
function render(::DLB,::Float32,::Float32,::LayoutData{Void}) end

function layoutString(b :: DLB, str)
    shape = shapeText(b, str)
    w,lot = simpleTextLayout(shape)
    LayoutData{LaidOutText}(20,10,w,lot)
end
render(b::DLB,x::Float32,y::Float32,l::LayoutData{LaidOutText}) =
    pushText!(b, Rect(x,y-l.height,l.width,l.height+l.depth),
              Colour(0),l.data)

raiseBox{T}(raise::Float32, l::LayoutData{T}) =
    LayoutData{T}(l.height+raise, l.depth-raise, l.width, l.data)

immutable FractionData
    num :: LayoutData
    den :: LayoutData
end

function layoutFraction(top :: LayoutData, bottom :: LayoutData)
    w = max(top.width, bottom.width,8.0)
    d = FractionData(top, bottom)
    height = vsize(top) + 2
    depth = vsize(bottom) + 2
    LayoutData{FractionData}(height, depth, w, d)
end

function render(b::DLB,x::Float32,y::Float32,l::LayoutData{FractionData})
    xt = (l.width - l.data.num.width) / 2
    xb = (l.width - l.data.den.width) / 2
    render(b,x + xt,y - 2 - l.data.num.depth,l.data.num)
    pushRect!(b, Rect(x+2,y-1,l.width-4,2), Colour(0))
    render(b,x + xb,y + 2 + l.data.den.height,l.data.den)
end

function layoutRow(x::LayoutData, y::LayoutData, z::LayoutData, zs...)
    layoutRow(x, layoutRow(y, z, zs...))
end
function layoutRow(x :: LayoutData, y :: LayoutData)
    function data(x::LayoutData, y::LayoutData)
        LayoutData[x,y]
    end
    function data(x::LayoutData{Array{LayoutData,1}}, y::LayoutData)
        cat(1, x.data, LayoutData[y])
    end
    function data(x::LayoutData, y::LayoutData{Array{LayoutData,1}})
        cat(1, LayoutData[x], y.data)
    end
    function data(x::LayoutData{Array{LayoutData,1}},
                  y::LayoutData{Array{LayoutData,1}})
        cat(1, x.data, y.data)
    end
    w = x.width + y.width
    h = max(x.height, y.height)
    d = max(x.depth, y.depth)
    LayoutData(h,d,w,data(x,y))
end
function render(b::DLB,x::Float32,y::Float32,l::LayoutData{Array{LayoutData,1}})
    for item in l.data
        render(b,x,y,item)
        x = x + item.width
    end
end

function layoutExpr(b, x) # when not an expr we just convert to string by default
    buf = IOBuffer()
    stream = IOContext(buf, compact=true, limit=true)
    show(stream, x)
    line = takebuf_string(buf)
    # hopefully only one line!
    layoutString(b, line)
end

function layoutExpr(b, x::Symbol)
    # TODO: maybe use mathematical italic if single letter
    s = string(x)
    layoutString(b, s)
end

function layoutExpr(b, expr::Expr)
    if expr.head == :macrocall
        if expr.args[1] == :(@big_str) || expr.args[1] == :(@int128_str)
            layoutString(b, expr.args[2])
        else
            layoutString(b, string(expr.args[1]) * "???")
        end
    elseif expr.head == :call
        f = expr.args[1]
        if f == :+ || f == :-
            op = layoutRow(layoutSpace(6),layoutExpr(b,f),layoutSpace(6))
            l = layoutSpace(0,0,0)
            notfirst = false
            for a in expr.args[2:end]
                if notfirst
                    l = layoutRow(l,op)
                end
                l = layoutRow(l,layoutExpr(b,a))
                notfirst = true
            end
            l
        elseif f == :*
            layoutRow((layoutExpr(b,x) for x=expr.args[2:end])...)
        elseif f == :/ || f == ://
            assert(length(expr.args) == 3)
            top = layoutExpr(b,expr.args[2])
            bot = layoutExpr(b,expr.args[3])
            layoutFraction(top,bot)
        elseif f == :^
            assert(length(expr.args) == 3)
            base = layoutExpr(b, expr.args[2])
            expt = layoutExpr(b, expr.args[3])
            raise1 = (2f0/3f0)*base.height
            raise2 = expt.depth + raise1
            raise = raise1 >= expt.depth ? raise1 : raise2
            layoutRow(base, raiseBox(raise,expt))
        else
            l = layoutRow(layoutString(b, string(expr.args[1])),
                          layoutString(b, "("))
            notfirst = false
            for it in expr.args[2:end]
                if notfirst
                    l = layoutRow(l, layoutString(b, ","),
                                  layoutSpace(8))
                end
                l = layoutRow(l, layoutExpr(b, it))
                notfirst = true
            end
            layoutRow(l, layoutString(b,")"))
        end
    else
        error("Don't know what to do with $(expr.head)")
    end
end

function renderStackItem!(b::DLB, item::ReduceWrapper, x, bottom, w)
    # we ignore w
    # which is probably wrong
    layout = layoutExpr(b, item.val);
    bottom = bottom - layout.height - layout.depth
    render(b, Float32(x), Float32(bottom + layout.height), layout)
    bottom
end
