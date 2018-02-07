immutable Digit
    digit :: Char
end
immutable DecimalPoint end
immutable Negation end
immutable Backspace end # when used and an empty string would be given, initial is returned

immutable ComplexPartSwitch end # switches from realpart to imagpart

abstract type Entry end
abstract type RealEntry <: Entry end

immutable IntegerEntry <: RealEntry
    digits :: String
end

immutable FloatEntry <: RealEntry
    integerPart :: IntegerEntry
    fractionalPart :: IntegerEntry
end

immutable ComplexEntry{T} <: Entry # T should be :real or :imaginary, indicating which part is being entered
    realPart :: RealEntry
    imagPart :: RealEntry
end

function iszero(x :: IntegerEntry)
    match(r"^[+-]?0*$", x.digits) != nothing
end

function iszero(x :: FloatEntry)
    iszero(x.integerPart) && iszero(x.fractionalPart)
end

Base.sign(x :: IntegerEntry) = iszero(x) ? 0 : (length(x.digits) > 0 && x.digits[1] == '-') ? -1 : 1
Base.sign(x :: FloatEntry) = sign(x.integerPart)

Base.string(item :: IntegerEntry; showsign = true) = showsign ? item.digits : sign(item) == -1 ? item.digits[2:end] : item.digits
Base.string(item :: FloatEntry; showsign = true) = string(item.integerPart, showsign=showsign) * "." * string(item.fractionalPart, showsign=false)

morallyNothing(x :: IntegerEntry) = x.digits == ""
morallyNothing(x :: FloatEntry) = false
morallyNothing(x :: ComplexEntry) = false

function renderStackItem!(b :: DLB, item :: RealEntry, x, bottom, w)
    y = bottom-30
    pushText!(b, Rect(x,y,w,30), getFont(b,font), Colour(0), (0,20), string(item))
    y
end
function renderStackItem!{T}(b :: DLB, item :: ComplexEntry{T}, x, bottom, w)
    # this is a bit horrid
    y = bottom-30
    unfocusedColour = Colour(0.4)
    realCol, imagCol = unfocusedColour, unfocusedColour
    if T == :real
        realCol = Colour(0)
    elseif T == :imaginary
        imagCol = Colour(0)
    else
        error("not right : $(typeof(item))")
    end
    if morallyNothing(item.realPart) && T == :imaginary
        w2,lot2 = simpleTextLayout(shapeText(getFont(b,font), string(item.imagPart)))
        w3,lot3 = simpleTextLayout(shapeText(getFont(b,font), "i"))
        pushText!(b, Rect(x,y,min(w2,w),30), imagCol, lot2)
        pushText!(b, Rect(x+w2,y,min(w2+w3,w)-w2,30), unfocusedColour, lot3)
    else
        s = sign(item.imagPart) == -1 ? '−' : '+'
        w1,lot1 = simpleTextLayout(shapeText(getFont(b,font), string(item.realPart)))
        w2,lot2 = simpleTextLayout(shapeText(getFont(b,font), " $s "))
        w3,lot3 = simpleTextLayout(shapeText(getFont(b,font), iszero(item.imagPart) && T == :real ? "0" : string(item.imagPart; showsign=false)))
        w4,lot4 = simpleTextLayout(shapeText(getFont(b,font), "i"))
        if w1 + w2 + w3 + w4 > w && w1 > 20
            y = y-30
            pushText!(b, Rect(x,y,min(w1,w),30), realCol, lot1)
            if w1 + w2 > w
                pushText!(b, Rect(x,y+30,min(w2,w),30), unfocusedColour, lot2)
                x = x + w2
                w = w - w2
            else
                pushText!(b, Rect(x+w1,y,min(w1+w2,w)-w1,30), unfocusedColour, lot2)
            end
            pushText!(b, Rect(x,y+30,min(w3,w),30), imagCol, lot3)
            pushText!(b, Rect(x+w3,y+30,min(w3+w4,w)-w3,30), unfocusedColour, lot4)
        else
            pushText!(b, Rect(x,y,min(w1,w),30), realCol, lot1); x+=w1; w-=w1;
            pushText!(b, Rect(x,y,min(w2,w),30), unfocusedColour, lot2); x+=w2; w-=w2;
            pushText!(b, Rect(x,y,min(w3,w),30), imagCol, lot3); x+=w3; w-=w3;
            pushText!(b, Rect(x,y,min(w4,w),30), unfocusedColour, lot4)
        end
    end
    y
end

function doEntry(e::Void, initial, extend)
    initial
end

function doEntry(e::Void, initial::Digit, extend)
    IntegerEntry(string(initial.digit))
end

function doEntry(e::Void, initial::Negation, extend)
    IntegerEntry("-")
end

function doEntry(e::String, initial, extend :: String)
    e * extend
end

function doEntry(e::Union{IntegerEntry, FloatEntry}, initial, extend :: String)
    if length(extend) == 1 && extend[1] ∈ "0123456789"
        doEntry(e, initial, Digit(extend[1]))
    elseif extend == "."
        doEntry(e, initial, DecimalPoint())
    else
        e
    end
end

function doEntry(e::IntegerEntry, initial, d :: Digit)
    IntegerEntry(e.digits * string(d.digit))
end

function doEntry(e::IntegerEntry, initial, d :: DecimalPoint)
    FloatEntry(IntegerEntry(e.digits == "-" ? "-0" : e.digits == "" ? "0" : e.digits), IntegerEntry(""))
end

function doEntry(e::IntegerEntry, initial, d :: Negation)
    IntegerEntry(e.digits[1] == '-' ? e.digits[2:end] : "-" * e.digits)
end

function doEntry(e::IntegerEntry, initial, d :: Backspace)
    if e.digits == ""
        nothing
    elseif length(e.digits) == 1
        initial
    else
        IntegerEntry(e.digits[1:end-1])
    end
end

function doEntry(e::FloatEntry, initial, d :: Digit)
    FloatEntry(e.integerPart, doEntry(e.fractionalPart, initial, d))
end

function doEntry(e::FloatEntry, initial, d :: Negation)
    FloatEntry(doEntry(e.integerPart, initial, d), e.fractionalPart)
end

function doEntry(e::FloatEntry, initial, d :: Backspace)
    fp = doEntry(e.fractionalPart, IntegerEntry(""), Backspace())
    if fp == nothing
        e.integerPart
    else
        FloatEntry(e.integerPart, fp)
    end
end

function doEntry(e::FloatEntry, initial, d :: DecimalPoint)
    e
end

function doEntry(e::RealEntry, initial, d :: ComplexPartSwitch)
    if morallyNothing(e)
        ComplexEntry{:imaginary}(IntegerEntry(""), e)
    else
        ComplexEntry{:imaginary}(e, IntegerEntry(""))
    end
end

function doEntry(e::ComplexEntry{:real}, initial, d :: ComplexPartSwitch) 
    ComplexEntry{:imaginary}(e.realPart, e.imagPart)
end
function doEntry(e::ComplexEntry{:imaginary}, initial, d :: ComplexPartSwitch) 
    ComplexEntry{:real}(e.realPart, e.imagPart)
end

function doEntry(e::ComplexEntry{:real}, initial, d)
    ComplexEntry{:real}(doEntry(e.realPart, initial, d), e.imagPart)
end
function doEntry(e::ComplexEntry{:imaginary}, initial, d)
    ComplexEntry{:imaginary}(e.realPart, doEntry(e.imagPart, initial, d))
end
function doEntry(e::ComplexEntry{:real}, inital, d :: Backspace)
    nr = doEntry(e.realPart, IntegerEntry(""), d)
    if nr == nothing
        if morallyNothing(e.imagPart)
            initial
        else
            ComplexEntry{:imaginary}(IntegerEntry(""), e.imagPart)
        end
    else
        ComplexEntry{:real}(nr, e.imagPart)
    end
end
function doEntry(e::ComplexEntry{:imaginary}, initial, d :: Backspace)
    ni = doEntry(e.imagPart, IntegerEntry(""), d)
    if ni == nothing
        if morallyNothing(e.realPart)
            initial
        else
            e.realPart
        end
    else
        ComplexEntry{:imaginary}(e.realPart, ni)
    end
end

function commitInput(s :: CalcState)
    if s.currentEntry != nothing
        newval = convertEntry(s.currentEntry)
        s.currentEntry = nothing
        if newval != nothing
            s.stack = push(s.stack, newval)
        end
    end
end

function convertEntry(e :: String)
    if '.' ∈ e
        parse(BigFloat, e)
        # parse(Float64, e)
    else
        parse(BigInt, e)
    end
end

function convertEntry(e :: IntegerEntry)
    if e.digits == "-" || e.digits == ""
        nothing
    else
        parse(BigInt, e.digits)
    end
end

function convertEntry(e :: FloatEntry)
    parse(BigFloat, e.integerPart.digits * "." * e.fractionalPart.digits)
end

function convertEntry(e :: ComplexEntry)
    r,c = convertEntry(e.realPart), convertEntry(e.imagPart)
    if r == nothing
        r = 0
    end
    if c == nothing
        c = 0
    end
    Complex(promote(r,c)...)
end

module DigitButtons
using Calc: Button, Digit, EntryOperation, OperationDescription, DecimalPoint
using Calc: FloatEntry, IntegerEntry, NegationOperation, MakeComplexOperation
export digits, negate, plusi
b(d) = Button(OperationDescription(d, "", EntryOperation(Digit(d[1]))),d[1])
b0 = Button(b("0"))
b1 = b("1")
b2 = b("2")
b3 = b("3")
b4 = b("4")
b5 = b("5")
b6 = b("6")
b7 = b("7")
b8 = b("8")
b9 = b("9")
bdot = Button(OperationDescription(".", "",
                                   EntryOperation(FloatEntry(IntegerEntry("0"),IntegerEntry("")),
                                                  DecimalPoint())), '.')
negate = Button(OperationDescription("+/-", "negate", NegationOperation()), 'n')
plusi = Button(OperationDescription("+i", "make a complex number from real and imaginary parts",
                                    MakeComplexOperation()), 'i')

digits = [b7 b8 b9
          b4 b5 b6
          b1 b2 b3
          negate b0 bdot]
end
