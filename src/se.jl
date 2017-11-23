# s-expression parser

module SExpression

isWhitespace(c) = c == ' ' || c == '\n' || c == '\t'
isTerminating(c) = isWhitespace(c) || c == '(' || c == ')'
isNumeric(c) = '0' <= c <= '9'
isNumericS(c) = isNumeric(c) || c == '+' || c == '-'
isNumericP(c) = isNumeric(c) || c == '.'
isNumericSP(c) = isNumericS(c) || c == '.'

export read
function read(s::AbstractString)
    s = s * " " # too lazy to implement checking for EOS in read
    val, pos = read(s, start(s))
    val
end

function read(m::Module, s::AbstractString)
    val = read(s)
    convert(m, val)
end

function convert(m, s::Symbol)
    try eval(m, s)
    catch e
        s
    end
end
function convert(m, a::Array)
    if length(a) > 0
        es = convert.(m, a)
        es[1](es[2:end]...)
    else
        convert(m, :NIL)
    end
end
convert(m, x) = x

function read(s, pos)
    char,pos2 = next(s,pos)
    if isWhitespace(char)
        read(s, pos2) # skip whitespace
    elseif char == '"'
        readString(s, pos2)
    elseif char == '('
        readList(s, pos2)
    elseif char == ')'
        error("Unexpected ')'")
    else
        readToken(s, pos)
    end
end

function readList(s, pos)
    list = []
    while true
        char, pos2 = next(s, pos)
        if isWhitespace(char)
            pos = pos2
            continue
        elseif char == ')'
            return list, pos2
        else
            item, pos = read(s, pos)
            push!(list, item)
        end
    end
end

function readToken(s, pos)
    char, pos2 = next(s, pos)
    if char == '#'
        char2, pos3 = next(s, pos2)
        if char2 == '\\'
            # read a character
            char, pos4 = next(s, pos3)
            char4, _   = next(s, pos4)
            if isTerminating(char4)
                char, pos4
            else # e.g. #\Newline
                readNamedChar(s, pos3)
            end
        else
            error("Unsupported macro character: #" * String(char2))
        end
    else
        tok = Char[char]
        maybeIntegral = isNumericS(char)
        maybeFloating = isNumericSP(char)
        char, pos3 = next(s, pos2)
        while !isTerminating(char)
            maybeIntegral = maybeIntegral && isNumeric(char)
            maybeFloating = maybeFloating && (isNumericSP(char) || char == 'e' || char == 'E')
            pos2 = pos3
            push!(tok, char)
            char, pos3 = next(s, pos2)
        end
        token = String(tok)
        if maybeIntegral
            try
                r = parse(Int64, token)
                return r, pos2
            end
        end
        if maybeFloating
            try
                r = parse(Float32, token)
                return r, pos2
            end
        end
        return Symbol(uppercase(token)), pos2
    end
end

const namedChars = Dict(
    :NEWLINE => '\n',
    :SPACE => ' ',
    :TAB => '\t',
    :RETURN => '\r'
)

function readNamedChar(s, pos)
    name, pos2 = read(s, pos)
    char = namedChars[name]
    char, pos2
end

function nextEscaped(a,b)
    c, b2 = next(a,b)
    if c == '\\'
        next(a,b2)
    else
        c, b2
    end
end
function readString(s, pos)
    chars = Char[]
    char, pos = nextEscaped(s, pos)
    while char != '"'
        push!(chars, char)
        char, pos = nextEscaped(s, pos)
    end
    String(chars), pos
end

end
