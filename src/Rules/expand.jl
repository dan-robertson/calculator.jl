

@defrules expand allowImmediateRematch=false

# @defrule expand a*(b+c)            --> a*b + b*c
# @defrule expand (x+y)^(n::Integer) --> begin
#     if n > 0
#         terms = [(binomial(n,k),k,n-k) for k in 0:n]
#         Expr(:call, :+, fromBinom(x,y,terms)...)
#     else
#         :(($x + $y)^$n)
#     end
# end

function fromBinom(x,y,as::Array)
    [fromBinom(x,y,a) for a in as]
end

function fromBinom(x,y,tup::Tuple)
    c,n,m = tup
    if n == 0 && m == 0
        c
    elseif n == 0 && c == 1
        :($y^$m)
    elseif m == 0 && c == 1
        :($x^$n)
    else
        :($c * $x^$n * $y^$m)
    end
end

# we just expand everything in one big go so we don't need to do much matching

@defrule expand (*(a)) --> begin
    # println("expand: $a")
    addve = Rewrite.getAlgebra(Val{:+}())
    mulve = Rewrite.getAlgebra(Val{:*}())
    word = Rewrite.toWord(mulve, a)
    if length(word) == 1 && abs(word[1][2]) == 1
        return a
    end
    coeffI = 1
    coeff = []
    dcoeffI = 1
    dcoeff = []
    numFactors = []
    denFactors = []
    for (fac, pow) in word
        if isa(fac, Expr) && fac.head == :call && Rewrite.getAlgebra(Val{fac.args[1]}()) == addve
            term = Rewrite.toWord(addve, fac)
            if length(term) > 1
                if pow > 0
                    for i = 1:pow
                        push!(numFactors, term)
                    end
                else
                    for i = 1:abs(pow)
                        push!(denFactors, term)
                    end
                end
            else
                push!(coeff, Rewrite.fromWord(addve, term))
            end
        elseif isa(fac, Number)
            if pow > 0
                coeffI *= fac^pow
            else
                dcoeffI *= fac^(-pow)
            end
        else
            push!(pow > 0 ? coeff : dcoeff, Rewrite.fromWord(mulve, [(fac,abs(pow))]))
        end
    end
    # println(string("numFactors: ", numFactors))
    # println(string("denFactors: ", denFactors))
    # println(string("numc: ", coeffI, "; ",  coeff))
    # println(string("denc: ", dcoeffI, "; ", dcoeff))
    if (length(numFactors) == 0 || length(numFactors) == 1 && length(coeff) == 0) &&
        (length(denFactors) == 0 || length(denFactors) == 1 && length(dcoeff) == 0)
        return a
    end
    if length(numFactors) == 0
        num = Expr(:call, :+, coeffI)
    elseif length(numFactors) == 1 && length(coeff) == 0 && coeffI == 1
        num = Rewrite.fromWord(addve, numFactors[1])
    else
        num = distributeProduct(numFactors, coeff, coeffI)
    end
    if length(denFactors) == 0
        den = Expr(:call, :+, coeffI)
    elseif length(denFactors) == 1 && length(dcoeff) == 0 && dcoeffI == 1
        den = Rewrite.fromWord(addve, denFactors[1])
    else
        den = distributeProduct(denFactors, coeff, coeffI)
    end
    # println(string("num: ", num))
    # println(string("den: ", den))
    if length(den.args) == 1 || length(den.args) == 2 && den.args[2] == 1
        if length(num.args) == 2
            num.args[2]
        else
            num
        end
    else
        if length(den.args) == 2
            den = den.args[2]
        end
        if length(num.args) == 2
            Expr(:call, :/, num.args[2], den)
        else
            Expr(:call, :+, (Expr(:call, :/, n, den) for n in num.args[2:end])...)
        end
    end
end

function distributeProduct(factors, coeff=[], numcoeff=1)
    cdict = Dict()
    for c in coeff
        if isa(c, Expr) && c.head == :call && c.args[1] == :^ && isa(c.args[3], Integer)
            cdict[c.args[2]] = get(cdict, c.args[2], 0) + c.args[3]
        else
            cdict[c] = get(cdict, c, 0) + 1
        end
    end
    result = Dict{Dict{Any,Int},Int}(cdict => numcoeff)
    for word in factors
        nr2 = Dict{Dict{Any,Int},Int}()
        for (thing,mult) in word
            for (other,mult2) in result
                prod = copy(other)
                prod[thing] = get(prod, thing, 0) + 1
                mul = mult*mult2
                if mul != 0
                    nm = get(nr2, prod, 0) + mul
                    if nm == 0
                        delete!(nr2, prod)
                    else
                        nr2[prod] = nm
                    end
                end
            end
        end
        result = nr2
    end
    Expr(:call, :+, (length(f) == 0 ? m :
                     length(f) == 1 && m == 1 ? 
                     first((p!=1 ? Expr(:call, :^, x, p) : x for (x,p) in f)) :
                     Expr(:call, :*, (m == 1 ? [] : [m])..., 
                          (p != 1 ? Expr(:call, :^, x, p) : x for (x,p) in f)...)
                     for (f,m) in result)...)
end
