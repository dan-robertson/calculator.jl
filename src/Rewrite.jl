module Rewrite

using FunctionalCollections

### This does algebra (in the 18th century sense) by rewrite rules.

### Commentary:

## Rewriting is split into two steps: matching and substituting
## Suppose we have a rule a => b, and we want to rewrite an expression e
## Then matching is the processes of finding a subexpression e' that looks like a inside e,
## and determining what variable substitutions need to happen inside a to unify a with e'.
## substituting is the process of getting from the match to the rewritten expression
##
## Note that in expressions we use / for division, never //
##
## The method for matching is
## match(expression, toMatch, mapping)
## with mapping being a PersistentHashMap{Symbol, Any}
## This should return a list of mappings, each being mapping
## extended by the result of matching the expression if there
## is a match, and throw an error if there are not mappings.
## This does not search subexpressions so match(:(x + (y * z)), :(a + b), mapping) should error.
## Matching generally uses the unify method
## 
## The method for substituting is substitute(substituter, mapping)
## It is somewhat simpler and is happy for substituter to be either a function,
## in which case it is called with the mapping as KW arguments
## or for it to be an Expr in which case any matching symbols are substituted

const Mapping = PersistentHashMap{TypeVar(:S,Symbol,Any), TypeVar(:T,Any)}
const emptyMapping = phmap()
immutable ReallyNotFound end

immutable UnificationError <: Exception
    symbol :: Symbol
    val1; val2 # the two things we failed to unify
end
immutable MatchingError <: Exception
    expression; matcher
end

mapping(x::Symbol,v) = unify(emptyMapping,@Persistent Dict(x => v))

function unify(m1::Mapping,m2::Mapping)
    default = ReallyNotFound()
    result = m1
    for (k,v2) in m2
        v1 = get(m1,k,default)
        if v1 == default
            result = assoc(m1,k,v2)
        elseif v1 == v2 # TODO: a more general equality test
            # result = result
        else
            throw(UnificationError(k, v1, v2))
        end
    end
    result
end

unify(m1::Mapping) = m1
unify(m1, m2) = unify(unify(m1),unify(m2))
unify(m1::Mapping,m2,m3,ms...) = unify(unify(m1,m2),m3,ms...)

unify{T}(m :: Pair{Symbol,T}) = mapping(m[1],m[2])
unify{T}(m :: Tuple{Symbol,T}) = mapping(m[1],m[2])

function match(expression::Expr, matcher, mapping)
    matchExpr(Val{expression.head}(),expression, matcher, mapping)
end

match(x::Symbol, m::Symbol, mapping) = [unify(mapping, m => x)]
function match(x::Symbol, m::Expr, mapping)
    if m.head == :call
        alg = getAlgebra(m.args[1])
        if alg != NoAlgebra()
            return matchAlgebra(alg, x, m, mapping)
        end
    end
    throw(MatchingError(x,m))
end

# matching some kind of object (not Expr/symbol).
match(object::Expr, matcher::Symbol, mapping) = [unify(mapping, matcher=>object)]
match(object, matcher::Symbol, mapping) = [unify(mapping, matcher=>object)]
function match(object, matcher, mapping)
    if object == matcher
        [mapping]
    else
        expr = expressionify(object)
        if expr == object
            throw(MatchingError(object,matcher))
        else
            match(expr, matcher, mapping)
        end
    end
end

# A way of converting objects like 1//2 into expressions like :(1 / 2)
expressionify(x) = x
expressionify(x::Rational) = :( $(x.num) / $(x.den) )
expressionify(x::Complex) = :( $(real(x)) + ($(imag(x)) * im))

function matchMany(exprs, matchers, mapping)
    # TODO: try to pick a good order to do the matching
    mappings = Set([mapping])
    pairs = sort(collect(zip(exprs,matchers)),
                 by=(x->estimateMatchDifficulty(x[2],mapping)), rev=true)
    for (e,m) in pairs
        mappings2 = Set()
        for mapping in mappings
            try
                for mapping2 in match(e,m,mapping)
                    push!(mappings2, mapping2)
                end
            end
        end
        mappings = mappings2
    end
    res = collect(mappings)
    if length(res) == 0
        throw(MatchingError(exprs,matchers))
    end
    res
end

# high values for more difficult matches
# more difficult matches should be done first
# to quickly get rid of bad solutions
function estimateMatchDifficulty(matcher::Expr, mapping)
    d = 0
    for x in drop(matcher.args,1)
        d = d + estimateMatchDifficulty(x, mapping)
    end
    d
end
estimateMatchDifficulty(matcher::Symbol, mapping) = matcher ∈ keys(mapping) ? 500 : 100
estimateMatchDifficulty(matcher, mapping) = 500

function matchExpr(::Val{:macrocall}, expression, matcher, mapping)
    m = args[1]
    if m == :(@int128_str) || m == :(@big_str)
        int = tryparse(BigInt, expression.args[2])
        if isnull(int)
            match(parse(BigFloat, expression.args[2]), matcher, mapping)
        else
            match(get(int), matcher, mapping)
        end
    else
        throw("Don't know how to handle macrocall $(args[1])")
    end
end

# for a general call we dispatch based on what is being called
# e.g. sin(x) is not treated specially but x * y === *(x,y) is.
function matchExpr(::Val{:call}, expression, matcher::Expr, mapping)
    if matcher.head != :call
        throw(MatchingError(expression, matcher))
    end
    alg = if isa(matcher.args[1], Expr)
        if matcher.args[1].head == :vect && length(matcher.args[1].args) == 1 &&
            isa(matcher.args[1].args[1], Symbol)
            NoAlgebra()
        else
            throw(MatchingError(expression, matcher))
        end
    else
        getAlgebra(Val{matcher.args[1]}())
    end
    matchAlgebra(alg, expression, matcher, mapping)
end

abstract Algebra
immutable NoAlgebra <: Algebra end
immutable GroupAlgebra <: Algebra
    identity               # e.g. 0   1    Arity 0
    op :: Symbol           # e.g. +   *    Arity Any
    inverse :: Symbol      # e.g. -   inv  Arity 1
    opinv :: Symbol        # e.g. -   /    Arity 2
    pow   :: Symbol        # e.g. *   ^    Arity 2 (1 in group, 1 in Z)
    matchPow :: Bool
    commutative :: Bool
    canonicaliseType
end
abstract Canonicalisation
immutable AdditionType <: Canonicalisation end
immutable MultiplicationType <: Canonicalisation end

getAlgebra(other) = NoAlgebra()
getAlgebra(::Val{:+}) = GroupAlgebra(0,:+,:-,:-,:*,false,true,AdditionType())
getAlgebra(::Val{:-}) = getAlgebra(Val{:+})
getAlgebra(::Val{:*}) = GroupAlgebra(0,:*,:inv,:/,:^,true,true,MultiplicationType())
getAlgebra(::Val{:/}) = getAlgebra(Val{:*})
getAlgebra(::Val{:inv}) = getAlgebra(Val{:*})


"""
When matching a function call we have either
match(foo(args...), foo(matchers...)) -> may succeed
match(foo(args...), [f](matcher...)) -> may succeed with f => foo
"""
function matchAlgebra(::NoAlgebra, expr::Expr, matcher::Expr, mapping)
    fname = expr.args[1]
    mfun  = matcher.args[1]
    # match function name
    fmapping = mapping
    if isa(mfun, Symbol)
        if fname == mfun
            # nothing to do
        else
            throw(MatchingError(expr, matcher))
        end
    elseif isa(mfun, Expr) && mfun.head == :vect && length(mfun.args) == 1 && isa(mfun.args[1], Symbol)
        fmapping = unify(fmapping, mfun.args[1] => fname)
    end
    if length(expr.args) != length(matcher.args)
        throw(MatchingError(expr, matcher))
    elseif length(expr.args) == 1
        [fmapping]
    else
        matchMany(expr.args[2:end], matcher.args[2:end], fmapping)
    end
end

# convert an expression into a word in a group g
# a word is a list representing a product of elements in g
# it is a list of the form [(elt,pow)...]
function toWord(g::GroupAlgebra, expr)
    if expr == g.identity
        [(g.identity,0)]
    else
        [(expr,1)]
    end
end

function toWord(g::GroupAlgebra, expr::Expr)
    if expr.head != :call
        (expr,1)
    else
        fun = expr.args[1]
        if fun == g.op
            multiplyWords(g, map(x -> toWord(g,x), expr.args[2:end])...)
        elseif fun == g.inverse && length(expr.args) == 2
            invWord(g, toWord(g,expr.args[2]))
        elseif fun == g.opinv && length(expr.args) == 3
            multiplyWords(g, toWord(g, expr.args[2]), invWord(g, toWord(g, expr.args[3])))
        elseif g.matchPow && fun == g.pow && length(expr.args) == 3 &&
               isa(expr.args[3],Integer) && g.commutative
            w = toWord(g, expr.args[2])
            pow = expr.args[3]
            [(x,p*pow) for (x,p) in w]
        else
            [(expr,1)]
        end
    end
end

function multiplyWords(g, words...)
    r = ()
    if g.commutative
        d = Dict()
        for wordy in words
            word = isa(wordy, NTuple{2}) ? [wordy] : wordy
            for (exp,pow) in word
                newPow = pow + get(d,exp,0)
                if newPow != 0
                    push!(d,exp=>newPow)
                else
                    delete!(d,exp)
                end
            end
        end
        r = d
    else
        w = []
        for wordy in words
            word = isa(wordy, NTuple{2}) ? [wordy] : wordy
            for (x,pow) in words
                if pow != 0 && x != g.identity
                    if length(w) > 0 && w[end][1] == x
                        newPow = w[end][2] + pow
                        if newPow == 0
                            pop!(w)
                        else
                            w[end] = (w[end][1],newPow)
                        end
                    else
                        push!(w,(x,pow))
                    end
                end
            end
        end
        r = w
    end
    if length(r) == 0
        [(g.identity,1)]
    else
        collect((a,b) for (a,b) in r)
    end
end

invWord(g,word::NTuple{2}) = [(word[1],-word[2])]
function invWord(g,word)
    if g.commutative
        collect((x,-p) for (x,p) in word)
    else
        collect((x,-p) for (x,p) in reverse(word))
    end
end

fromWord(g::GroupAlgebra,word) = fromWord(g,g.canonicaliseType,word)
function fromWord(g::GroupAlgebra,word::NTuple{2})
    if word[2] == 1
        word[1]
    elseif word[2] == -1
        Expr(:call,g.inverse,word[1])
    else
        Expr(:call,g.pow,word[1],word[2])
    end
end
function fromWord(g::GroupAlgebra,::AdditionType,word)
    if length(word) == 0
        g.identity
    else
        exp = fromWord(g,word[1])
        lastPlus = false
        for (x,pow) in drop(word,1)
            if pow > 0 && lastPlus
                push!(exp.args, pow == 1 ? x : Expr(:call, g.pow, x, pow))
            elseif pow > 0
                exp = Expr(:call, g.op, exp, pow == 1 ? x : Expr(:call, g.pow, x, pow))
                lastPlus = true
            elseif pow < 0
                exp = Expr(:call, g.opinv, exp, pow == -1 ? x : Expr(:call, g.pow, x, -pow))
            end
        end
        exp
    end
end
function fromWord(g::GroupAlgebra,::MultiplicationType,word)
    top = collect((x,pow) for (x,pow) in word if pow > 0)
    bot = collect((x,-pow) for (x,pow) in word if pow < 0)
    xtop = ()
    if length(top) == 0 
        xtop = g.identity
    else
        args = collect(pow == 1 ? x : Expr(:call,g.pow,x,pow) for (x,pow) in top)
        xtop = length(args) == 1 ? args[1] : Expr(:call,g.op,args...)
    end
    xbot = ()
    if length(bot) == 0 
        xtop
    else
        args = collect(pow == 1 ? x : Expr(:call,g.pow,x,pow) for (x,pow) in bot)
        xbot = length(args) == 1 ? args[1] : Expr(:call,g.op,args...)
        Expr(:call,g.opinv,xtop,xbot)
    end
end

function matchAlgebra(g::GroupAlgebra, object, matcher::Expr, mapping)
    if !g.commutative
        throw(MatchingError(object,matcher))
    end
    # object is not something 
    matchWord = toWord(g, matcher)
    # for the sake of sanity we only permit matching against symbols or literals
    literalBits = collect((x,-p) for (x,p) in matchWord if !isa(x,Symbol))
    expr = object
    if length(literalBits) == 0 || length(literalBits) == 1 && 
        (literalBits[1][2] == 0 || literalBits[1][1] == g.identity)
        expr = object
    else
        expr = Expr(:call,g.op,object,fromWord(g,literalBits))
    end
    identmap = unify((x=>g.identity for (x,p) in matchWord if isa(x,Symbol) && abs(p) > 1)...)
    symbolBits = collect((x,p) for (x,p) in matchWord if isa(x,Symbol) && abs(p) == 1)
    map = unify(mapping,identmap)
    mappings = []
    for (x,p) in symbolBits
        try
            res = unify(map,p>0 ? x=>expr : x=>Expr(:call, g.inv, expr),
                        (y=>g.identity for (y,p) in symbolBits if y != x)...)
            push!(res, mappings)
        end
    end
    if length(mappings) == 0
        throw(MatchingError(object,matcher))
    end
    mappings
end

function matchAlgebra(g::GroupAlgebra, expr::Expr, matcher::Expr, mapping)
    # determine if expr is an expression in g
    exword = toWord(g,expr)
    maword = toWord(g,matcher)
    sort!(maword, by=(x->estimateMatchDifficulty(x[1],mapping)+100abs(x[2])), rev=true)
    mappings = Set()
    if !g.commutative
        error("TODO: match noncommutative groups")
    end
    mag_helper_com(g, mappings, Set([mapping]), exword, start(exword),
                   maword, start(maword))
    if length(mappings) == 0
        throw(MatchingError(expr, matcher))
    else
        collect(mappings)
    end
end

mightMatchExtension(g::GroupAlgebra, matching::Symbol) = true
function mightMatchExtension(g::GroupAlgebra, matching::Expr)
    if matching.head == :call
        f = matching.args[1]
        if f == g.op || f == g.inverse || f == g.opinv
            true
        else
            false
        end
    else
        true
    end
end
mightMatchExtension(::GroupAlgebra, ::Any) = false

function mag_helper_com(g::GroupAlgebra, mappings::Set, mappingset, exword_iter, exword_state,
                        maword_iter, maword_state)
    if !done(maword_iter, maword_state)
        (matcher, pow), state2 = next(maword_iter, maword_state)
        if pow == 0
            # skip as doesn't contribute
            mag_helper_com(g, mappings, mappingset, exword_iter, exword_state, maword_iter, state2)
        elseif done(maword_iter, state2)
            # special case: one matcher left, so match it against everything left
            exword = collect(rest(exword_iter, exword_state))
            if length(exword) == 0
                return # We don't match against id yet
                exword = [(g.identity, pow)]
            end
            gcdpow = reduce(gcd, (pow for (x,pow) in exword))
            if mod(gcdpow,pow) == 0 && pow != 1
                rootword = [(x,div(p,pow)) for (x,p) in exword]
                mappings2 = Set()
                workedFor = 0
                for mapping in mappingset
                    try
                        push!(mappings2,match(fromWord(g,rootword), matcher, mapping)...)
                        workedFor = workedFor + 1
                    end
                end
                for mapping2 in mappings2
                    push!(mappings, mapping2)
                end
                if length(mappingset) == workedFor
                    return # It worked so don't need # TODO: o try the other way
                end
            end
            matcher2 = pow == 1 ? matcher : Expr(:call, g.pow, matcher, pow)
            mappings2 = Set()
            for mapping in mappingset
                try
                    push!(mappings2,match(fromWord(g,exword), matcher2, mapping)...)
                end
            end
            for mapping2 in mappings2
                push!(mappings, mapping2)
            end
        else
            mag_helper_com1(g, mappings, mapping, exword_iter, exword_state,
                            maword_iter, state2, matcher, pow, [], [])
        end
    elseif done(exword_iter, exword_state)
        push!(mappings, mapping)
    end
end

function mag_helper_com1(g::GroupAlgebra, mappings::Set, mappingset, exword_iter, exword_state,
                         maword_iter, maword_state, matcher, pow, exword_part, exword_skip)
    if !done(exword_iter, exword_state)
        # match against one letter
        (x, pow2), state2 = next(exword_iter, exword_state)
        exword = [exword_part ; (x,pow2)]
        gcdpow = reduce(gcd, (pow for (x,pow) in exword))
        skipNormalMatch = false
        if mod(gcdpow,pow) == 0 && pow != 1
            # we can match m^pow against exword as m against exword^(1/pow)
            rootword = [(x,div(p,pow)) for (x,p) in exword]
            mappings2 = Set()
            workedFor = 0
            for mapping in mappingset
                try
                    push!(mappings2,match(fromWord(g,rootword), matcher, mapping)...)
                    workedFor = workedFor + 1
                end
            end
            it, st = if length(exword_skip) > 0
                iter = Base.flatten((exword_skip, rest(exword_iter, state2)))
                iter, start(iter)
            else
                exword_iter, state2
            end
            if length(mappings2) > 30
                for mappingset2 in Base.partition(mappings2, 15)
                    mag_helper_com(g, mappings, Set(mappingset2), it, st,
                                   maword_iter, maword_state)
                end
            else
                mag_helper_com(g, mappings, mappings2, it, st,
                               maword_iter, maword_state)
            end
            # if there was a match for everything we can skip the normal attempt
            skipNormalMatch = workedFor == length(mappingset)
        end
        # we try to match exword against m^pow
        if !skipNormalMatch
            matcher2 = pow == 1 ? matcher : Expr(:call, g.pow, matcher, pow)
            mappings2 = Set()
            for mapping in mappingset
                try
                    push!(mappings2,match(fromWord(g,exword), matcher2, mapping)...)
                end
            end
            it, st = if length(exword_skip) > 0
                iter = Base.flatten((exword_skip, rest(exword_iter, state2)))
                iter, start(iter)
            else
                exword_iter, state2
            end
            if length(mappings2) > 30
                for mappingset2 in Base.partition(mappings2, 15)
                    mag_helper_com(g, mappings, Set(mappingset2), it, st,
                                   maword_iter, maword_state)
                end
            else
                mag_helper_com(g, mappings, mappings2, it, st,
                               maword_iter, maword_state)
            end
        end
        # we try extending the current word
        if mightMatchExtension(g, matcher)
            mag_helper_com1(g, mappings, mappingset, exword_iter, state2,
                            maword_iter, maword_state, matcher, pow, exword, exword_skip)
        end
        # we try skipping the current letter
        mag_helper_com1(g, mappings, mappingset, exword_iter, state2, maword_iter, maword_state,
                        matcher, pow, exword_part, [exword_skip ; (x,pow)])
        # TODO: if abs(pow) > 1 try splitting it up. Should only do this if abs(pow) is small though
    else
        # Done
        # Todo: maybe try matching against identity (if length(exword_part) == 0)
    end
end




# Substituting function

function substitute(substituter::Function, mapping::Mapping)
    substituter(;mapping...)
end

function substitute(substituter, mapping::Mapping)
    substituter
end

substitute(substituter::Symbol, mapping::Mapping) = get(mapping, substituter, substituter)
function substitute(substituter::Expr, mapping::Mapping)
    Expr(substituter.head,
         (isa(arg,Union{Symbol,Expr}) ? substitute(arg,mapping) : arg
          for arg in substituter.args)...)
end

# Rewriting function

export rewrite
"""
    rewrite(expr, matcher, sub; limit=20, allowImmediateRecursion=true)

search through expr for something that matches matcher and when it is
found, replace it by substituting sub. limit is the maximum number of
times rewrite may recur into/on a single subexpression. if
allowImmediateRecursion is set to false then rewrite cannot rewrite
the same subexpression twice, so for exmple the rule [f](x,y) -> f(y,x)
can only be applied once foo(x,y) ~> foo(y,x) instead of repeatedly
until the limit is reaced.
"""

function rewrite(expr, matcher, sub; limit=20, allowImmediateRecursion=true)
    _rewrite(expr, matcher, sub, limit, 0, allowImmediateRecursion)
end

function _rewrite(expr, matcher, sub, limit, n, allowImmediateRecursion)
    if n >= limit
        return expr
    end
    ms = ()
    try
        m = match(expr, matcher, emptyMapping)
        if length(m) > 0
            ms = m[1]
        end
    end
    if ms != ()
        expr2 = substitute(sub, ms)
        n = n + 1
        if allowImmediateRecursion
            return _rewrite(expr2, matcher, sub, limit, n, allowImmediateRecursion)
        else
            expr = expr2
        end
    end
    # recurse
    if isa(expr, Expr) && expr.head == :call
        rewritten = [i == 1 ? arg : _rewrite(arg, matcher, sub, limit, n, allowImmediateRecursion)
                     for (i,arg) in enumerate(expr.args)]
        Expr(:call, rewritten...)
    else
        expr
    end
end

# End of Module1
end
