module Rewrite

using FunctionalCollections
using Base.Iterators

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

#const Mapping = PersistentHashMap{TypeVar(:S,Symbol,Any), TypeVar(:T,Any)}
const Mapping = PersistentHashMap{S,T} where {S<:Symbol,T}
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
    if isa(matcher, Expr) && matcher.head == :(::) 
        # println("$object :: $(matcher.args[2]) ? ")
        if isa(object, matcher.args[2])
            return match(object, matcher.args[1], mapping)
        end
    end
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
expressionify(x::Integer) = x < 0 ? :(-$(-x)) : x

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
    (d * 105) ÷ 100
end
estimateMatchDifficulty(matcher::Symbol, mapping) = matcher ∈ keys(mapping) ? 500 : 100
estimateMatchDifficulty(matcher, mapping) = 500
estimateMatchDifficulty(matcher::Type, mapping) = 750

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
getAlgebra(::Val{:-}) = getAlgebra(Val{:+}())
getAlgebra(::Val{:*}) = GroupAlgebra(1,:*,:inv,:/,:^,true,true,MultiplicationType())
getAlgebra(::Val{:/}) = getAlgebra(Val{:*}())
getAlgebra(::Val{:inv}) = getAlgebra(Val{:*}())


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
            word = isa(wordy, Tuple{T,S} where {T,S}) ? [wordy] : wordy
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
            word = isa(wordy, Tuple{T,S} where {T,S}) ? [wordy] : wordy
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

invWord(g,word::Tuple{T,S} where {T,S}) = [(word[1],-word[2])]
function invWord(g,word)
    if g.commutative
        collect((x,-p) for (x,p) in word)
    else
        collect((x,-p) for (x,p) in reverse(word))
    end
end

fromWord(g::GroupAlgebra,word) = fromWord(g,g.canonicaliseType,word)
function fromWord(g::GroupAlgebra,word::Tuple{T1,T2} where {T1,T2})
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
    top = Tuple{Any,Int}[]
    bot = Tuple{Any,Int}[]
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

# TODO: not even sure this ever gets called
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
            res = unify(map,p>0 ? x=>expr : x=>Expr(:call, g.inverse, expr),
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
    # println("matchAlgebra: $expr, $matcher")
    # determine if expr is an expression in g, otherwise we don't bother looking for a match
    if expr.head != :call
        throw(MatchingError(expr, matcher))
    else
        f = expr.args[1]
        if !(f == g.op || f == g.inverse || f == g.opinv || f == g.pow)
            throw(MatchingError(expr, matcher))
        end
    end
    exword = toWord(g,expr)
    maword = toWord(g,matcher)
    if length(maword) == 1 && maword[1][2] == -1 # matching -foo
        if length(exword) == 1 && (exword[1][2] == -1 || isa(exword[1][1], Number) && exword[1][1] < 0)
            # this is ok. Carry on
            # TODO: do this faster here
        elseif all((x,p)->p<0, exword)
            # this is ok too. Carry on
        else
            throw(MatchingError(expr, matcher))
        end
    end
    # println("exword: $exword")
    # println("maword: $maword")
    sort!(maword, by=(x->estimateMatchDifficulty(x[1],mapping)+100abs(x[2])), rev=true)
    # println("maword: $maword")
    mappings = Set()
    if !g.commutative
        error("TODO: match noncommutative groups")
    end
    mag_helper_com(g, mappings, Set([mapping]), exword, start(exword),
                   maword, start(maword))
    if length(mappings) == 0
        throw(MatchingError(expr, matcher))
    else
        # println("got: $mappings")
        collect(mappings)
    end
end

mightMatchExtension(::GroupAlgebra, ::Symbol) = true
function mightMatchExtension(g::GroupAlgebra, matching::Expr)
    if matching.head == :call
        f = matching.args[1]
        if f == g.op || f == g.inverse || f == g.opinv
            true
        else
            false
        end
    elseif matching.head == :(::)
        matching.args[2] == Expr
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
                        # println("matching $(fromWord(g,rootword)), $matcher")
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
                    # println("matching $(fromWord(g,exword)), $matcher")
                    push!(mappings2,match(fromWord(g,exword), matcher2, mapping)...)
                end
            end
            for mapping2 in mappings2
                push!(mappings, mapping2)
            end
        else
            mag_helper_com1(g, mappings, mappingset, exword_iter, exword_state,
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
                    # println("matching $(fromWord(g,rootword)), $matcher")
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
                    # println("matching $(fromWord(g,exword)), $matcher")
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
                        matcher, pow, exword_part, [exword_skip ; (x,pow2)])
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

export Rule
immutable Rule
    match
    sub
end


export rewrite
"""
    rewrite(expr, matcher, sub;   limit=20, allowImmediateRematch=true)
    rewrite(expr, rule::Rule;     limit=20, allowImmediateRematch=true)
    rewrite(expr, rules::Rule...; limit=20, allowImmediateRematch=true, onMatch=:allRules)

search through expr for something that matches matcher and when it is
found, replace it by substituting sub. limit is the maximum number of
times rewrite may recur into/on a single subexpression. if
allowImmediateRematch is set to false then rewrite cannot rewrite
the same subexpression twice, so for exmple the rule [f](x,y) -> f(y,x)
can only be applied once foo(x,y) ~> foo(y,x) instead of repeatedly
until the limit is reaced.

If multiple rules are given then onMatch can be either :allRules or :deeper.
If it is :deeper then all rules are tried on subexpressions of the
result of applying a rule before the other rules are tried on the
whole expression.
If it is :allRules then all rules are tried on the whole expression
and then they are tried on the subexpressions.
"""
function rewrite(expr, rules::Rule...; limit=20, allowImmediateRematch=true, onMatch=:allRules)
    rewrite(expr, collect(Rule, rules),
            limit=limit, allowImmediateRematch=allowImmediateRematch, onMatch=onMatch)
end

function rewrite(expr, rules::Array{Rule,1}; limit=20, allowImmediateRematch=true, onMatch=:allRules)
    _rewrite(expr, rules, 1, limit, 0, allowImmediateRematch, onMatch, NoAlgebra())
end

function rewrite(expr, matcher, sub; kvs...)
    rewrite(expr, Rule(matcher, sub); kvs...)
end

shouldTryMatch(x, matcher) = true
function shouldTryMatch(g::GroupAlgebra, matcher::Expr)
    if matcher.head == :call
        alg = getAlgebra(Val{matcher.args[1]}()) 
        g != alg
    else
        true
    end
end

# returns canonicalised expr, and bool specifying whether changed
function canonicalise(expr, enclosingAlg)
    # nothing to do
    expr, false
end

function canonicalise(expr::Expr, enclosingAlg)
    if expr.head == :call
        alg = getAlgebra(Val{expr.args[1]}())
        canons = Tuple{Any,Bool}[canonicalise(arg,alg) for arg in expr.args]
        if alg == NoAlgebra() || alg == enclosingAlg
            # default behaviour
            if any(x->x[2], canons)
                Expr(:call, (x for (x,b) in canons)...), true
            else
                expr, false
            end
        else
            expr2 = if any(x->x[2], canons)
                Expr(:call, (x for (x,b) in canons)...)
            else
                expr
            end
            word = toWord(alg, expr2)
            fromWord(alg, word), true
        end
    else
        canons = Tuple{Any,Bool}[canonicalise(arg,alg) for arg in expr.args]
        if any(x->x[2], canons)
            Expr(expr.head, (x for (x,b) in canons)...), true
        else
            expr, false
        end
    end
end

# onMatch should be :deeper => [try to match self again], do all rules on
#                              depper subexprs, then try rules on result
#                   :allRules => [try to match self again], try to match all rules,
#                                try rules on deeper subexprs
function _rewrite(expr, rules, rulestart, limit, n, allowImmediateRematch, onMatch, inAlg)
    # println("_rewrite($expr, $rules, $rulestart, $limit, $n, $allowImmediateRematch, $onMatch, $inAlg)")
    if n >= limit
        return expr
    end
    orulestart = rulestart
    ms = nothing
    for (rulenum, rule) in rest(enumerate(rules), (rulestart, rulestart))
        if !shouldTryMatch(inAlg, rule.match)
            continue
        end
        try
            m = match(expr, rule.match, emptyMapping)
            if length(m) > 0
                ms = (rule.sub, m[1])
                rulestart = rulenum
                break
            end
        end
    end
    if ms != nothing
        expr2, ignore = canonicalise(substitute(ms...), inAlg)
        n = n + 1
        if allowImmediateRematch
            n2 = n + 1
            rule = rules[rulestart]
            pm = ms[2]
            try
                while n2 < limit
                    m = match(expr2, rule.match, emptyMapping)
                    if length(m) > 0
                        if m[1] == pm
                            break
                        end
                        expr2, ignore = canonicalise(substitute(rule.sub, m[1]), inAlg)
                        pm = m[1]
                    else
                        break
                    end
                    n2 = n2 + 1
                end
            end
        end
        expr = expr2
    end
    if onMatch == :allRules && rulestart <= length(rules)
        # try other rules
        expr = _rewrite(expr, rules, rulestart + 1, limit, n, allowImmediateRematch, onMatch, inAlg)
    end
    if orulestart == 1
        # recurse
        expr = if isa(expr, Expr) && expr.head == :call
            alg = getAlgebra(Val{expr.args[1]}())
            rewritten = [i == 1 ? arg :
                         _rewrite(arg, rules, 1, limit, n, allowImmediateRematch, onMatch, alg)
                         for (i,arg) in enumerate(expr.args)]
            canonicalise(Expr(:call, rewritten...), inAlg)[1]
        else
            expr
        end
    end
    if onMatch == :deeper && rulestart != orulestart && rulestart <= length(rules)
        expr = _rewrite(expr, rules, rulestart + 1, limit, n, allowImmediateRematch, onMatch, inAlg)
    end
    expr
end


module Rules

import Rewrite
using Rewrite: Rule, rewrite

"""
    @rule matcher --> substitute

matcher should follow the normal matcher syntax. substitute is interpreted as:

If substitute contains no expressions with \$ (e.g. \$x, \$(foo(bar,baz)))
then it is passed to substitute as an expression.
If it contains an expression with \$ then it it gets converted into the
function returning exactly that expression with variables from the match
substitued.
If it begins with begin (i.e. a block) then it is interpreted as the body
of the function to do the substitution
"""
macro rule(expr::Expr)
    if expr.head != :-->
        error("invalid rule syntax, --> in wrong place")
    end
    matcher = expr.args[1]
    evalTypes(matcher)
    substituter = expr.args[2]
    evalTypes(substituter)
    if (isa(substituter, Expr) && substituter.head == :block) || hasDollarSigns(substituter)
        # make a plain function
        symbols = Set()
        collectSymbols(matcher, symbols)
        substituter = isa(substituter, Expr) && substituter.head == :block ? 
            Expr(:quote, Expr(:$, substituter)) : # seems to stop weird hygene issues
            Expr(:quote, substituter)
        subexpr = Expr(:->,
                       Expr(:tuple,
                            Expr(:parameters,
                                 (Expr(:kw,var,:nothing) for var in symbols)...)),
                       substituter)
        substituter = subexpr
    else
        substituter = Expr(:quote, substituter)
    end
    r = Expr(:call, :Rule,
         Expr(:quote, matcher),
         substituter)
    # println(r)
    r
end

"""
    @defrule ruleset matcher --> substitute

Like @rule but saves the rule into ruleset
"""
macro defrule(set, rule)
    :(push!($(Symbol(set, :_rules)), @rule $rule))
end

macro defrule(foo)
    dump(foo)
    error("Not enough arguments to @defrule")
end

"""
    @defrules name

Define a new set of rules (by giving a variable name)
"""
macro defrules(name, kvs...)
    fname = esc(name)
    kvsym = gensym("kvs")
    exprsym = gensym("expr")
    rulesname = esc(Symbol(name, :_rules))
    fdef = Expr(:call, fname,
                Expr(:parameters, 
                     (begin @assert isa(kv,Expr) && (kv.head == :kw || kv.head == :(=))
                      Expr(:kw, kv.args[1], kv.args[2])
                      end for kv in kvs)...,
                     Expr(:(...), kvsym)),
                exprsym)
    fcall = Expr(:call, esc(:rewrite),
                 Expr(:parameters,
                      (Expr(:kw,kv.args[1],kv.args[1]) for kv in kvs)...,
                      Expr(:(...), kvsym)),
                 exprsym,
                 rulesname)
    rulefun = Expr(:(=), fdef, fcall)
    quote
        $rulesname = Rule[]
        export $fname
        $rulefun
    end
end

collectSymbols(s::Symbol, set) = push!(set, s)
collectSymbols(::Any, ::Any) = nothing
function collectSymbols(x::Expr, set)
    if x.head == :call
        if isa(x.args[1], Expr) && x.args[1].head == :vect &&
            length(x.args[1].args) == 1
            collectSymbols(x.args[1].args[1], set)
        end
        for y in rest(x.args,2)
            collectSymbols(y, set)
        end
    elseif x.head == :(::)
        collectSymbols(x.args[1], set)
    end
end

hasDollarSigns(x) = false
hasDollarSigns(x::Expr) = x.head == :$ || any(hasDollarSigns, x.args)

evalTypes(x) = nothing
function evalTypes(x::Expr)
    if x.head == :(::)
        x.args[2] = eval(x.args[2])
        if x.args[1] isa Expr && x.args[1].head == :globalref
            x.args[1] = x.args[1].args[2]
        else
            evalTypes(x.args[1])
        end
    else
        for (i,a) in enumerate(x.args)
            if a isa Expr && a.head == :globalref
                a = a.args[2]
                x.args[i] = a
            end
            evalTypes(a)
        end
    end
end

include("Rules/canonicalise.jl")
include("Rules/numeric.jl")
include("Rules/expand.jl")
include("Rules/trigExpand.jl")

# End Module Rules 
end

# End of Module
end
