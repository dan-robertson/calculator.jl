abstract Simplification

#maybe we should change things so with NoSimplification, a + b
#generates the expression :(a + b), while AlmostNoSimplification would
#attempt to evaluate it

simplify(simp :: Simplification, x) = x

type NoSimplification <: Simplification end

simplify(simp::NoSimplification, x) = x

type NumericSimplification <: Simplification end

simplify(simp::NumericSimplification, num :: Irrational) = BigFloat(num)
