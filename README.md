# `calculator.jl`

This is a RPN calculator, written in julia. It has a stack of plain
Julia objects (which know how to draw themselves) and buttons
correspond to julia operations. All calculations are done in terms of
`BigInt`s or `BigFloat`s or their `Complex` variants by default and
precision is currently fixed although it should not be too much work
to make this changeable. A hodgepodge collection of operations are
supported and more are added as I think of them. I aim to use
libraries implementing them rather than doing it myself.

We try to use introspection to determine whether the operation
corresponding to a button may be done however this does not always
work. For example a matching method exists to compute `besselj` with
complex bigfloats however this method throws an error immediately.
There seems to be know good way to speculatively call these existing
methods without having the potential cost (in terms of time) of them
running to completion. We would not want the calculator to attempt to
sum two vectors of 100 million items to determine whether or not they
may be summed.

A sample of operations currently included are

* Entry of integers, decimals, and complex numbers
* +, ×, ─, ÷, x^y, 1/x, absolute value
* sin, cos, tan and all hyperbolic/inverse variants
* cosec, sec, cot and all hyperbolic/invers variants (*)
* constants π, e, γ, φ
* exp, log (natural log), lg (log base 10), 10^x
* square root, cube root, square, cube
* round, floor, ciel
* choose (binomial coefficient), factorial, double factorial
* gcd, lcm
* gamma, beta
* entering vectors
  * on the stack with `[`, `,`, and `]`
  * from the stack using pack (taking _n_ elements from the stack into
    a vector)
  * by repeating an element _n_ times (build)
  * of the first _n_ natural numbers
  * unpacking the elements of a vector onto the stack
* mapping an operation over each element of a vector
* reducing a vector by putting an operation between each element (with
  associativity appropriate to the operator)

The user interface is based
on [WebRender UI](https://github.com/dan-robertson/wrui) with a small
interface to it being in `wr.jl`.
