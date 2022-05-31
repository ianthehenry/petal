# Dim

An experimental array programming langauge inspired by [ivy](https://github.com/robpike/ivy) and [J](https://www.jsoftware.com/#/README).

It doesn't work yet and there isn't anything here except a complicated parser in the [`dim_syntax`](dim_syntax/) crate.

Dim syntax is similar to J, with a very terse notation based around "parts of speech" of different identifiers. But there are some important differences:

In Dim, adverbs are prefix instead of postfix. J: `+/` Dim: `fold +`.

Dim "verbs" are not ambivalent. Unary ("monadic") verbs and binary ("dyadic") verbs are distinct parts of speech.

There is an apparent exception for `-`. Depending on context, it is either (binary) subtraction or (unary) negation. So `x -y` is different from `x - y`.

This isn't actually a special operator, though: `-x` is short for `-1x`, and Dim supports scaling by constant factors by juxtaposition. This only works with numeric literals: `2(x + y)` is multiplication, but `c(x + y)` is not. This type of multiplication has higher precedence than any other operations (even conjunction application).

Dim does not support ["stranding"](https://aplwiki.com/wiki/Strand_notation) notation for vectors. Dim notation uses square brackets to delimit arrays of all ranks. This means vectors of one element do not require explicit construction.

| J   | Dim |
| --- | --- |
| `,10` | `[10]` |
| `1 2 3` | `[1 2 3]` |
| `2 3 $ 1 2 3 4 5 6` | `[1 2 3; 4 5 6]` |
| `2 2 2 $ 1 2 3 4 5 6 7 8` | `[1 2; 3 4;; 5 6; 7 8]` |

Elements in an array literal can be any shape, as long as all elements have the same shape. For example, this is a 3x3 matrix:

    x = [1 2 3]
    y = [7 8 9]
    [x [4 5 6] y]

You can use semicolons as a shorthand instead of nested square brackets:

    [1 2 3; 4 5 6] = [[1 2 3] [4 5 6]]

Dim does support tuples. So `(1 "foo" 3)` represents a tuple of three elements. The elements in a tuple do not need to be the same type, unlike arrays. There is no difference between a tuple of one element and the value itself.

Dim allows partial application of binary functions using syntax similar to Haskell's [operator sections](https://wiki.haskell.org/Section_of_an_infix_operator): `double = (* 2)`. (This requires an explicit conjunction in J because of function ambivalence.)

Dim does not support ["hooks" or "forks"](https://www.jsoftware.com/help/jforc/forks_hooks_and_compound_adv.htm) exactly, but it supports "tacit" function composition. For unary functions, this looks like:

    (f g) x = f (g x)

For unary and binary functions, this "precomposes" the function with the left or right operand.

    x (+ f) y = x + (f y)
    x (f +) y = (f x) + y
    x (f + g) y = (f x) + (g y)

Note that the first example is equivalent to J's "dyadic hook." J's "monadic hook" is simply the reflexive version of that, so it can be constructed with the reflex adverb `~`: `~(+ f) x = x + (f x)`. J's "monadic fork" is the reflexive version of the last example, so `x ~(f + g) y = (f x) + (g y)`. There is no simple equivalent of the "dyadic fork." The "monadic noun fork" is identical in J and Dim, as a combination of partial application and implicit composition: `(1 + f) y = 1 + (f y)`. There is no simple equivalent of the "dyadic noun fork."
