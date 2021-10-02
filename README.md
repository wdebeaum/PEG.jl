[![Build Status](https://travis-ci.org/wdebeaum/PEG.jl.png)](https://travis-ci.org/wdebeaum/PEG.jl)

[![PEG](http://pkg.julialang.org/badges/PEG_0.7.svg)](http://pkg.julialang.org/?pkg=PEG&ver=0.7)

# PEG

Define a
[Parsing Expression Grammar](https://en.wikipedia.org/wiki/Parsing_expression_grammar)
via a macro and abuse of Julia syntax.

* Rules: `@rule name = expression`
* Choice: infix `,`
* Sequence: infix `&`
* Positive lookahead: prefix `+`
* Negative lookahead: prefix `-`
* Option (zero or one time): postfix `[:?]` (≡ `[0:1]`)
  * (note that [?] won't work in Julia >= 1.0 per JuliaLang/julia#22712)
* Zero or more times: postfix `[*]` (≡ `[0:end]`)
* One or more times: postfix `[+]` (≡ `[1:end]`)
* Exactly `m` times: postfix `[m]` (≡ `[m:m]`) (where m is an integer)
* Between `m` and `n` times inclusive: postfix `[m:n]`
* At most `n` times: postfix `[0:n]`
* At least `m` times: postfix `[m:end]`
* Terminals: `r"regex"`, `"string"`
  * Extra regex flags: `p` is for punctuation, and eats whitespace (`\s*`)
    after the match; `w` is for word, and implies `p`, but also makes sure
    match boundaries are word boundaries (`\b`); `h` modifies `p` and `w` to
    eat only horizontal whitespace (`\h`). Values passed to semantics functions
    exclude eaten whitespace.
* Semantics: `expression |> unary_function` (like ParserCombinator)
  * or `expression > nary_function` to interpolate args.
  * Returning the special singleton value `PEG.Failure()` from a semantics
    function causes the parsing expression it's attached to to fail (return
    `nothing` instead of a tuple). Returning `nothing` from a semantics
    function is not special; it just makes the first part of the tuple
    `nothing`. See the parsing function signature below.

Put another way:

```julia
using PEG
@rule grammar = "using PEG\n" & rule[*]
@rule rule = r"@rule"p & nonterminal & r"="p & choice
@rule choice = seq & (r","p & seq)[*]
@rule seq = item & (r"&"p & item)[*] & (r"\|?>"p & julia_function)[:?]
@rule item = lookahead , counted
@rule lookahead = r"\("p & (r"[+-]"p) & seq & r"\)"p
@rule counted = single & (count)[:?]
@rule count = range , r"\["p & (":?" , r"[\*\+]"p) & r"]"p
@rule range = r"\["p & integer & (r":"p & (integer , r"end"w))[:?] & r"]"p
@rule integer = r"\d+"w
@rule single = parens , terminal , nonterminal
@rule parens = r"\("p & choice & r"\)"p
@rule nonterminal = r"\pL\w+"w
@rule terminal = regex , string & r"\s*"
@rule regex = r"\br" & string & r"[himpswx]*\s*"
@rule string = r"\"(\\.|[^\"])*\""
@rule julia_function = # left as an exercise ;)
```

Each rule defines a parsing function with the following signature:

```julia
nonterminal(input::T, cache=PEG.Cache()) where T <: AbstractString
  ::Union{Nothing,Tuple{Any,SubString}}
```

The `Any` part of the return value is the abstract syntax tree, while the
`SubString` is the remaining input after the parsed portion. If parsing fails,
`nothing` is returned.

While you can use rules defined in this way directly, it might be more
convenient to use the functions `parse_next(rule, input; whole=false)` or
`parse_whole(rule, input)`. See their documentation for more information.

Call `PEG.setdebug!()` to have debugging information printed during parsing.
Call `PEG.setdebug!(false)` to turn it off again.

## Versus [ParserCombinator](https://github.com/andrewcooke/ParserCombinator.jl)

PEG...

* is simpler/less featureful. PEG does not:
  * backtrack, except within regexen and to try the next choice (`,`). That is,
    repetition `[]` is always greedy and possessive (to use PCRE terminology).
  * have `Empty(x)`/`@e_str`. Use semantics functions to discard values.
  * have `Dot()`. Use `r"."`.
  * have `Eos()`. Use `parse_whole`.
  * parse streams. Use `open(x->parse_whole(rule, read(x, String)), args...)`.
  * include parsers for two random languages.
* has nicer syntax:
  * Operator precedence makes sense. Tight to loose, the operators are: postfix
    `[]` (whatever is in the brackets), prefix `+`/`-`, infix `&`, `|>`/`>`,
    `,`.
  * PC's `Plus(x)` and `Star(x)` become actual plusses and stars: `x[+]` and
    `x[*]`. And PEG has `x[:?]` too.
  * `Equal`/`@e_str` and `Pattern`/`@p_str` are unneccessary, just use bare
    strings and regexen (with extra flags).
* does not require mutual recursion loops to be broken with `Delayed()`.
* does not have special types for matchers/rules, and does not require a
  trampoline to "interpret" them. They're just plain functions you can call
  directly.

## Versus [StringParserPEG](https://github.com/vonDonnerstein/StringParserPEG.jl)

PEG...

* is about half the size, though they're both pretty small.
* can produce parsers that are much faster and use less memory (see benchmark
  below).
* does not put the whole grammar in a string and force you to deal with
  multiple levels of escaping if the language you're parsing involves
  backslashes or quotes.
* allows regex flags (and adds new ones).
* does not have syntax for suppressing values like `-(rule)`. Use semantics
  functions to discard values.
* has semantics functions that are easier to deal with:
  * They only take the list of children, no other arguments.
  * Positions of children in that list are consistent; a rule like `foo bar[*]
    baz` that matches zero `bar`s will pass child values like `[foo, [], baz]`,
    not `[foo, baz]`.
  * They can return `nothing` as a value without causing the parse to fail.
  * Function expressions are evaluated in the scope of the caller of `@rule`,
    not in the `PEG` module, so you don't have to prefix named functions
    defined outside the grammar rule itself with `Main.` or whatever.
  * The sequence operator `&` binds tighter than the semantics binding
    operators `|>`/`>`, so you don't have to use as many parentheses.

## JSON parsing benchmark

I wrote the same PEG grammar for parsing JSON using both this PEG package and
StringParserPEG. I made no real effort to optimize the grammars. I used
[BenchmarkTools](https://github.com/JuliaCI/BenchmarkTools.jl) to test them
against each other and against the hand-written (and presumably more optimized)
JSON parser from [JSON.jl](https://github.com/JuliaIO/JSON.jl). I used a
10,000-line JSON file from
[Chevrotain](https://github.com/Chevrotain/chevrotain/blob/gh-pages/performance/samples/10K_json.js),
and tested each of the three parsers repeatedly parsing that file (as a string)
for 60 seconds. See [json-benchmark.jl](json-benchmark.jl) for the code I used,
including both the grammars and the benchmarking code. Here are some of the
results:

| parser          | mean parsing time (ms) | memory estimate (MiB) |
| --------------- | ---------------------: | --------------------: |
| JSON            |                  3.262 |                  1.49 |
| PEG             |                849.936 |                 62.53 |
| StringParserPEG |               4635.000 |              11630.00 |

Take all of this with a grain of salt. This isn't a great test because (a)
these grammars don't really exercise some of the more interesting features of
the PEG formalism, and (b) I put a lot less time and effort into these specific
PEG grammars than presumably went into JSON.jl (they're less than 15 lines of
code each!). But it does seem clear that while this PEG package isn't as
efficient as a hand-written, optimized parser, it is much more efficient than
StringParserPEG.

## Migrating from PEG 0.2 to PEG 1.0

PEG 0.2 works with julia 0.6, while PEG 1.0 works with julia 1.0 (and julia
0.7). Julia 1.0 has a number of differences from julia 0.7 that required some
changes to PEG, which will in turn require some minor syntactic changes to any
grammars written with PEG 0.2 if you want to use them with PEG 1.0/julia 1.0.

* change `>>` to `|>`
* change `>>>` to `>`
* change `|` to `,`
  * Note that this also makes it so you don't have to put parens around your
    lambda expressions.
* change `[?]` to `[:?]`

There are some other changes outside the grammar syntax as well:

* If you were doing this to parse a stream as previously suggested:
  * `open(x->parse_whole(rule, readstring(x)), args...)`
* now you should do this instead:
  * `open(x->parse_whole(rule, read(x, String)), args...)`
* change `Void` to `Nothing`
* change `ParseError` to `Meta.ParseError`

Also note that `PEG.Failure` is now an immutable type (`struct`). That
shouldn't really matter because it has no fields, but it is still technically a
visible change; `isbits(PEG.Failure())` is now `true` where before it was
`false`.
