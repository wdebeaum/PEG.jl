# PEG

Define a
[Parsing Expression Grammar](https://en.wikipedia.org/wiki/Parsing_expression_grammar)
via a macro and abuse of Julia syntax.

* Rules: `@rule name = expression`
* Choice: infix `|`
* Sequence: infix `&`
* Positive lookahead: prefix `+`
* Negative lookahead: prefix `-`
* Option (zero or one time): postfix `[?]` (≡ `[0:1]`)
* Zero or more times: postfix `[*]` (≡ `[0:end]`)
* One or more times: postfix `[+]` (≡ `[1:end]`)
* Exactly `m` times: postfix `[m]` (≡ `[m:m]`) (where m is an integer)
* Between `m` and `n` times inclusive: postfix `[m:n]`
* At most `n` times: postfix `[0:n]`
* At least `m` times: postfix `[m:end]`
* Terminals: `r"regex"`, `"string"`
  * Extra regex flags: `p` is for punctuation, and eats whitespace (`\s*`)
    after the match; `w` is for word, and implies `p`, but also makes sure
    match boundaries are word boundaries (`\b`). Values passed to semantics
    functions exclude eaten whitespace.
* Semantics: `expression >> unary_function` (like ParserCombinator's `|>`)
  * or `expression >>> nary_function` to interpolate args (like
    ParserCombinator's `>`).
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
@rule choice = seq & (r"\|"p & seq)[*]
@rule seq = item & (r"&"p & item)[*] & (r">>>?"p & julia_function)[?]
@rule item = lookahead | counted
@rule lookahead = r"\("p & (r"[+-]"p) & seq & r"\)"p
@rule counted = single & (count)[?]
@rule count = range | r"\["p & (r"[\?\*\+]"p) & r"]"p
@rule range = r"\["p & integer & (r":"p & (integer | r"end"w))[?] & r"]"p
@rule integer = r"\d+"w
@rule single = parens | terminal | nonterminal
@rule parens = r"\("p & choice & r"\)"p
@rule nonterminal = r"\pL\w+"w
@rule terminal = regex | string & r"\s*"
@rule regex = r"\br" & string & r"[impswx]*\s*"
@rule string = r"\"(\\.|[^\"])*\""
@rule julia_function = # left as an exercise ;)
```

Each rule defines a parsing function with the following signature:

```julia
nonterminal{T<:AbstractString}(input::T, cache=PEG.Cache())::
  Union{Void,Tuple{Any,SubString}}
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
  * backtrack, except within regexen and to try the next choice (`|`). That is,
    repetition `[]` is always greedy and possessive (to use PCRE terminology).
  * have `Empty(x)`/`@e_str`. Use semantics functions to discard values.
  * have `Dot()`. Use `r"."`.
  * have `Eos()`. Use `parse_whole`.
  * parse streams. Use `open(x->parse_whole(rule, x), args...)`.
  * include parsers for two random languages.
* has nicer syntax:
  * Operator precedence makes sense. Tight to loose, the operators are: postfix
    `[]` (whatever is in the brackets), prefix `+`/`-`, infix `&`, `>>`/`>>>`,
    `|`.
    * One exception: `->` gobbles up everything after it, so it's best to
      put lambdas in parens. E.g. `foo >> x->f(x) | bar` is actually `foo >>
      (x->f(x) | bar)`, so you might want to write `foo >> (x->f(x)) | bar`
      or `(foo >> x->f(x)) | bar` instead.
  * PC's `Plus(x)` and `Star(x)` become actual plusses and stars: `x[+]` and
    `x[*]`. And PEG has `x[?]` too.
  * `Equal`/`@e_str` and `Pattern`/`@p_str` are unneccessary, just use bare
    strings and regexen (with extra flags).
* does not require mutual recursion loops to be broken with `Delayed()`.
* does not have special types for matchers/rules, and does not require a
  trampoline to "interpret" them. They're just plain functions you can call
  directly.
