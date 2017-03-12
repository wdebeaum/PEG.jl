"""
Define a Parsing Expression Grammar via a macro and abuse of Julia syntax.

* Rules: `@rule name = expression`
* Alternation: infix `|`
* Sequence: infix `&`
* Positive lookahead: prefix `+`
* Negative lookahead: prefix `-`
* Zero or one time: postfix `[?]` (≡ `[0:1]`)
* Any number of times: postfix `[*]` (≡ `[0:end]`)
* One or more times: postfix `[+]` (≡ `[1:end]`)
* Exactly `m` times: postfix `[m]` (≡ `[m:m]`) (where m is an integer)
* Between `m` and `n` times inclusive: postfix `[m:n]`
* At most `n` times: postfix `[0:n]`
* At least `m` times: postfix `[m:end]`
* Terminals: `r"regex"`, `"string"`
  * Extra regex flags: `p` is for punctuation, and eats whitespace (`\\s*`)
    after the match; `w` is for word, and implies `p`, but also makes sure
    match boundaries are word boundaries (`\\b`). Values passed to semantics
    functions exclude eaten whitespace.
* Semantics: `expression >> unary_function` (like ParserCombinator's `|>`)
  * or `expression >>> nary_function` to interpolate args (like
    ParserCombinator's `>`).

Put another way:

```julia
using PEG
@rule grammar = "using PEG\\n" & rule[*]
@rule rule = r"@rule"p & nonterminal & r"="p & alt
@rule alt = seq & (r"\\|"p & seq)[*]
@rule seq = item & (r"&"p & item)[*] & (r">>>?"p & julia_function)[?]
@rule item = lookahead | counted
@rule lookahead = r"\\("p & (r"[+-]"p) & seq & r"\\)"p
@rule counted = single & (count)[?]
@rule count = range | r"\\["p & (r"[\\?\\*\\+]"p) & r"]"p
@rule range = r"\\["p & integer & (r":"p & (integer | r"end"w))[?] & r"]"p
@rule integer = r"\\d+"w
@rule single = parens | terminal | nonterminal
@rule parens = r"\\("p & alt & r"\\)"p
@rule nonterminal = r"\\pL\\w+"w
@rule terminal = regex | string & r"\\s*"
@rule regex = r"\\br" & string & r"[impswx]*\\s*"
@rule string = r"\\"(\\\\.|[^\\"])*\\""
@rule julia_function = # left as an exercise ;)
```

Each rule defines a parsing function with the following signature:

    nonterminal{T<:AbstractString}(input::T, cache=PEG.Cache())::
      Union{Void,Tuple{Any,SubString}}

The `Any` part of the return value is the abstract syntax tree, while the
`SubString` is the remaining input after the parsed portion. If parsing fails,
`nothing` is returned.

Call `PEG.setdebug!()` to have debugging information printed during parsing.
Call `PEG.setdebug!(false)` to turn it off again.
"""
module PEG
export @rule, parse_whole, squash_ast

global debug = false
function setdebug!(val::Bool=true)
  global debug = val
end

"""
A Cache maps from (rule name or gensym, input string length remaining) to
either nothing or (parsed value, remaining input substring) (which is what rule
functions return).
"""
typealias Cache Dict{Tuple{Symbol,Int},Union{Void,Tuple{Any,SubString}}}

function cache_rule(sym::Symbol, fn::Function, input::SubString, cache::Cache)
  local key = (sym, length(input))
  haskey(cache, key) && return cache[key]
  if debug
    if !ismatch(r"^##", string(sym))
      println(" "^length(stacktrace()) * string(sym))
    end
    cache[key] = fn(input, cache)
    if cache[key] != nothing
      println(" "^length(stacktrace()) * "$sym matched " * string(length(input)) * ":" * string(length(cache[key][2])+1) * " bytes from end of input, returning " * string(cache[key][1]))
#    else
#      println(" "^length(stacktrace()) * string(sym) * " failed to match " * string(length(input)) * " bytes from end of input")
    end
    return cache[key]
  else
    return (cache[key] = fn(input, cache))
  end
end

# terminal ""
function to_rule(str::AbstractString)
  # not cached because I'm guessing startswith is probably faster
  (input, cache)->begin
    if startswith(input, str)
      (str, input[length(str)+1:end])
    end
  end
end

# nonterminal
function to_rule(sym::Symbol)
  # not cached here because it's already cached in the callee rule
  :((input, cache) -> $(esc(sym))(input, cache))
  #                   ^^^^^^^^^^^
  # This is why some to_rule methods return expressions instead of anonymous
  # functions. Anything that could contain a nonterminal needs to be an
  # expression, so that this esc call appears literally in the expression the
  # @rule macro returns. If we used eval instead of esc here, and raw anonymous
  # functions everywhere, then sym would be evaluated in the context of the PEG
  # module instead of in the caller's module.
end

to_rule(expr::Expr) = to_rule(Type{expr.head}, expr.args...)
to_rule(head::Union{Type{Type{:call}},Type{Type{:macrocall}}}, name::Symbol, args...) =
  to_rule(head, Type{name}, args...)
to_rule(head, args...) = error("can't convert $head expression to PEG rule; args=$args")

# variant of to_rule(::Symbol) for qualified rule references
to_rule(::Type{Type{:.}}, args...) =
  :((input, cache) -> $(esc(Expr(:., args...)))(input, cache))

"Return (a copy of) regex flags with flag removed, and a boolean indicating whether the flag was there in the first place."
function remove_re_flag{T<:AbstractString}(flags::T, flag::Char)
  local i = search(flags, flag)
  if i == 0
    (flags, false)
  else
    (flags[1:prevind(flags, i)] * flags[nextind(flags, i):end], true)
  end
end

# terminal r""
function to_rule(::Type{Type{:macrocall}}, ::Type{Type{Symbol("@r_str")}}, str::String, flags...)
  if length(flags) == 0
    flags = ""
  else
    flags = flags[1]
  end
  # p = punctuation; allows whitespace after the match
  local p
  flags, p = remove_re_flag(flags, 'p')
  # w = word; ensures both sides of the match are word boundaries; implies p
  local w
  flags, w = remove_re_flag(flags, 'w')
  p = p || w
  str = "^" * (w ? "\\b" : "") * "($str)" * (w ? "\\b" : "") * (p ? "\\s*" : "")
  re = Regex(str, flags)
  local sym = gensym(string(re))
  (input, cache)->cache_rule(sym, (input, cache)->begin
    local m = match(re, input)
    m == nothing && return
    (m.captures[1], input[length(m.match)+1:end])
  end, input, cache)
end

# fn[m:n]
function to_rule(::Type{Type{:ref}}, fn, range::Expr)
  fn = to_rule(fn)
  range.head == :(:) ||
    error("can't convert non-range ref expression to PEG rule: $range")
  local m
  local n
  m, n = range.args
  n == :end && (n = Inf)
  local sym = gensym("[$m:$n]")
  :((input, cache)->cache_rule($(Meta.quot(sym)), (input, cache)->begin
    local results = []
    local i = 0
    while i < $n
      local m = $fn(input, cache)
      m == nothing && break
      local result
      result, input = m
      push!(results, result)
      i += 1
    end
    i < $m && return
    (results, input)
  end, input, cache))
end

# defined in terms of the above method
to_rule(::Type{Type{:ref}}, fn, n::Int) = to_rule(Type{:ref}, fn,:($n:$n))
to_rule(::Type{Type{:ref}}, fn, n::Symbol) = to_rule(Type{:ref}, fn, Type{n})
to_rule(::Type{Type{:ref}}, fn, ::Type{Type{:?}}) = to_rule(Type{:ref}, fn,:(0:1))
to_rule(::Type{Type{:ref}}, fn, ::Type{Type{:*}}) = to_rule(Type{:ref}, fn,:(foo[0:end]).args[2])
to_rule(::Type{Type{:ref}}, fn, ::Type{Type{:+}}) = to_rule(Type{:ref}, fn,:(foo[1:end]).args[2])

# positive lookahead
function to_rule(::Type{Type{:call}}, ::Type{Type{:+}}, fn)
  fn = to_rule(fn)
  # not cached here because it's already cached in fn
  :((input, cache)->begin
    local m = $fn(input, cache)
    m == nothing ? nothing : (nothing, input) # NOTE: original input
  end)
end

# negative lookahead
function to_rule(::Type{Type{:call}}, ::Type{Type{:-}}, fn)
  fn = to_rule(fn)
  # not cached here because it's already cached in fn
  :((input, cache)->begin
    local m = $fn(input, cache)
    m == nothing ? (nothing, input) : nothing
  end)
end

"Given e.g. x=:(((a op b) op c) op d), return [a,b,c,d]."
flatten_op(x, op::Symbol) = Any[x]
function flatten_op(x::Expr, op::Symbol)
  if x.head == :call && x.args[1] == op
    push!(flatten_op(x.args[2], op), x.args[3])
  else
    Any[x]
  end
end

# sequence
function to_rule(::Type{Type{:call}}, ::Type{Type{:&}}, a, b)
  local seq = map(to_rule, [flatten_op(a, :&); b])
  local sym = gensym(:&)
  :((input, cache)->cache_rule($(Meta.quot(sym)), (input, cache)->begin
    local results = []
    local item
    for item ∈ [$(seq...)]
      local m = item(input, cache)
      m == nothing && return
      local result
      result, input = m
      push!(results, result)
    end
    (results, input)
  end, input, cache))
end

"Singleton value that semantics functions may return to cause the parsing
expression they were called from to fail, e.g. `return Failure()`."
type Failure
end

# semantics
function to_rule(::Type{Type{:call}}, ::Type{Type{:>>}}, pe, fn)
  pe = to_rule(pe)
  local sym = gensym(:>>)
  :((input, cache)->cache_rule($(Meta.quot(sym)), (input, cache)->begin
    local m = ($pe)(input, cache)
    m == nothing && return nothing
    local pe_result
    pe_result, input = m
    fn_result = $(esc(fn))(pe_result)
    fn_result == Failure() && return nothing
    (fn_result, input)
  end, input, cache))
end

function to_rule(::Type{Type{:call}}, ::Type{Type{:>>>}}, pe, fn)
  pe = to_rule(pe)
  local sym = gensym(:>>>)
  :((input, cache)->cache_rule($(Meta.quot(sym)), (input, cache)->begin
    local m = ($pe)(input, cache)
    m == nothing && return nothing
    local pe_result
    pe_result, input = m
    fn_result = $(esc(fn))(pe_result...) # "..." is the only difference from ^^^
    fn_result == Failure() && return nothing
    (fn_result, input)
  end, input, cache))
end

# alternation
function to_rule(::Type{Type{:call}}, ::Type{Type{:|}}, a, b)
  local alt = map(to_rule, [flatten_op(a, :|); b])
  local sym = gensym(:|)
  :((input, cache)->cache_rule($(Meta.quot(sym)), (input, cache)->begin
    local item
    for item ∈ [$(alt...)]
      local m = item(input, cache)
      m == nothing || return m
    end
    return nothing
  end, input, cache))
end

"""
    @rule name = parsing_expression...

Defines a single rule in a parsing expression grammar as a function. See the
documentation of the PEG module for a description of parsing expression
grammars, and the type and usage of the rule functions defined using `@rule`.
"""
macro rule(assignment::Expr)
  assignment.head == :(=) || error("expected = Expr, but got $assignment")
  local name
  local value_expr
  name, value_expr = assignment.args
  local value_fn = to_rule(value_expr)
  :(function $(esc(name)){T<:AbstractString}(input::T, cache::Cache=Cache())
      if !isa(input, SubString)
	input = SubString(input, 1, length(input))
      end
      local sym = Symbol($(string(name)))
      cache_rule(sym, $value_fn, input, cache)
    end)
end

"""
    parse_whole(rule, input)

Parse the whole `input` string as one instance of the given `rule`. This
differs from just calling the `rule` itself on the `input` in a couple
important ways:

* When parsing succeeds, `parse_whole` returns only the parsed value, while
  `rule` returns a Tuple of the parsed value and the remaining unparsed input.
* When parsing fails, `parse_whole` throws an exception, with information on
  what failed to match starting at the latest point in the string that any
  parsing expression matched up to. `rule` just returns `nothing`.
"""
function parse_whole{T<:AbstractString}(rule::Function, input::T)
  local cache = Cache()
  local m = rule(input, cache)
  if m == nothing # failed to parse
    # find the last index we tried to parse anything starting at, and all the
    # cache keys for the expressions we tried to parse there
    # FIXME plain strings aren't cached, so we'll miss them as keys
    local last_index = 0
    local last_keys = Symbol[]
    for pair ∈ cache
      if pair[1][2] < last_index
	last_index = pair[1][2]
	last_keys = [pair[1][1]]
      elseif pair[1][2] == last_index
        push!(last_keys, pair[1][1])
      end
    end
    last_index = length(input) - last_index +1 # ugh, backwards 1-based indexing
    # convert regex gensym keys to something more readable, remove other
    # gensyms, and convert everything to strings
    last_keys = map(x->begin
      local s = string(x)
      local m = match(r"^##(r[^#]+)#\d+$", s)
      if m != nothing
	m.captures[1]
      elseif ismatch(r"^##", s)
	nothing
      else
	s
      end
    end, last_keys)
    filter!(x->(x != nothing), last_keys)
    # translate the index into line and column and get the text on the line
    local before = split(input[1:last_index-1], r"\r\n|\n\r|\n|\r")
    local after = replace(input[last_index:end], r"[\r\n].*", "")
    local line_num = length(before)
    local column_num = length(before[end])
    local line = before[end] * after
    local message = "On line $line_num, at column $column_num:\n$line\n" * " "^Int(clamp(column_num-1, 0, Inf)) * "^ here\nexpected one of the following: " * join(last_keys, ", ") * "\n"
    debug && print(message)
    throw(ParseError(message))
  else # parse succeeded
    m[1]
  end
end

"""
    squash_ast(x)

Given an ast returned by a grammar with no semantics, return a simplified
version, such that there are no `nothing`s, and no `Any[]`s with exactly zero
or one element.
"""
squash_ast(x) = x
function squash_ast(children::Array{Any,1})
  squashed_children = map(squash_ast, children)
  filter!(x->(x != nothing), squashed_children)
  if length(squashed_children) == 0
    nothing
  elseif length(squashed_children) == 1
    squashed_children[1]
  else
    squashed_children
  end
end

end
