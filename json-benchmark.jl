# see results in README.md

using BenchmarkTools
using PEG # this package
using StringParserPEG # another similar package
using JSON # hand-written JSON parser for comparison

# PEG grammar
commasPEG(head, tail) = [head, (t[2] for t ∈ tail)...]
bracketsPEG(left, middle, right) = (isempty(middle) ? [] : middle[1])
@rule object =
  r"{"p & (pair & (r","p & pair)[*] > commasPEG)[:?] & r"}"p > Dict∘bracketsPEG
@rule pair = string & r":"p & value > (k,c,v) -> (k => v)
@rule array =
  r"\["p & (value & (r","p & value)[*] > commasPEG)[:?] & r"]"p > bracketsPEG
@rule value =
  string, number, object, array,
  r"true|false"w |> Meta.parse, r"null"w |> x->nothing
@rule string = r"\"([^\"\\]|\\[\"\\/bfnrt]|\\u[0-9A-Fa-f]{4})*\""p |> s->Meta.parse(replace(s, r"\$"=>"\\\$"))
@rule number = r"-?(0|[1-9]\d*)(\.\d+)?([Ee][+-]?\d+)?"p |> Meta.parse
@rule startPEG = r"\s*" & value > (w,v)->v

# StringParserPEG grammar
commasSPP(r,v,f,l,c) = (length(c) == 1 ? [c[1]] : [c[1], (t.children[2] for t ∈ c[2].children)...])
bracketsSPP(r,v,f,l,c) = (isempty(c[2].children) ? [] : c[2].children[1])
braces = Dict∘bracketsSPP
parsevalue(r,v,f,l,c) = Meta.parse(v)
parsestr(r,v,f,l,c) = Meta.parse(replace(v, r"\$"=>"\\\$"))
grammarSPP = Grammar("""
  object => (r({\\s*)r & ?((pair & *(r(,\\s*)r & pair)) {Main.commasSPP}) & r(}\\s*)r) {Main.braces}
  pair => (string & r(:\\s*)r & value) {(r,v,f,l,c)->(c[1] => c[3])}
  array => (r(\\[\\s*)r & ?((value & *(r(,\\s*)r & value)) {Main.commasSPP}) & r(]\\s*)r) {Main.bracketsSPP}
  value => string | number | object | array | r((true|false)\\b\\s*)r {Main.parsevalue} | r(null\\b\\s*)r {(r,v,f,l,c)->:null}
  string => r(\\"([^\\"\\\\]|\\\\[\\"\\\\/bfnrt]|\\\\u[0-9A-Fa-f]{4})*\\"\\s*)r {Main.parsestr}
  number => r(-?(0|[1-9]\\d*)(\\.\\d+)?([Ee][+-]?\\d+)?\\s*)r {Main.parsevalue}
  start => (r(\\s*)r & value) {(r,v,f,l,c)->c[1]}
""")

# load JSON string from file from
# https://github.com/Chevrotain/chevrotain/blob/gh-pages/performance/samples/10K_json.js
json_str = open(x->read(x,String), "10K.json")

println("JSON:")
display(@benchmark JSON.parse(json_str) seconds=60)
println("PEG:")
display(@benchmark parse_whole(startPEG, json_str) seconds=60)
println("StringParserPEG:")
display(@benchmark parse(grammarSPP, json_str) seconds=60)
