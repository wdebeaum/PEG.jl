using PEG
using Test

function parse_fails_at(rule, input)
  try
    parse_whole(rule, input)
    "parse succeeded!"
  catch err
    isa(err, Meta.ParseError) || rethrow()
    m = match(r"^On line \d+, at column \d+ \(byte (\d+)\):", err.msg)
    m == nothing && rethrow()
    parse(Int, m.captures[1])
  end
end

# test miscellaneous features not covered by calculator or meta

# repeat range
@rule a34 = "a"[3:4]
@test parse_fails_at(a34, "aa") == 1
@test parse_whole(a34, "aaa") == ["a", "a", "a"]
@test parse_whole(a34, "aaaa") == ["a", "a", "a", "a"]
@test parse_fails_at(a34, "aaaaa") == 1

# 1 or more times
@rule aplus = "a"[+]
@test parse_fails_at(aplus, "") == 1
@test parse_whole(aplus, "a") == ["a"]
@test parse_whole(aplus, "aaa") == ["a", "a", "a"]

# positive lookahead
@rule abc = +"abc" & r"\w+"
@test parse_fails_at(abc, "abdargh") == 1
@test parse_whole(abc, "abc") == [nothing, "abc"]
@test parse_whole(abc, "abcfoo") == [nothing, "abcfoo"]

# negative lookahead
@rule notabc = -"abc" & r"\w+"
@test parse_whole(notabc, "ab") == [nothing, "ab"]
@test parse_whole(notabc, "abd") == [nothing, "abd"]
@test parse_whole(notabc, "abdfoo") == [nothing, "abdfoo"]
@test parse_fails_at(notabc, "abcargh") == 1

# interpolating semantics
@rule interp = r"\d+" & r"[a-z]+" & r"\d+" > (a,b,c) -> b * Base.string(parse(Int, a) + parse(Int, c))
@test parse_whole(interp, "123abc456") == "abc579"

# multibyte characters in input and rule ("∈" is 3 bytes)
@rule inop = "∈"
@rule inexpr = r"[a-z]+" & inop & r"[a-z]+"
@test parse_whole(inexpr, "foo∈bar") == ["foo", "∈", "bar"]
@test parse_fails_at(inexpr, "foo∈42") == 7
@test parse_fails_at(inexpr, "foo∈") == 7
@test parse_fails_at(inexpr, "foo42") == 4
@test parse_fails_at(inexpr, "foo") == 4
