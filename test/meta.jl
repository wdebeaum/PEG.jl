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
@rule julia_function = nonterminal
using Test
@test grammar(replace(open(s->read(s, String), @__FILE__), r"using Test.*"s => ""))[2] == ""

