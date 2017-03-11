using PEG

function do_op(x)
  local v
  local op_v_pairs
  v, op_v_pairs = x
  for op_v ∈ op_v_pairs
    op, v2 = op_v
    v = eval(Symbol(op))(v, v2)
  end
  v
end

@rule num = r"\d+"w >> (x->parse(Int, x)) | r"\("p & add & r"\)"p >> x->x[2]
# parens required here ^                ^ to prevent fn from gobbling line
@rule mul = num & (r"[/*]"p & num)[*] >> do_op
@rule add = mul & (r"[+-]"p & mul)[*] >> do_op

using Base.Test
@test add("2 + 3 * 5") == (17, "")
@test add("2 * 3 + 5") == (11, "")
@test add("(2 + 3) * 5") == (25, "")
