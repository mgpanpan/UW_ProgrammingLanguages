require "./expressions_small_languages.rb"

exp1 = Add.new(Add.new(Int.new(5), Int.new(7)), Negate.new(Int.new(3)))

eval_out = exp1.eval;
toString_out = exp1.toString;
hasZero_out = exp1.hasZero;

puts eval_out.i
puts toString_out
puts hasZero_out
