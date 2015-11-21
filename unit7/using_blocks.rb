def foo x
    if x
        yield
    else
        yield
        yield
    end
end

foo true do puts "hi" end

foo false do puts "hi" end

def count i
    if yield i
        1
    else
        1 + (count (i+1) {|x| yield x})
    end
end

# count 1 do |x| x == 100 end

a = [3, 5, 7, 9]
b = a.map {|x| x + 1}
i = b.count {|x| x >= 6}

c = a.map do |x| lambda {|y| x >= y} end
c[0].call 3
j = c.count {|x| x.call(5)} # the number of elements greater equal than 5

