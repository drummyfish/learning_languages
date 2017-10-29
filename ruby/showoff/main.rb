#!/usr/bin/ruby

$FACTORIAL_OF = 10

class MyClass
  @@object_count = 0

  def initialize(name)           # constructor
    @name = name
    @@object_count += 1
    puts "Hi, I'm an object, my name is #{@name} and there is #{@@object_count} of us."
    ObjectSpace.define_finalizer(self, method(:finalize))
  end

  def get_name()
    return @name
  end

  def finalize(object_id)        # destructor
    puts ObjectSpace._id2ref(object_id).get_name() + " says bye."
  end
end

def factorial_recursive(value)
  if value <= 1
    return 1
  else
    return value * factorial_recursive(value - 1)
  end
end

def factorial_iterative(value)
  result = 1

  for i in 2..value
    result *= i
  end

  return result
end

#=========================== MAIN ==============================

puts factorial_recursive($FACTORIAL_OF)
puts factorial_iterative($FACTORIAL_OF)

object1 = MyClass.new("object1")
object2 = MyClass.new("object2")

