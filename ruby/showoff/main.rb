#!/usr/bin/ruby
require "open-uri"
require "cmath"
require "date"
require "tracer"

$FACTORIAL_OF = 10
$SORT_LENGTH = 10000

class MyClass
  @@object_count = 0

  def initialize(name)           # constructor
    @name = name
    @@object_count += 1
    puts "Hi, I'm an object, my name is #{@name} and there is #{@@object_count} of us."
    ObjectSpace.define_finalizer(self, method(:finalize))
  end

  def print_info
    #TODO
  end

  def get_name
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

def bubble_sort(data)
  for i in (0...data.length)
    for j in (0...data.length - i - 1)
      if data[j] > data[j + 1]
        data[j], data[j + 1] = data[j + 1], data[j]  # swap
      end
    end
  end
end

def print_header(title,character=?-)
  puts ?\n + title

  for i in (0...title.length)
    print character
  end

  puts ""
end

def recurse(depth)
  if depth <= 0
    puts "recursion bottom"
  else
    recurse depth - 1
  end
end

#=========================== MAIN ==============================

print_header "Showing off the power of Ruby!", ?~

print_header "general"

puts "Today is #{DateTime.now} and your platform is #{RUBY_PLATFORM}."

t1 = Time.now
factorial_recursive($FACTORIAL_OF)
t2 = Time.now
puts "Factorial of #{$FACTORIAL_OF} recursively: #{t2 - t1} seconds."

t1 = Time.now
factorial_iterative($FACTORIAL_OF)
t2 = Time.now
puts "Factorial of #{$FACTORIAL_OF} iteratively: #{t2 - t1} seconds."

puts ""
puts "Tracing recursive function call:"

Tracer.on {
  recurse 1
}

puts ""

begin
  print "If I divide by zero... "
  a = 10 / 0
rescue Exception => e 
  puts "there will be an exception: #{e.message}."
end

object1 = MyClass.new("object1")
object1.print_info
object2 = MyClass.new("object2")

sort_data = Array.new

#for i in (0...$SORT_LENGTH)
#  sort_data.push(($SORT_LENGTH - i) % ($SORT_LENGTH / 4))
#end

#t1 = Time.now
#bubble_sort sort_data
#t2 = Time.now

#puts "It took me #{t2 - t1} seconds to sort an array of length #{$SORT_LENGTH} with bubble sort."

puts "Sleep for 1 second."
Kernel::sleep 1

print_header "networking"

#url = "http://google.com"

#puts "I'm downloading #{url}..."

#begin
#  open("http://google.com") {|f|
#  puts "The html is #{f.length}" " characters long."
#  }
#rescue
#  puts "I couldn't do it."
#end

print_header "math"

puts "e^(i * pi) + 1 = #{CMath.exp(1i * Math::PI) + 1}"

print "sin :"

for i in (0...15)
  print "%.2f " % Math::sin(i / 15.0 * Math::PI * 2.0)
end

puts "---------------------"
