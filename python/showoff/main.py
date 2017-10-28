import time
import sys
import pprint
import inspect
import datetime
import math
import cmath
import random
import md5
import sha
import platform
import urllib2
import traceback

FACTORIAL_OF = 10

class MyClass(object):
  num_of_objects = 0

  def __init__(self, name):          # constructor
    MyClass.num_of_objects += 1
    self.name = name
    print("Hi, I'm an object, my name is " + name + " and there's " + str(MyClass.num_of_objects) + " of us.")

  def print_info(self):
    print("My class name is " + type(self).__name__ + " and it has these attributes:")

    attributes = [item for item in type(self).__dict__.keys() if item[0] != "_"]

    for a in attributes:
      print("  " + a)

  def __del__(self):                 # destructor
    print(self.name + " says bye.")

def factorial_recursive(n):
  return 1 if n <= 1 else n * factorial_recursive(n - 1)

def factorial_iterative(n):
  result = 1

  for i in range(1,n + 1):
    result *= i
 
  return result

def print_header(text, underscore="-"):
  print("\n" + text + "\n" + len(text) * underscore)

def download_web(url):
  response = urllib2.urlopen(url)
  return response.read()

def print_stack(depth):
  if depth <= 0:
    print("Showing the stack of recursive function call:")
    traceback.print_stack()
  else:
    print_stack(depth - 1)

# ===================== main ========================

print_header("Showing off the power of Python!","=")

print_header("general")

print("arguments: \n" + "\n".join(["  " + str(i) + ": " + sys.argv[i] for i in range(len(sys.argv))]))

print("It's " + str(datetime.datetime.now()) + ".")

sequence = [random.randint(1,10) for i in range(10)]
print("10 random number sequence from 1 to 10: " + str(sequence))
random.shuffle(sequence)
print("the same sequence randomly shuffled: " + str(sequence))
sequence.sort()
print("and the same sequence sorted: " + str(sequence))

sentence = "Hello world."

print("md5 of \"" + sentence + "\" is " + md5.new(sentence).hexdigest() + ".")
print("sha of \"" + sentence + "\" is " + sha.new(sentence).hexdigest() + ".")

print("Your platform type is " + platform.machine() + " with " + platform.processor() + " CPU running " +
      platform.system() + " OS, Python version is " + platform.python_version() + " (" + platform.python_implementation() + ").")

print_stack(3)

print("")

object1 = MyClass("object1")
object1.print_info()
object2 = MyClass("object2")

print("Sleep for 1 second.")
time.sleep(1)

t1 = time.time()
fact = factorial_recursive(FACTORIAL_OF)
t2 = time.time()

print("factorial of " + str(FACTORIAL_OF) + " recursively - " + str(t2 - t1) + " seconds.")

t1 = time.time()
fact = factorial_iterative(FACTORIAL_OF)
t2 = time.time()

print("factorial of " + str(FACTORIAL_OF) + " iteratively - " + str(t2 - t1) + " seconds.")

print_header("math")

print("e^(i * pi) + 1 = " + str(cmath.e ** (complex(0,1) * cmath.pi) + 1))
print("sin: " + " ".join([str(math.sin(i / 10.0 * 2 * math.pi))[:4] for i in range(10)]))

print_header("web")

url = "http://google.com"

print("I'm downloading " + url + "...")

try:
  html = download_web(url)
  print("The html is " + str(len(html)) + " characters long.")
except Exception:
  print("I couldn't do it.")

print("------------------")
