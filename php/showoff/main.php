<?php

/*
  Show the power of PHP with only standard library.

  Miloslav Ciz, 2017
  WTFPL license
*/

define("FACTORIAL_OF", 10);

Class MyClass
  {
    protected $name;

    public function __construct($name)   // constructor
      {
        $this->name = $name;
        echo "Hi, I'm an object, my name is " . $name . ".\n";
      }

    public function print_info()
      {
        echo "My class is named " . get_class() . ". My methods are: \n";

        foreach (get_class_methods(get_class()) as $key => $value)
          echo "  " . $value . "\n";
      }

    public function __destruct()         // destructor
      {
        echo $this->name . " says bye.\n";
      }
  }

function factorial_recursive($value)
  {
    return $value < 2 ? 1 : $value * factorial_recursive($value - 1);
  }

function factorial_iterative($value)
  {
    $result = 1;

    for ($i = 2; $i <= $value; $i++)
      $result *= $i;

    return $result;
  }

function print_header($title, $char='-')
  {
    echo "\n" . $title . "\n";

    for ($i = 0; $i < strlen($title); $i++)
      echo $char;

    echo "\n";
  }

print_header("Showing off the power of PHP!",'~');
print_header("general");

echo "Today is " . date('Y-m-d H:i:s') . ".\n";

$t1 = microtime(true);
factorial_recursive(FACTORIAL_OF);
$t2 = microtime(true);
echo "Factorial of " . FACTORIAL_OF . " recursively: " . ($t2 - $t1) . " seconds.\n";

$t1 = microtime(true);
factorial_iterative(FACTORIAL_OF);
$t2 = microtime(true);
echo "Factorial of " . FACTORIAL_OF . " iteratively: " . ($t2 - $t1) . " seconds.\n";

$o1 = new MyClass("object1");
$o1->print_info();
$o2 = new MyClass("object2");

?>
