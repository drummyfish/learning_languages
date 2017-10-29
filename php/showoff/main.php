<?php

Class MyClass
  {
    protected $name;

    public function __construct($name)   // constructor
      {
        $this->name = $name;
        echo "Hi, I'm an object, my name is " . $name . ".\n";
      }

    public function __destruct()         // destructor
      {
        echo $this->name . " says bye.\n";
      }
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

$o1 = new MyClass("object1");
$o2 = new MyClass("object2");

?>
