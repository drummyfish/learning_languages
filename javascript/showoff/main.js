function print_header(message, separator)
  {
    if (separator == undefined)
      separator = "-";

    console.log("");
    console.log(message);

    var separator_string = "";

    for (i = 0; i < message.length; i++)
      separator_string += separator;

    console.log(separator_string);
  }

print_header("Showing off the power of JavaScript!","~");
