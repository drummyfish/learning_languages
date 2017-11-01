program ShowOff;

{
  Show the capabilities of Pascal with only the
  standard library.

  Miloslav Ciz, 2017
  WTFPL license
}

uses crt;

const FACTORIAL_OF = 10;
const SORT_LENGTH = 10000;

procedure print_header(message: string; separator: char);

var i: byte;

begin
  WriteLn();
  WriteLn(message);
  
  for i := 1 to Length(message) do
    Write(separator);

  WriteLn();
end;

procedure introduce();
begin
  WriteLn('I am Pascal, a case-insensitive compiled language that');
  WriteLn('is no longer used very much, but I served well as');
  WriteLn('an introductory language to many programmers. I was also');
  WriteLn('used by Apple.');
end;

begin
  print_header('Showing off the power of Pascal!','~');
  introduce();
  print_header('general','-');
end.
