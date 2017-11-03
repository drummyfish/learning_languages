program ShowOff;

{
  Show the capabilities of Pascal with only the
  standard library.

  Miloslav Ciz, 2017
  WTFPL license
}

uses crt, dos, math, strutils, sysutils, matrix, video;

const FACTORIAL_OF = 10;
const SORT_LENGTH = 10000;

type t_sort_data = array[1..SORT_LENGTH] of word;

var time_start: word;   { global variable for measuring time }

procedure print_header(message: string; separator: char);
var i: byte;
begin
  writeln();
  writeln(message);
  
  for i := 1 to length(message) do
    write(separator);

  writeln();
end;

procedure introduce();
begin
  writeln('I am Pascal, a case-insensitive compiled language that');
  writeln('is no longer used very much, but I served well as');
  writeln('an introductory language to many programmers. I was also');
  writeln('used by Apple.');
end;

function simple_real(number: real): string;
begin
  simple_real := floattostrf(number,fffixed,2,4);
end;

procedure print_matrix3(m: Tmatrix3_double);
var i, j: byte;
begin
  for j := 0 to 2 do
    begin
      write('  ');

      for i := 0 to 2 do
        write(simple_real(m.data[j,i]),' ');

      writeln();
    end;
end;

procedure bubble_sort(var data: t_sort_data);
var i, j, tmp: word;
begin
  for i := 1 to length(data) do
    for j := 1 to length(data) - i do
      if data[j] > data[j + 1] then
        begin                          { swap }
          tmp := data[j];      
          data[j] := data[j + 1];
          data[j + 1] := tmp;
        end;
end;

function factorial_recursive(number: word): word;
begin
  if number <= 1 then
    factorial_recursive := 1
  else
    factorial_recursive := number * factorial_recursive(number - 1);
end;

function factorial_iterative(number: word): word;
var i, n: word;
begin
  n := 1;

  for i := 1 to number do
    n := n * i;

  factorial_iterative := n;
end;

procedure time_measure_begin();
var h, m, s: word;
begin
  gettime(h,m,s,time_start);
end;

function time_measure_end(): real; { returns time in seconds }
var h, m, s, time_end: word;
begin
  gettime(h,m,s,time_end);
  time_measure_end := (time_end - time_start) / 100; 
end;

{------------- main ---------------}

var year, month, day, wday, hour, minute, second, second100, i, j, tmp: word;
var sec, sine, cosine: real;
var path, teststr: string;
var m1, m2: Tmatrix3_double;
var sort_data: t_sort_data;

begin
  print_header('Showing off the power of Pascal!','~');
  introduce();
  print_header('general','-');

  writeln('CLI arguments:');

  for i := 1 to paramcount() do
    writeln('  ',paramstr(i));

  write('random numbers from 1 to 10: ');

  for i := 1 to 10 do
    write(random(9) + 1,' ');

  writeln();

  time_measure_begin();
  tmp := factorial_recursive(FACTORIAL_OF);
  sec := time_measure_end();
  writeln('factorial of ',FACTORIAL_OF,' recursively: ',sec,' seconds');

  time_measure_begin();
  tmp := factorial_iterative(FACTORIAL_OF);
  sec := time_measure_end();
  writeln('factorial of ',FACTORIAL_OF,' iteratively: ',sec,' seconds');

  getdate(year,month,day,wday);
  gettime(hour,minute,second,second100);

  for i := 1 to SORT_LENGTH do  { init sort data }
    sort_data[i] := (SORT_LENGTH + 1 - i) mod (SORT_LENGTH div 4);

  writeln('It is ',day,'.',month,'.',year,', ',hour,':',minute,':',second,'.');
  writeln('free disk space: ',diskfree(0) div 1000000000,' GB / ',disksize(0) div 1000000000,' GB');
  getdir(0,path);
  writeln('current directory: ',path);
  writeln('system PATH variable: ',getenv('PATH'));

  writeln('sort test...');
  time_measure_begin();
  bubble_sort(sort_data);
  sec := time_measure_end();
  writeln('It took me ',sec, ' seconds to sort an array of length ', SORT_LENGTH, ' with bubble sort.');

  teststr := 'Hello World';
  writeln('"',teststr,'" backwards is "',reversestring(teststr),'".');

  print_header('math','-');

  for j := 0 to 2 do
    for i := 0 to 2 do
      begin
        m1.data[i,j] := i + j;
        m2.data[j,i] := i * j;
      end;

  writeln('matrix A:');
  print_matrix3(m1);
  writeln('matrix B:');
  print_matrix3(m2);
  writeln('matrix A * B:');
  m1 := m1 * m2;
  print_matrix3(m1);

  writeln();

  writeln('pi = ',floattostrf(pi,fffixed,2,30));
  write('sin: ');

  for i := 0 to 9 do
    begin
      sincos(i / 9 * 2 * pi,sine,cosine);
      write(' ',simple_real(sine));
    end;

  writeln();
end.
