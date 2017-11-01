/*
  Show off the capabilities of C language with only
  standard library.

  Miloslav Ciz, 2017
  WTFPL license
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <limits.h>
#include <float.h>
#include <stdint.h>
#include <math.h>
#include <locale.h>
#include <complex.h>
#include <tgmath.h>
#include <wchar.h>

#define FACTORIAL_OF 10
#define SORT_LENGTH 10000
#define PI 3.1415926535
#define E 2.718281

#define time_diff(t1,t2) ((double) (t2 - t1) / ((double) CLOCKS_PER_SEC))

int line_count();         // forward declaration

int factorial_recursive(int n)
  {
    return n <= 1 ? 1 : n * factorial_recursive(n - 1); 
  }

int factorial_iterative(int n)
  {
    int result = 1;

    for (int i = 2; i <= n; i++)
      result *= i;

    return result;
  }

void bubble_sort(int *data, int data_len)
  {
    int tmp;

    for (int i = 0; i < data_len; i++)
      for (int j = 0; j < data_len - i - 1; j++)
        if (data[j] > data[j + 1])
          {
            tmp = data[j + 1];
            data[j + 1] = data[j];
            data[j] = tmp;
          }
  }

void print_separator(char c, int length)
  {
    for (int i = 0; i < length; i++)
      putchar(c);

    printf("\n");
  }

void print_array(char *title, double *a, int len)
  {
    printf("%s: ",title);

    for (int i = 0; i < len; i++)
      printf("%7.2f ",a[i]);

    printf("\n");
  }

void print_header(char *text)
  {
    printf("\n%s\n",text);
    print_separator('-',strlen(text));
  }

void introduce()
  {
    printf("I'm C, the super-fast, lower-level language. I was\n");
    printf("born as a brother to UNIX and nowadays I power Linux.\n");
    printf("Old-school hackers still prefer me to C++ as I can use\n");
    printf("their HW just a little bit more efficiently sometimes.\n");
  }

int main(int argc, char **argv)
  {
    print_separator('~',27);
    printf("Showing off the power of C!\n");
    print_separator('~',27);

    introduce();

    print_header("general");

    printf("My source code file is called %s and has %i lines. This is line %i.\n",__BASE_FILE__,line_count(),__LINE__);

    printf("CLI params:\n");
    
    for (int i = 0; i < argc; i++)
      printf("  %d: %s\n",i,argv[i]);

    time_t rawtime;
    struct tm *timeinfo;
    clock_t t1,t2;

    time(&rawtime);
    timeinfo = localtime(&rawtime);
    printf("The current time and date is: %s",asctime(timeinfo));

    printf("random numbers: %d %d %d\n",rand(),rand(),rand());

    printf("Executing system command \"ls\" returns %d.\n",system("ls"));

    printf("Environment variable PATH: %s.\n",getenv("PATH"));

    printf("Compiler version is %s.\n",__VERSION__);

    t1 = clock();
    factorial_recursive(FACTORIAL_OF);
    t2 = clock();

    printf("factorial of %d recursively: %f seconds.\n",FACTORIAL_OF,time_diff(t1,t2));

    t1 = clock();
    factorial_iterative(FACTORIAL_OF);
    t2 = clock();

    printf("factorial of %d iteratively: %f seconds.\n",FACTORIAL_OF,time_diff(t1,t2));

    factorial_iterative(15);

    int sort_data[SORT_LENGTH];

    for (int i = 0; i < SORT_LENGTH; i++)
      sort_data[i] = (SORT_LENGTH - i) % (SORT_LENGTH / 4);

    printf("sort test...\n");

    t1 = clock();
    bubble_sort(sort_data,SORT_LENGTH);
    t2 = clock();

    printf("It took me %f seconds to sort an array of length = %d with bubble sort.\n",time_diff(t1,t2),SORT_LENGTH);

    print_header("limits");

    printf("bits in char: %d\n",CHAR_BIT);
    printf("size of int: %lu\n",sizeof(int));
    printf("max value of unsigned int: %u\n",UINT_MAX);
    printf("max value of unsigned long long: %llu\n",ULLONG_MAX);
    printf("min and max value of float: %e, %e\n",FLT_MIN,FLT_MAX);
    printf("min and max value of double: %e, %e\n",DBL_MIN,DBL_MAX);
 
    print_header("math");

    const int array_len = 10;

    double sin_array[array_len];
    double exp_array[array_len];
    double log_array[array_len];

    for (int i = 0; i < array_len; i++)
      {
        double t = i / ((double) array_len);

        sin_array[i] = sin(t * PI * 2);
        exp_array[i] = exp(t * 10.0);
        log_array[i] = log(t * 10.0);
      }

    print_array("sin",sin_array,array_len);
    print_array("exp",exp_array,array_len);
    print_array("log",log_array,array_len);

    double complex z = (I + 1) * (I + 1);
    printf("\n(i + 1)^2 = %.2f + %.2fi\n",creal(z),cimag(z));

    z = exp(I * PI) + 1;     // Euler's identity
    printf("e^(i * pi) + 1 = %.2f + %.2fi",creal(z),cimag(z));

    return 0;
  }

int line_count() {return __LINE__;}
