/*
  Show off the capabilities of C++ language with only
  standard library.

  Miloslav Ciz, 2017
  WTFPL license
*/

#include <iostream>
#include <string>
#include <vector>
#include <random>
#include <chrono>
#include <algorithm>
#include <complex>
#include <cmath>
#include <utility>

#define PI 3.1415926535

using namespace std;
using namespace chrono;

#define FACTORIAL_OF 10
#define SORT_LENGTH 10000

int line_count();     // forward declaration

high_resolution_clock measure_clock;
high_resolution_clock::time_point t1;
high_resolution_clock::time_point t2;

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

class MyClass
  {
    public:
      MyClass(string name) // constructor
        {
          this->name = name;
          MyClass::object_count++;

          cout << "Hi, I'm an object, my name is " << name <<
            " and there is " << MyClass::object_count << " of us." << endl;
        }

      ~MyClass() // destructor
        {
          cout << this->name << " says bye." << endl;
        }

      static int object_count;

    protected:
      string name;
  };

int MyClass::object_count = 0;

void print_header(string title, char character='-')
  {
    cout << endl << title << endl;

    for (int i = 0; i < (int) title.size(); i++)
      cout << character;

    cout << endl;
  }

template<typename T>
void print_vector(vector<T> v)
  {
    for (int i = 0; i < (int) v.size(); i++)
      cout << v[i] << " ";
  }

void time_measure_begin()
  {
    t1 = measure_clock.now();
  }

double time_measure_end()
  {
    t2 = measure_clock.now();
    duration<double> time_span = duration_cast<duration<double>>(t2 - t1);
    return time_span.count();
  }

void bubble_sort(int *data, int data_len)
  {
    for (int i = 0; i < data_len; i++)
      for (int j = 0; j < data_len - i - 1; j++)
        if (data[j] > data[j + 1])
          swap<int>(data[j],data[j + 1]);
  }

int divide(int a, int b)
  {
    if (b == 0)
      throw "Can't divide by zero!";

    return a / b;
  }

int main()
  {
    cout << "Showing off the power of C++!" << endl;
    print_header("general");
    
    cout << "My source code file is called " << __BASE_FILE__ << " and has " << line_count() << " lines. This is line " << __LINE__ << "." << endl;

    MyClass object1("object1"), object2("object2");

    vector<int> v;

    for (int i = 0; i < 10; i++)
      v.push_back(rand() % 10 + 1);

    cout << "sequence of random numbers from 1 to 10: ";
    print_vector(v);
    cout << endl;

    shuffle(v.begin(),v.end(),default_random_engine(0));

    cout << "the same sequence randomly shuffled: ";
    print_vector<int>(v);
    cout << endl;

    sort(v.begin(),v.end());

    cout << "and the same sequence sorted: ";
    print_vector<int>(v);
    cout << endl;

    cout << "factorial of " << FACTORIAL_OF << " recursively: ";
    time_measure_begin();
    factorial_recursive(FACTORIAL_OF);
    double dt = time_measure_end();
    cout << dt << " seconds" << endl;

    cout << "factorial of " << FACTORIAL_OF << " iteratively: ";
    time_measure_begin();
    factorial_iterative(FACTORIAL_OF);
    dt = time_measure_end();
    cout << dt << " seconds" << endl;

    int sort_data[SORT_LENGTH];

    for (int i = 0; i < SORT_LENGTH; i++)
      sort_data[i] = (SORT_LENGTH - i) % (SORT_LENGTH / 4);

    time_measure_begin();
    bubble_sort(sort_data,SORT_LENGTH);
    dt = time_measure_end();

    cout << "It took me " << dt << " seconds to sort an array of length " << SORT_LENGTH << " with bubble sort." << endl;    

    try
      {
        cout << "If I try to divide by zero... ";
        divide(10,0);
      }
    catch (const char *message)
      {
        cout << "there will be an exception: " << message << endl;
      }

    print_header("math");

    cout << "e^(i * pi) + 1 = " <<  (exp(complex<double>(0,1) * PI) + 1.0) << endl;
    
    vector<double> v2;

    for (int i = 0; i < 10; i++)
      v2.push_back(sin(i / 10.0 * 2 * PI));

    cout << "sin: ";
    print_vector<double>(v2);
    cout << endl;

    cout << "----------------" << endl;
    return 0;
  }

int line_count() {return __LINE__;}

