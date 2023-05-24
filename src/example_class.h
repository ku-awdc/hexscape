/*
  
  This is an example class
  
*/

#ifndef EXAMPLE_CLASS_H
#define EXAMPLE_CLASS_H

#include <Rcpp.h>

class ExampleClass
{
public:
  ExampleClass() = delete;
  ExampleClass(const int number) : m_number(number)
  {
    Rcpp::Rcout << "Making an ExampleClass object with number " << m_number << "\n";
  }
  
  void show() const
  {
    Rcpp::Rcout << "An ExampleClass object with number " << m_number << "\n";
  }
  
  int get_number() const
  {
    return m_number;
  }
  
  void set_number(const int number)
  {
    m_number = number;
  }
  
  ~ExampleClass()
  {
    Rcpp::Rcout << "Destroying ExampleClass object\n";
  }
  
private:
  int m_number;
  
};

#endif // EXAMPLE_CLASS_H
