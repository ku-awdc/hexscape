/*
  
  This is an example function definition
  
*/

#include "example_function.h"

#include <Rcpp.h>

double example_function(const double a, const double b)
{
  
  const double c = a * b;
  
  return c;
  
}
