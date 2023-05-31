/*
  
  This is an example templated function
  
*/

#ifndef EXAMPLE_TEMPLATED_FUNCTION_H
#define EXAMPLE_TEMPLATED_FUNCTION_H

#include <Rcpp.h>

template<bool T_checking>
double example_templated_function(const double a, const double b)
{
  
  // Potentially computationally intensive check code, that will only be implemented for T_checking = true
  if constexpr (T_checking)
  {
    if (a < 0.0 || b < 0.0)
    {
      Rcpp::stop("Both arguments must be strictly positive!");
    }
  }
  
  const double rv = a * b;
  
  return rv;
  
}

#endif // EXAMPLE_TEMPLATED_FUNCTION_H
