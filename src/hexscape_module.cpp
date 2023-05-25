#include <Rcpp.h>

#include "example_function.h"
#include "example_templated_function.h"
#include "example_class.h"


template <class RcppModuleClassName>
RcppModuleClassName* invalidate_default_constructor() {
	Rcpp::stop("Default constructor is disabled for this class");
	return 0;
}
#define DISABLE_DEFAULT_CONSTRUCTOR() .factory(invalidate_default_constructor)


RCPP_MODULE(hexscape_module){

	using namespace Rcpp;
	
	function("Rcpp_example_function", &example_function);
  
  function("Rcpp_ex_temp_withcheck", &example_templated_function<true>);
  function("Rcpp_ex_temp_nocheck", &example_templated_function<false>);
  
	class_<ExampleClass>("ExampleClass")
	  DISABLE_DEFAULT_CONSTRUCTOR()
    .constructor<const int>("Constructor with a single integer argument")
	  .method("show", &ExampleClass::show, "A show method")
    .property("number_readonly", &ExampleClass::get_number, "Get the current number (read only)")
    .property("number_readwrite", &ExampleClass::get_number, &ExampleClass::set_number, "Get and set the current number")
	;

}
