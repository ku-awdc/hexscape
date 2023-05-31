#include <Rcpp.h>

#include "example_function.h"
#include "example_templated_function.h"
#include "connectedness.h"


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

  using Conn1 = Connectedness<false>;
	class_<Conn1>("RcppConnectedness")
	  DISABLE_DEFAULT_CONSTRUCTOR()
    .constructor<const int, const Rcpp::NumericMatrix>("Constructor")
	  .method("get_connectedness", &Conn1::get_connectedness, "A show method")
	;

}
