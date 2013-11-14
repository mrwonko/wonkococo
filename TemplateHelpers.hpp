#ifndef WONKOCOCO_TEMPLATE_HELPERS_HPP_INCLUDED
#define WONKOCOCO_TEMPLATE_HELPERS_HPP_INCLUDED

#include <type_traits>

namespace wcc
{

namespace Helper
{

/**
	@brief Whether a given list of types contains a given type

	Usage:
	@code{.cpp}
		Helper::Contains<int, char, double, int*>::value
	@endcode
*/
// default: false
template < typename Needle, typename... haystack > struct Contains : std::false_type{};

// specialization: head of list is Needle
template < typename Needle, typename... tail > struct Contains < Needle, Needle, tail... > : std::true_type {};

// Recursion: If head of list is not Needle, ignore it.
template < typename Needle, typename head, typename... tail > struct Contains < Needle, head, tail... > : Contains < Needle, tail... > {};

}

}

#endif
