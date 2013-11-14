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
		Helper::ContainsType<int, char, double, int*>::value
	@endcode
*/
// default: false
template < typename Needle, typename... haystack > struct ContainsType : std::false_type{};

// specialization: head of list is Needle
template < typename Needle, typename... tail > struct ContainsType < Needle, Needle, tail... > : std::true_type {};

// Recursion: If head of list is not Needle, ignore it.
template < typename Needle, typename head, typename... tail > struct ContainsType < Needle, head, tail... > : ContainsType < Needle, tail... > {};

/**
	@brief Whether a given list of numbers contains a given number

	Usage:
	@code{.cpp}
		Helper::ContainsValue<42, 2, 3, 4>::value
	@endcode
*/
// default: false
template < typename T, T Needle, T... haystack > struct ContainsValue : std::false_type{};

// specialization: head of list is Needle
template < typename T, T Needle, T... tail > struct ContainsValue < T, Needle, Needle, tail... > : std::true_type {};

// Recursion: If head of list is not Needle, ignore it.
template < typename T, T Needle, T head, T... tail > struct ContainsValue < T, Needle, head, tail... > : ContainsValue < T, Needle, tail... > {};

}

}

#endif
