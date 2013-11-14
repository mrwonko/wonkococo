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
		Helper::ContainsNumber<42, 2, 3, 4>::value
	@endcode
*/
// default: false
template < size_t Needle, size_t... haystack > struct ContainsNumber : std::false_type{};

// specialization: head of list is Needle
template < size_t Needle, size_t... tail > struct ContainsNumber < Needle, Needle, tail... > : std::true_type {};

// Recursion: If head of list is not Needle, ignore it.
template < size_t Needle, size_t head, size_t... tail > struct ContainsNumber < Needle, head, tail... > : ContainsNumber < Needle, tail... > {};

}

}

#endif
