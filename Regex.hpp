#ifndef WONKOCOCO_REGEX_HPP_INCLUDED
#define WONKOCOCO_REGEX_HPP_INCLUDED

#include <string>
#include <sstream>
#include <limits>
#include "TemplateHelpers.hpp"

namespace wcc
{

namespace Regex
{

//			Types

/**
	@brief The empty regular language over a given alphabet

	@todo Of no practical relevance? Delete?
*/
template < typename Alphabet > struct EmptyLanguage
{
	typedef Alphabet alphabet;
};

/**
	@brief The regular language over a given alphabet consisting of the empty word
*/
template < typename Alphabet > struct EmptyWordLanguage
{
	typedef Alphabet alphabet;
};

/**
	@brief The regular language over a given alphabet consisting of the word consisting of a given letter

	@note Not called SingleLetterWordLanguage for brevity.
**/
template < typename Alphabet, Alphabet letter_ > struct Letter
{
	typedef Alphabet alphabet;
	static const Alphabet letter = letter_;
};

/**
	@brief The regular language consisting of the union of two given regular languages

	@pre The given regular languages must be over the same alphabet
*/
template < typename language1, typename language2 > struct Union
{
	// C++14's concept may allow for nicer errors in case the given argument has no alphabet typedef, for now this will error horribly.
	// The below workaround doesn't work since the second parameter needs type arguments
	/*
	static_assert
		( std::is_same< language1, EmptyLanguage >::value
		|| std::is_same< language1, EmptyWordLanguage >::value
		|| std::is_same< language1, Letter >::value
		|| std::is_same< language1, Union >::value
		|| std::is_same< language1, Concat >::value
		|| std::is_same< language1, Repeat >::value
		, "Parameter 1 to union must be a regular language!"
		);
	*/
	static_assert
		( std::is_same< typename language1::alphabet, typename language2::alphabet >::value
		, "The regular languages in a union must be over the same alphabet!"
		);
	typedef typename language1::alphabet alphabet;
	typedef language1 lang1;
	typedef language2 lang2;
};

/**
	@brief The regular language consisting of the concatenation of the words of two given regular languages

	Where the first part comes from the first language and the second part from the second.

	@pre The given regular languages must be over the same alphabet
*/
template < typename language1, typename language2 > struct Concat
{
	static_assert( std::is_same< typename language1::alphabet, typename language2::alphabet >::value, "The regular languages in a concatenation must be over the same alphabet!" );
	typedef typename language1::alphabet alphabet;
	typedef language1 lang1;
	typedef language2 lang2;
};

/**
	@brief The regular language consisting of 0 or more concatenations of words of the given language
*/
template < typename language > struct Repeat
{
	typedef typename language::alphabet alphabet;
	typedef language lang;
};

/**
	@brief Capturing a word in a regular language

	For evaluation - if you need read parts later, capture them. (E.g. identifiers.)
*/
template < typename language > struct Capture
{
	typedef typename language::alphabet alphabet;
	typedef language lang;
};

//			Convenience types

/**
	@brief Something optional

	Usage:
	@code{.cpp}
		typedef optional< lang1 >::type lang2;
	@endcode
*/
template < typename language > struct Optional
{
	typedef Union< EmptyWordLanguage < language >, language > type;
};

/**
	@brief Shorthand for any of a continuous range of characters

	i.e. a-c == a|b|c

	Usage:
	@code{.cpp}
		typedef Range< char, 'a', 'z' >::type myLanguage;
	@endcode
*/
template < typename Alphabet, Alphabet min, Alphabet max > struct Range
{
	static_assert( min < max, "Range minimum must be less than Range maximum" );
	typedef Union< Letter< Alphabet, min >, typename Range< Alphabet, min + 1, max >::type > type;
};

template < typename Alphabet, Alphabet letter > struct Range < Alphabet, letter, letter >
{
	typedef Letter < Alphabet, letter > type;
};

/**
	@brief Repeating something N times

	i.e. a{4} = aaaa

	Usage:
	@code{.cpp}
		typedef RepeateNTimes< language1, 5 >::type language2;
	@endcode
*/
template < typename language, size_t repetitions > struct RepeatNTimes
{
	typedef Concat< language, typename RepeatNTimes< language, repetitions - 1 >::type > type;
};

template < typename language > struct RepeatNTimes < language, 1 >
{
	typedef language type;
};

template < typename language > struct RepeatNTimes < language, 0 >
{
	typedef EmptyWordLanguage < typename language::alphabet > type;
};

/**
	@brief Repeating something at least N times
*/
template < typename language, size_t minRepetitions > struct RepeatAtLeast
{
	typedef Concat< language, typename RepeatAtLeast< language, minRepetitions - 1 >::type > type;
};

template < typename language > struct RepeatAtLeast < language, 0 >
{
	typedef Repeat< language > type;
};

/**
	@brief Repeating something N to M times
*/
template < typename language, size_t minRepetitions, size_t maxRepetitions > struct RepeatFromTo
{
	static_assert( minRepetitions < maxRepetitions, "Minimum comes first in RepeatFromTo!" );
	typedef Concat < language, typename RepeatFromTo < language, minRepetitions - 1, maxRepetitions - 1 >::type > type;
};

template < typename language, size_t n > struct RepeatFromTo < language, n, n >
{
	typedef typename RepeatNTimes< language, n >::type type;
};

template < typename language, size_t n > struct RepeatFromTo < language, 0, n >
{
	typedef Concat < typename Optional< language >::type, typename RepeatFromTo < language, 0, n - 1 >::type > type;
};

// since both of the above would have to deduce 1 parameter for RepeatFromTo<X, 0, 0>, they're ambiguous. Hence this specialization:
template < typename language > struct RepeatFromTo < language, 0, 0 >
{
	typedef typename RepeatNTimes< language, 0 >::type type;
};

/**
	@brief Shorthand for once or more, in case you dislike repeating yourself

	You could just write a|a*
*/

template < typename language > struct OnceOrMore
{
	typedef Concat< language, Repeat< language > > type;
};

/**
	@brief Shorthand for any possible except these

	You could write
	@code{.cpp}
		Concat< Range<char, 0, 'a'-1>::type, Range<char, 'a'+1, MAX_CHAR>::type>
	@endcode
	or
	@code{.cpp}
		AnyBut<char, 'a'>::type.
	@endcode
*/

template < typename Alphabet, Alphabet... exceptions > struct AnyBut;
// TODO
// basic idea: iterate from std::numeric_limits< Alphabet >::min() to ...::max(), take those for which Helper::Contains<Cur, exceptions...>::value == false

//			Functions

/**
	@brief Convert a given RegEx to an std::string representation.

	Captures are displayed as <...>

	Use like this:
	@code{.cpp}
		std::cout << Regex::ToString< myLanguage >::run() << std::endl;
	@endcode
*/

template < typename _ > struct ToString;

template < typename Alphabet > struct ToString < EmptyLanguage < Alphabet > >
{
	static std::string run()
	{
		return "<empty language>";
	}
};

template < typename Alphabet > struct ToString < EmptyWordLanguage < Alphabet > >
{
	static std::string run()
	{
		return "";
	}
};

template < typename Alphabet, Alphabet letter > struct ToString < Letter < Alphabet, letter > >
{
	static std::string run()
	{
		std::stringstream ss;
		ss << letter;
		std::string s( ss.str() );
		if( s == "|" || s == "(" || s == ")" || s == "." || s == "\\" || s == "<" || s == ">" || s == "*" )
		{
			return '\\' + s;
		}
		else
		{
			return s;
		}
	}
};

template < typename lang1, typename lang2 > struct ToString < Union < lang1, lang2 > >
{
	static std::string run()
	{
		return '(' + ToString< lang1 >::run() + ")|(" + ToString< lang2 >::run() + ')';
	}
};

template < typename lang1, typename lang2 > struct ToString < Concat < lang1, lang2 > >
{
	static std::string run()
	{
		return '(' + ToString< lang1 >::run() + ").(" + ToString< lang2 >::run() + ')';
	}
};

template < typename lang > struct ToString < Repeat < lang > >
{
	static std::string run()
	{
		return '(' + ToString< lang >::run() + ")*";
	}
};

template < typename lang > struct ToString < Capture < lang > >
{
	static std::string run()
	{
		return '<' + ToString< lang >::run() + '>';
	}
};

}

}

#endif // COMPILERNAMEGOESHERE_REGEX_HPP_INCLUDED
