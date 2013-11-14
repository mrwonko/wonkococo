#ifndef COMPILERNAMEGOESHERE_REGEX_HPP_INCLUDED
#define COMPILERNAMEGOESHERE_REGEX_HPP_INCLUDED

namespace Regex
{

/**
	@brief The empty regular language over a given alphabet
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

}

#endif // COMPILERNAMEGOESHERE_REGEX_HPP_INCLUDED
