#include <iostream>
#include <cstdlib> // EXIT_SUCCESS

#include "Regex.hpp"

int main(int argc, char** argv)
{
	typedef Regex::Union <
		Regex::Concat <
			Regex::EmptyWordLanguage < char >,
			Regex::EmptyLanguage< char > // concatenating the empty language with anything results in the empty language, but whatever, this is about printing.
		>,
		Regex::Capture <
			Regex::Concat <
				Regex::Letter < char, 'W' >,
				Regex::Repeat <
					Regex::Letter < char, '!' >
				>
			>
		>
	> myRegularLanguage;
	std::cout << Regex::ToString< myRegularLanguage >::run() << std::endl;
	return EXIT_SUCCESS;
}
