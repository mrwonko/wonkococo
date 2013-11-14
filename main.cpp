#include <iostream>
#include <cstdlib> // EXIT_SUCCESS

#include "Regex.hpp"

#include "TemplateHelpers.hpp"

int main(int argc, char** argv)
{
	using namespace wcc;
	typedef Regex::Union <
		Regex::Concat <
			Regex::EmptyWordLanguage < char >,
			Regex::EmptyLanguage< char > // concatenating the empty language with anything results in the empty language, but whatever, this is about printing.
		>,
		Regex::Capture <
			Regex::Concat <
				Regex::Letter < char, 'W' >,
				Regex::Repeat <
					Regex::Range< char, 'a', 'z' >::type
				>
			>
		>
	> myRegularLanguage;
	std::cout << Regex::ToString< myRegularLanguage >::run() << std::endl;
	std::cout << "3x X: " << Regex::ToString<
		Regex::RepeatNTimes< Regex::Letter< char, 'X' >, 3 >::type
	>::run() << std::endl;
	std::cout << "at least 2x y: " << Regex::ToString<
		Regex::RepeatAtLeast< Regex::Letter< char, 'y' >, 2 >::type
	>::run() << std::endl;
	std::cout << "2-4x z: " << Regex::ToString<
		Regex::RepeatFromTo< Regex::Letter< char, 'z' >, 2, 4 >::type
	>::run() << std::endl;
	std::cout << "a+: " << Regex::ToString<
		Regex::OnceOrMore< Regex::Letter< char, 'a' > >::type
	>::run() << std::endl;

	// Testing Contains
	std::cout << "false: " << Helper::Contains<int, char, double, int*>::value << std::endl;
	std::cout << " true: " << Helper::Contains<int, char, double, int, bool>::value << std::endl;
	std::cout << "false: " << Helper::Contains<int>::value << std::endl;
	return EXIT_SUCCESS;
}
