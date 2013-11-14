#include <iostream>
#include <cstdlib> // EXIT_SUCCESS

#include "Regex.hpp"

int main(int argc, char** argv)
{
	typedef Regex::Union <
		Regex::EmptyWordLanguage < char >,
		Regex::Letter < char, 'W' >
	> myRegularLanguage;
	std::cout << myRegularLanguage::lang2::letter << std::endl;
	return EXIT_SUCCESS;
}
