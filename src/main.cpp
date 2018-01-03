#include <iostream>
#include <vector>
#include <assert.h>
#include <sstream>
#include <fstream>
#ifndef WIN32
#include <dirent.h>
#else
#include <direct.h>
#endif
#include <sys/stat.h>

#include "json11.hpp"

#include "song.h"
#include "datamodel.h"
#include "read_util.h"
#include "read_util.hc"

extern void write_cmakefile(const Datamodel::Conf& la_conf);


Datamodel::Conf read_conf(const std::string& in) {
	try {
		std::string errmsg;
		json11::Json j(json11::Json::parse(in, errmsg));
		assert(j.is_object());
		json11::Json::object o = j.object_items();
		Datamodel::Conf la_conf;
		{
			la_conf.song_ = o["filename"].string_value();
			la_conf.srcdir_ = o["srcdir"].string_value();
			la_conf.builddir_ = o["builddir"].string_value();
		}
#ifdef WIN32
	{
		char buffer[1000];
		if (_fullpath(buffer, la_conf.srcdir_.c_str(), 9999) != NULL) {
			la_conf.srcdir_ = buffer;
		}
		else {
			throw std::runtime_error("pb avec fullpath");
		}
	}
#else
		if (! path_is_absolute(la_conf.srcdir_)) {
			std::ostringstream oss ;
			oss << "srcdir is not absolute : '" << la_conf.srcdir_ << "'" << std::endl ;
			throw std::runtime_error(oss.str()) ;
		}
#endif


#ifdef WIN32
	{
		char buffer[1000];
		if (_fullpath(buffer, la_conf.builddir_.c_str(), 9999) != NULL) {
			la_conf.builddir_ = buffer;
		}
		else {
			throw std::runtime_error("pb avec fullpath");
		}
	}
#else
		if (!path_is_absolute(la_conf.builddir_)) {
			std::ostringstream oss ;
			oss << "builddir is not absolute : '" << la_conf.builddir_ << "'" << std::endl ;
			throw std::runtime_error(oss.str()) ;
		}
#endif
		return la_conf;
	}
	catch (std::runtime_error& e) {
		std::cout << e.what() << std::endl; exit(1);
	}
	catch (...) {
		std::cout << "caught unknown" << std::endl; exit(1);
	}
}



void test() {
	try {
		WIN32_FIND_DATA ffd;
		HANDLE hFind = INVALID_HANDLE_VALUE;
		DWORD dwError = 0;

		std::string s = "d:\\users\\t0005634\\Documents\\work\\fantastic-palm-tree\\songs\\*";

		hFind = FindFirstFile(s.c_str(), &ffd);
		if (hFind == INVALID_HANDLE_VALUE) {
			throw std::runtime_error("bad file " + s);
		}

		for (;;) {
			if (FindNextFile(hFind, &ffd) == 0) { break; }
			// std::cout << "found '" << ffd.cFileName << "'" << std::endl;
			if (ffd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)   {
				// std::cout << "------- is a directory " << std::endl;
			}
			else {
				// std::cout << "------- is a file " << std::endl;
			}
		}
	}
	catch (std::exception& e) {
		std::cout << "caught " << e.what() << std::endl;
	}
	catch (...) {
		std::cout << "caught unknown exception" << std::endl;
	}
};




int main(int argc, char** argv) {
	try {
		std::cout << __FILE__ << ":" << __LINE__ << std::endl;
		//  rude::Socket socket ;

		//test();
		//exit(1);

		assert(argc > 0);
		std::ifstream file(argv[1], std::ios::binary);
		if (file.fail()) {
			perror(argv[1]);
			return 1;
		}
		std::basic_string<char> buffer((std::istreambuf_iterator<char>(file)), (std::istreambuf_iterator<char>()));
		std::cout << "buffer : " << buffer << std::endl;
		Datamodel::Conf la_conf = read_conf(buffer);

		write_cmakefile(la_conf);


		return 0;
	}
	catch (std::exception& e) {
		std::cout << __FILE__ << ":" << __LINE__ << std::endl;
		std::cout << "caught : " << e.what() << std::endl;
		return 1;
	}
	catch (...) {
		std::cout << "caught unkonwn" << std::endl;
		return 1;
	}

}

