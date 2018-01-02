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


		std::string cmake_filename(la_conf.builddir_ + "\\CMakeLists.txt");
		std::cout << "generate '" << cmake_filename << "'" << std::endl;

		std::ofstream fout_cmake(cmake_filename);
		if (fout_cmake.bad()) {
			throw std::runtime_error("bad file " + cmake_filename);
		}
		fout_cmake << "\
					  project(\"songs\")\n\
					  cmake_minimum_required(VERSION 2.8)\n\
					  #set(CMAKE_VERION 3.5)\n\
					  include(UseLATEX.cmake)\n\
					  ";


		std::function<void(bool& acc, const std::string& filename, bool)> walk =
			[&la_conf, &fout_cmake](bool& acc, const std::string& filename, bool is_dir) {
			if (is_dir) {
				std::string builddir(replace_path(filename, la_conf.srcdir_.c_str(), la_conf.builddir_.c_str()));
				std::cout << "mkdir '" << builddir << "'" << std::endl;
#ifdef WIN32
				_mkdir(builddir.c_str());
#else
				xxxx
					mkdir(builddir.c_str(), 077);
#endif

			}
			else if (extension(filename) == "song") {
				Song song;
				song.read(la_conf, filename);
				song.write(la_conf);
				std::string name(replace_path(filename, (la_conf.srcdir_ + "/").c_str(), ""));
				name = replace_extension(name, "");
				fout_cmake << "\n\
							  \n\
							  set(LATEX_OUTPUT_PATH xxx-pdf/" << basename(dirname(filename)) << ")\n\
							  latex_get_output_path(output_dir)\n\
							  \n\
							  add_custom_target(\n\
							  install_" << basename(name) << " ALL\n\
							  DEPENDS ${output_dir}/" << basename(name) << ".pdf\n\
							  COMMAND mkdir -p install/" << dirname(name) << "\n\
							  COMMAND cp ${output_dir}/" << basename(name) << ".pdf install/" << dirname(name) << "/.\n\
							  )\n\
							  \n\
							  add_latex_document(\n\
							  " << name << ".tex \n\
							  FORCE_PDF \n\
							  DEPENDS \n\
							  ";


				for (unsigned int count = 0; count < song.grilles_.size(); ++count) {
					fout_cmake << "${output_dir}/" << basename(name) << "-grille-" << count << ".mps\n";
				}
				for (unsigned int count = 0; count < song.tablatures_.size(); ++count) {
					fout_cmake << "${output_dir}/" << basename(name) << "-tab-" << count << ".mps\n";
				}
				fout_cmake << ")\n";

				for (unsigned int count = 0; count < song.grilles_.size(); ++count) {
					fout_cmake << "\n\
								  add_custom_command(\n\
								  OUTPUT ${output_dir}/" << basename(name) << "-grille-" << count << ".mps\n\
								  DEPENDS " << name << "-grille-" << count << ".mp\n\
								  WORKING_DIRECTORY " << basename(dirname(filename)) << "\n\
								  COMMAND mpost " << basename(name) << "-grille-" << count << "\n\
								  COMMAND cp " << basename(name) << "-grille-" << count << ".1  ${output_dir}/" << basename(name) << "-grille-" << count << ".mps \n \
								  )\n ";
				}
				for (unsigned int count = 0; count < song.tablatures_.size(); ++count) {
					fout_cmake << "\n\
								  add_custom_command(\n\
								  OUTPUT ${output_dir}/" << basename(name) << "-tab-" << count << ".mps\n\
								  DEPENDS " << name << "-tab-" << count << ".mp\n\
								  WORKING_DIRECTORY " << basename(dirname(filename)) << "\n\
								  COMMAND mpost " << basename(name) << "-tab-" << count << "\n\
								  COMMAND cp " << basename(name) << "-tab-" << count << ".1  ${output_dir}/" << basename(name) << "-tab-" << count << ".mps \n \
								  )\n ";
				}

			}
			else {
				std::cout << "unknown file : '" << filename << "'" << std::endl;
			}
			return;
		};

		bool ret;
		walk_tree(la_conf.srcdir_, ret, walk);

		std::cout << "DONE !" << std::endl;
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

