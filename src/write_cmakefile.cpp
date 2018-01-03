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

std::string target_of_song(const Datamodel::Conf& la_conf,const std::string& filename) {
	std::string name(replace_path(filename, (la_conf.srcdir_ + "/").c_str(), ""));
	name = replace_extension(name, "");
	return "install_" + basename(dirname(filename)) + "_" + basename(name);
}

void write_cmakefile(const Datamodel::Conf& la_conf) {
	std::string cmake_filename(la_conf.builddir_ + "\\CMakeLists.txt");
	std::cout << "generate '" << cmake_filename << "'" << std::endl;

	std::ofstream fout_cmake(cmake_filename);
	if (fout_cmake.bad()) {
		throw std::runtime_error("bad file " + cmake_filename);
	}
	fout_cmake << "project(\"songs\")" << std::endl;
	fout_cmake << "cmake_minimum_required(VERSION 2.8)" << std::endl;
	fout_cmake << "#set(CMAKE_VERION 3.5)" << std::endl;

	fout_cmake << "set(LATEX_COMPILER latex)" << std::endl;
	fout_cmake << "set(PDFLATEX_COMPILER pdflatex)" << std::endl;
	fout_cmake << "set(HTLATEX_COMPILER htlatex)" << std::endl;
	fout_cmake << "set(BIBER_COMPILER biber)" << std::endl;
	fout_cmake << "set(BIBTEX_COMPILER bibtex)" << std::endl;
	fout_cmake << "set(MAKEINDEX_COMPILER makeindex)" << std::endl;

	fout_cmake << "include(UseLATEX.cmake)" << std::endl;
	fout_cmake << "set(LATEX_OUTPUT_PATH xxx-pdf/" << ")" << std::endl;


	std::function<void(const std::string&)> write_song_entry_in_makefile = [&la_conf, &fout_cmake](const std::string& filename){
		Song song;
		song.read(la_conf, filename);
		song.write(la_conf);
		std::string name(replace_path(filename, (la_conf.srcdir_ + "/").c_str(), ""));
		name = replace_extension(name, "");

		fout_cmake << "#================= song " << std::endl;
		// fout_cmake << "set(LATEX_OUTPUT_PATH xxx-pdf/" << basename(dirname(filename)) << ")" << std::endl;
		fout_cmake << "latex_get_output_path(output_dir)" << std::endl;
		fout_cmake << "add_custom_target(" << std::endl;
		fout_cmake << "  " << target_of_song(la_conf,filename) << std::endl;
		fout_cmake << "  DEPENDS ${output_dir}/" << basename(name) << ".pdf" << std::endl;
		fout_cmake << "  COMMAND mkdir -p install/" << dirname(name) << std::endl;
		fout_cmake << "  COMMAND cp ${output_dir}/" << basename(name) << ".pdf install/" << dirname(name) << "/." << std::endl;
		fout_cmake << ")" << std::endl;
		fout_cmake << "add_latex_document(" << std::endl;
		fout_cmake << "  " << name << ".tex" << std::endl;
		fout_cmake << "  FORCE_PDF" << std::endl;
		fout_cmake << "  DEPENDS" << std::endl;


		for (unsigned int count = 0; count < song.grilles_.size(); ++count) {
			fout_cmake << "    ${output_dir}/" << basename(name) << "-grille-" << count << ".mps\n";
		}
		for (unsigned int count = 0; count < song.tablatures_.size(); ++count) {
			fout_cmake << "    ${output_dir}/" << basename(name) << "-tab-" << count << ".mps\n";
		}
		fout_cmake << ")\n";

		for (unsigned int count = 0; count < song.grilles_.size(); ++count) {
			fout_cmake << "add_custom_command(\n  OUTPUT ${output_dir}/" << basename(name) << "-grille-" << count << ".mps" << std::endl;
			fout_cmake << "  DEPENDS " << name << "-grille-" << count << ".mp" << std::endl;
			fout_cmake << "  WORKING_DIRECTORY " << basename(dirname(filename)) << std::endl;
			fout_cmake << "  COMMAND mpost " << basename(name) << "-grille-" << count << std::endl;
			fout_cmake << "  COMMAND cp " << basename(name) << "-grille-" << count << ".1  ${output_dir}/" << basename(name) << "-grille-" << count << ".mps" << std::endl;
			fout_cmake << ")" << std::endl;
		}
		for (unsigned int count = 0; count < song.tablatures_.size(); ++count) {
			fout_cmake << "add_custom_command(" << std::endl;
			fout_cmake << "  OUTPUT ${output_dir}/" << basename(name) << "-tab-" << count << ".mps" << std::endl;
			fout_cmake << "  DEPENDS " << name << "-tab-" << count << ".mp" << std::endl;
			fout_cmake << "  WORKING_DIRECTORY " << basename(dirname(filename)) << std::endl;
			fout_cmake << "  COMMAND mpost " << basename(name) << "-tab-" << count << std::endl;
			fout_cmake << "  COMMAND cp " << basename(name) << "-tab-" << count << ".1  ${output_dir}/" << basename(name) << "-tab-" << count << ".mps" << std::endl;
			fout_cmake << ") " << std::endl;
		}

	};
	std::function<void(const std::string&)> write_book_entry_in_makefile = [&la_conf, &fout_cmake](const std::string& filename){
		Book book;
		book.read(la_conf, filename);
		book.write(la_conf);
		std::string name(replace_path(filename, (la_conf.srcdir_ + "/").c_str(), ""));
		name = replace_extension(name, "");
		fout_cmake << "#================= book " << std::endl;

		// fout_cmake << "set(LATEX_OUTPUT_PATH xxx-pdf/" << basename(dirname(filename)) << ")" << std::endl;
		fout_cmake << "latex_get_output_path(output_dir)" << std::endl;
		fout_cmake << "add_custom_target(" << std::endl;
		fout_cmake << "  install_book_" << basename(name) << std::endl;
		fout_cmake << "  DEPENDS ${output_dir}/" << basename(name) << ".pdf" << std::endl;
		fout_cmake << "  COMMAND mkdir -p install/" << dirname(name) << std::endl;
		fout_cmake << "  COMMAND cp ${output_dir}/" << basename(name) << ".pdf install/" << dirname(name) << "/." << std::endl;
		fout_cmake << ")" << std::endl;
		fout_cmake << "add_latex_document(" << std::endl;
		fout_cmake << "  " << name << ".tex" << std::endl;
		fout_cmake << "  FORCE_PDF" << std::endl;
		fout_cmake << "  DEPENDS" << std::endl;
		std::for_each(book.songs_.begin(), book.songs_.end(), [&fout_cmake](Book::song_info& info) {
			if (info.found_) {
				fout_cmake << "    install_" << basename(dirname(info.filename_)) << "_" << basename(replace_extension(info.filename_, "")) << std::endl;
			}
		});
		fout_cmake << ")" << std::endl;
	};
	std::function<void(bool& acc, const std::string& filename, bool)> walk = [&la_conf, &fout_cmake, &write_song_entry_in_makefile, &write_book_entry_in_makefile](bool& acc, const std::string& filename, bool is_dir) {
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
			write_song_entry_in_makefile(filename);
		}
		else if (extension(filename) == "book") {
			write_book_entry_in_makefile(filename);
		}
		else {
			std::cout << "unknown file : '" << filename << "'" << std::endl;
		}
		return;
	};

	bool ret;
	walk_tree(la_conf.srcdir_, ret, walk);

	std::function<void()> write_phony_install_books = [&fout_cmake, &la_conf](){
		fout_cmake << "#================= PHONY " << std::endl;
		fout_cmake << "add_custom_target(\n  install_books\n";
		std::function<void(bool& acc, const std::string& filename, bool)> walk_book_target = [&fout_cmake, &la_conf](bool& acc, const std::string&filename, bool is_dir){
			if (!is_dir && (extension(filename) == "book")) {
				std::string name(replace_path(filename, (la_conf.srcdir_ + "/books/").c_str(), ""));
				name = replace_extension(name, "");
				fout_cmake << "  DEPENDS install_book_" << name << std::endl;
			}
			return;
		};
		bool ret;
		walk_tree(la_conf.srcdir_, ret, walk_book_target);
		fout_cmake << ")" << std::endl;
	};

	std::function<void()> write_phony_install_songs = [&fout_cmake, &la_conf]() {
		fout_cmake << "#================= PHONY " << std::endl;
		fout_cmake << "add_custom_target(\n  install_all_songs\n";
		std::function<void(bool& acc, const std::string& filename, bool)> walk_song_target = [&fout_cmake, &la_conf](bool& acc, const std::string& filename, bool is_dir){
			if (!is_dir && (extension(filename) == "song")) {
				fout_cmake << "  DEPENDS " << target_of_song(la_conf, filename) << std::endl;
			}
			return;
		};
		bool ret;
		walk_tree(la_conf.srcdir_, ret, walk_song_target);
		fout_cmake << ")" << std::endl;
	};




	write_phony_install_books();
	write_phony_install_songs();

	std::cout << "DONE !" << std::endl;

}
