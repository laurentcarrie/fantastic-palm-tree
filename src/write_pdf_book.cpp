#include <iostream>
#include <vector>
#include <assert.h>
#include <sstream>
#include <fstream>
#include <functional>
#include <ctime>
#include <iomanip>
#include <numeric>
#ifdef WINDOWS
#include <Windows.h>
#endif
#include "song.h"
#include "datamodel.h"
#include "read_util.h"

extern void write_preamble(std::ofstream&);

const void Book::write(const Datamodel::Conf& la_conf)  {


	std::string path(replace_extension(filename_, ".tex"));
	path = replace_path(path, la_conf.srcdir_.c_str(), la_conf.builddir_.c_str());
	std::cout << "Write tex file '" << path << "'" << std::endl;
	std::ofstream fout(path);
	write_preamble(fout);

	fout << "\\title{" << title_ << "}" << std::endl;
	fout << "\
\\makeatletter\n\
\\let\\latexl@section\\l@section\n\
\\def\\l@section#1#2{\\begingroup\\let\\numberline\\@gobble\\latexl@section{ #1 }{#2}\\endgroup}\n\
\\makeatother\n\
\\makeindex\n\
\\begin{document}\n\
\\maketitle\n\
%%\\begin{ multicols }{2}\n\
%%\\end{ multicolumns }\n\
%%\\titlecontents{ section }[0em]\n\
% %{\\vskip 0.5ex}%%\n\
%%{}%% numbered sections formatting\n\
%%{}%% unnumbered sections formatting\n\
%%{}%%\n\
\n\
\\begin{multicols}{4}\n\
\\tableofcontents{}\n\
\\end{multicols}\n\
" ;

	if (print_index_) {
		fout << "\n\
				\\printindex\n\
				";
	}

	std::function<void(song_info)> write_song = [&fout,&la_conf](song_info info) {
		if (info.found_) {
			Song* song = info.song_;
			fout << "\n\
\\clearpage\n\
\\index{" << song->auteur_ << "!" << song->titre_ << "}\n\
%%\\pdfbookmark[1]{" << song->titre_ << "}{" << song->titre_ << "}\n\
%%\\invisiblesection{" << song->titre_ << "}\n\
\\fancyhead[L]{{\\invisiblesection{" << song->titre_ << " (" << song->auteur_ << " )} \\titlefont " << song->titre_ << "} } \n\
\\fancyhead[R]{{\\authorfont " << song->auteur_ << "}} \n\
\\fancyhead[C]{} \n\
" ;
			song->write_body(la_conf, fout);
			}
		else {
			fout << "\n\
\\clearpage\n\
\\section{" << info.filename_ <<  "(non trouv\\'e)}\n\
\\fancyhead[L]{{\\titlefont " << info.filename_ << "} } \n\
\\fancyhead[R]{{\\authorfont " "}} \n\
\\fancyhead[C]{} \n\
no such song : '" << info.filename_<< "'\n" ;
			}
	};

	fout << "%% nb songs : " << songs_.size() << std::endl;
std::for_each(songs_.begin(), songs_.end(), write_song);

fout << "\n\\end{document}\n";
}

