#include <iostream>
#include <vector>
#include <assert.h>
#include <sstream>
#include <fstream>
#include <functional>
#include <ctime>
#include <iomanip>

#include "song.h"
#include "datamodel.h"
#include "read_util.h"

namespace Datamodel {
std::string concat_filenames(const std::string& n1,const std::string& n2) {
    std::string ret ;
    ret = n1 + "/" + n2 ;
    return ret ;
}
}


void Song::read(const std::string& filename) {
  std::function<void(std::ifstream&)> r =
    [&r,this](std::ifstream& fin) {
        char line[1001] ;
        try {
            if (fin.eof()) {
                std::cout << "EOF" << std::endl ;
                return ;
            }
            if (fin.bad()) return ;
            if (fin.fail()) return ;
            fin.getline(line,1000) ;
            auto t=my_split(line) ;
            std::string word = std::get<0>(t) ;
	    std:: cout << "word : '" << word << "'" << std::endl ;
            std::string arg = std::get<1>(t) ;
            if ( word == "\\titre" ) {
	      titre_ = read_string_until_empty_line(fin) ;
            }
            else if ( word == "\\auteur" ) {
	      auteur_ = read_string_until_empty_line(fin) ;
            }
            else if ( word == "\\grille" ) {
	      grille_.push_back(Grille(fin,arg)) ;
            }
            else if ( word == "\\lyrics" ) {
	      std::cout << "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL" << std::endl ;
	      Lyrics l ;
	      l.nb_cols_ = 1 ;
	      l.title_ = arg ;
	      l.data_ = read_array_until_empty_line(fin) ;
	      lyrics_.push_back(l) ;
            }
            else if ( word == "\\lyrics2" ) {
	      std::cout << "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL" << std::endl ;
	      Lyrics l ;
	      l.nb_cols_ = 2 ;
	      l.title_ = arg ;
	      l.data_ = read_array_until_empty_line(fin) ;
	      lyrics_.push_back(l) ;
            }
            r (fin) ;
        }
        catch (std::exception& e) {
            std::cout << __FILE__ << ":" << __LINE__ << " ; caught " << e.what()  << std::endl ;
        }
        catch (...) {
            std::cout << __FILE__ << ":" << __LINE__ << " ; caught unknown" << std::endl ;
        }
    } ;


  filename_ = filename ;

  std::ifstream fin(filename,std::ios::binary) ;
  r(fin) ;
  return ;
}


void write_preamble(std::ofstream& fout) {

  fout << "\
\\documentclass[a4paper,portrait]{article}\n\
\\usepackage[utf8]{inputenc}\n\
\\usepackage[T1]{fontenc}\n\
%%\\usepackage{wasysym}\n\
\\usepackage{soulutf8}\n\
\\usepackage{fixltx2e}\n\
%%\\usepackage[pdftex,colorlinks=true,urlcolor=blue,linkcolor=blue]{hyperref}\n\
\\usepackage{hyperref}\n\
\\usepackage{diagbox}\n\
%%\\usepackage{caption}\n\
%%\\usepackage[scale=0.8]{geometry}\n\
\\usepackage[a4paper,inner=1cm,outer=1cm,top=2cm,bottom=2cm]{geometry} \n\
\\usepackage{color,soul}\n\
\\definecolor{grey}{rgb}{0.7,0.7,0.7}\n\
\\definecolor{grey8}{rgb}{0.8,0.8,0.8}\n\
\\definecolor{colora}{rgb}{0.9,0.7,0.9}\n\
\\sethlcolor{grey8}\n\
\\usepackage[pdftex]{graphicx}\n\
%%\\usepackage{lmodern}\n\
%%\\usepackage{textcomp}\n\
%%\\usepackage{kpfonts}\n\
\\usepackage{array,multirow}\n\
\\usepackage{fancyhdr} \n\
\\pagestyle{fancy} \n\
\\usepackage{makeidx}\n\
\\newcolumntype{M}[1]{>{\\centering\\arraybackslash}m{#1}}\n\
%%\\newcolumntype{M}[1]{>{\\centering}m{#1}}\n\
\\newcolumntype{N}{@{}m{0pt}@{}}\n\
\\renewcommand{\\arraystretch}{1.2}\n\
\\usepackage{multicol}\n\
%%\\usepackage{french}\n\
\\setlength{\\columnseprule}{1pt}\n\
\\setlength{\\columnsep}{0cm}\n\
\\usepackage{lastpage}\n\
\\usepackage{needspace}\n\
\\usepackage{titletoc}\n\
\\hypersetup{ pdftitle={}, pdfauthor={},bookmarks=true, bookmarksopen=true,pdftoolbar=true, pdffitwindow=false,colorlinks=false,linkcolor=red, citecolor=red,filecolor=magenta,urlcolor=black }\n\
%%\\usepackage{bookmark}\n\
\\usepackage[metapost,truebbox,mplabels]{mfpic}\n\
\n\
%% de la doc de multicols\n\
%%\\setlength{\\textwidth}{39pc}\n\
%%\\setlength{\\textheight}{54pc}\n\
%%\\setlength{\\parindent}{1em}\n\
%%\\setlength{\\parskip}{0pt plus 1pt}\n\
%%\\setlength{\\oddsidemargin}{0pc}\n\
%%\\setlength{\\marginparwidth}{0pc}\n\
%%\\setlength{\\topmargin}{-2.5pc}\n\
%%\\setlength{\\headsep}{20pt}\n\
\n\
\n\
\\newcommand\\invisiblesection[1]{%%\n\
\\refstepcounter{section}%%\n\
\\addcontentsline{toc}{section}{\\protect\\numberline{\\thesection}#1}%%\n\
\\sectionmark{#1}} \n\
\\newcommand*{\\authorfont}{\\fontfamily{ptm}\\fontsize{20}{25}\\fontshape{it}\\selectfont} \n\
\\newcommand*{\\titlefont}{\\fontfamily{ptm}\\fontsize{30}{35}\\fontshape{it}\\selectfont} \n\
\\newcommand*{\\commentfont}{\\fontfamily{ptm}\\fontsize{12}{15}\\fontshape{it}\\selectfont} \n\
\\newcommand*{\\commentafont}{\\fontfamily{ptm}\\fontsize{12}{15}\\fontshape{it}\\selectfont} \n\
\\newcommand*{\\lyricstitlefont}{\\color{black}\\fontfamily{ptm}\\fontsize{15}{15}\\fontshape{it}\\selectfont} \n\
\\newenvironment{lyricsfont}{\\fontfamily{ptm}\\fontsize{12}{12}\\selectfont}{} \n\
\\newenvironment{grillefont}{\\color{blue}\\fontfamily{ptm}\\fontsize{15}{15}\\selectfont}{} \n\
\\newcommand*{\\tabbox}[2][t]{%%\n\
    \\vspace{0pt}\\parbox[#1][3.7\\baselineskip]{0.5cm}{\\strut#2\\strut}}\n\
" ;

  std::time_t t = std::time(nullptr) ;
  std::tm* tm = std::localtime(&t) ;
  fout << "\\fancyfoot[L]{généré le " 
       << std::setw(2) << std::setfill('0') << tm->tm_mday << "/" 
       << std::setw(2) << std::setfill('0') << (tm->tm_mon+1) << "/" 
       << std::setw(4) << (tm->tm_year+1900) << "} \n\
" ;

  fout << "\\fancyfoot[C]{} \n\
\\fancyfoot[R]{page \\thepage~sur \\pageref{LastPage}} \n\
\\renewcommand\\headrulewidth{0.02in}\n\
\\renewcommand\\footrulewidth{0.02in}\n\
\\Needspace{5\\baselineskip}\n\
\n\
\\def\\mystrut(#1,#2){\\vrule height #1 depth #2 width 0pt}\n\
\\newcolumntype{C}[1]{%%\n\
   >{\\mystrut(10ex,10ex)\\centering}%%\n\
   p{#1}%%\n\
   <{}}  \n\
" ;
}

const void Song::write() {
  std::string path = replace_extension(filename_,".tex") ;
  std::cout << "write song " << path << std::endl ;

  std::function<void(std::ofstream&)> write_1 =
    [this](std::ofstream&fout) {
    fout << "\
\\title{" << this->titre_ << "}\n\
\\author{" << this->auteur_ << "}\n\
\\fancyhead[L]{{\\titlefont " << this->titre_ << "} }\n\
\\fancyhead[R]{{\\authorfont "<<  this->auteur_ << "}}\n\
\\fancyhead[C]{} \n\
\\begin{document}\n\
"
    ;
  } ;



  std::function<void(std::ofstream&)>write_lyrics =
    [this](std::ofstream&fout) {
    fout << "LLLLLLLLLLLLLL" << this->lyrics_.size() << std::endl; 
    fout << "\\begin{multicols}{2}\n" ;
    for (auto l:this->lyrics_) {
      fout << "\\begin{lyricsfont}\n" ;
      fout << "\\begin{verse}\n" ;
      fout << "{\\commentfont \\hl{" << tex_of_string(l.title_) << "}}\n" ;
      for (auto line:l.data_) {
	if (line=="\\") {
	  fout << "\n\\end{verse}\n\\begin{verse}\n" ;
	} else {
	  fout << line << std::endl; 
	}
      }
      fout << "\\end{verse}\n" ;
      fout << "\\end{lyricsfont}\n" ;
    }
    fout << "\\end{multicols}\n" ;
  } ;



  std::ofstream fout(path) ;
  write_preamble(fout) ;
  write_1(fout) ;
  write_lyrics(fout) ;
  fout << "\\end{document}\n" ;
}
