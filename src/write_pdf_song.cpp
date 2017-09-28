#include <iostream>
#include <vector>
#include <assert.h>
#include <sstream>
#include <fstream>
#include <functional>
#include <ctime>
#include <iomanip>
#include <numeric>

#include "song.h"
#include "datamodel.h"
#include "read_util.h"

extern Datamodel::Accord chord_of_string(const std::string&) ;

std::string replace_chord(const std::string& line) {
  std::function<void (std::string& line)> r = 
    [&r](std::string& line) {
    auto pos1 = line.find('[') ;
    if (pos1==std::string::npos) return ;
    auto pos2 = line.find(']') ;
    if (pos2==std::string::npos) return ;
    
    std::string before = line.substr(0,pos1) ;
    std::string after = line.substr(pos2+1,line.size()-pos2-1) ;
    std::string chord_and_word = line.substr(pos1+1,pos2-pos1-1) ;

    auto pos3 = chord_and_word.find(';') ;
    std::string chord = chord_and_word.substr(0,pos3) ;
    std::string word = chord_and_word.substr(pos3+1,chord_and_word.size()-pos3-1) ;

    std::cout << "before '" << before << "'" << std::endl; 
    std::cout << "chord_and_word '" << chord_and_word << "'" << std::endl; 
    std::cout << "chord '" << chord << "'" << std::endl; 

    Datamodel::Accord a(chord_of_string(chord)) ;
    {
      std::ostringstream oss ;
      std::cout << "note : '" << a.t_.chord_.note_ << "'" << std::endl ;
      oss << "\\textsuperscript{\\textcolor{red}{" << a.t_.chord_.note_ ;
      switch (a.t_.chord_.alteration_) {
      case Datamodel::Accord::None : break ;
      case Datamodel::Accord::Flat : oss << "$\\flat$" ; break ;
      case Datamodel::Accord::Sharp : oss << "$\\sharp$" ; break ;
      default : assert(false) ;
      }
      std::string subscript ;
      if (a.t_.chord_.minor_) { subscript="m" ; }
      if (a.t_.chord_.minor7_) { subscript += "7" ; }
      if (a.t_.chord_.sus4_) { subscript += "4" ; } 
      if (a.t_.chord_.major7_) { subscript += "7M" ; }
      if (subscript != "" ) { oss << "\\textsubscript{\\small{" << subscript << "}}" ; }
      oss << "}}" ;
      chord = oss.str() ;
    }

    std::cout << "word '" << word << "'" << std::endl; 
    std::cout << "after '" << after << "'" << std::endl; 
    std::string line2(before + chord + "\\underline{" + word + "}" + after) ;
    std::cout << "line2 '" << line2 << "'" << std::endl; 
    line=line2 ;
    // std::string line2(after) ;
    // assert(line.size() > line2.size()+2) ;
    r(line) ;
  } ;
  std::string ret(line) ;
  std::cout << "BBBBBBBBBBefore : " << line << std::endl ;
  r(ret) ;
  std::cout << "AAAAAAAAAAAAAafter : " << ret << std::endl ;
  return ret ;
}
  


std::string mp_grille_filename (const Datamodel::Conf& conf,const Song& song,int count) {
  std::string ret(replace_extension(song.filename_,"")) ;
  ret = replace_path(ret,conf.srcdir_.c_str(),conf.builddir_.c_str()) ;
  replace_extension(ret,"") ;
  ret += "-grille-" + std::to_string(count) + ".mp" ;
  return ret ;
}

std::string mp_tab_filename (const Datamodel::Conf& conf,const Song& song,int count) {
  std::string ret(replace_extension(song.filename_,"")) ;
  ret = replace_path(ret,conf.srcdir_.c_str(),conf.builddir_.c_str()) ;
  replace_extension(ret,"") ;
  ret += "-tab-" + std::to_string(count) + ".mp" ;
  return ret ;
}

extern void grille_write_mp(const std::string&filename,const Grille& grille) ;

extern void tab_write_mp(const Song& song,const std::string&filename,const Tablature& tab) ;


void write_preamble(std::ofstream& fout) {

  fout << "\
\\documentclass[a4paper,portrait]{article}\n\
\\usepackage[utf8]{inputenc}\n\
\\usepackage[T1]{fontenc}\n\
%\\usepackage{wasysym}\n\
\\usepackage{soulutf8}\n\
\\usepackage{fixltx2e}\n\
%\\usepackage[pdftex,colorlinks=true,urlcolor=blue,linkcolor=blue]{hyperref}\n\
\\usepackage{hyperref}\n\
\\usepackage{diagbox}\n\
%\\usepackage{caption}\n\
%\\usepackage[scale=0.8]{geometry}\n\
\\usepackage[a4paper,inner=1cm,outer=1cm,top=2cm,bottom=2cm]{geometry} \n\
\\usepackage{color,soul}\n\
\\definecolor{grey}{rgb}{0.7,0.7,0.7}\n\
\\definecolor{grey8}{rgb}{0.8,0.8,0.8}\n\
\\definecolor{colora}{rgb}{0.9,0.7,0.9}\n\
\\sethlcolor{grey8}\n\
\\usepackage[pdftex]{graphicx}\n\
%\\usepackage{lmodern}\n\
%\\usepackage{textcomp}\n\
%\\usepackage{kpfonts}\n\
\\usepackage{array,multirow}\n\
\\usepackage{fancyhdr} \n\
\\pagestyle{fancy} \n\
\\usepackage{makeidx}\n\
\\newcolumntype{M}[1]{>{\\centering\\arraybackslash}m{#1}}\n\
%\\newcolumntype{M}[1]{>{\\centering}m{#1}}\n\
\\newcolumntype{N}{@{}m{0pt}@{}}\n\
\\renewcommand{\\arraystretch}{1.2}\n\
\\usepackage{multicol}\n\
%\\usepackage{french}\n\
\\setlength{\\columnseprule}{1pt}\n\
\\setlength{\\columnsep}{0cm}\n\
\\usepackage{lastpage}\n\
\\usepackage{needspace}\n\
\\usepackage{titletoc}\n\
\\hypersetup{ pdftitle={}, pdfauthor={},bookmarks=true, bookmarksopen=true,pdftoolbar=true, pdffitwindow=false,colorlinks=false,linkcolor=red, citecolor=red,filecolor=magenta,urlcolor=black }\n\
%\\usepackage{bookmark}\n\
\\usepackage[metapost,truebbox,mplabels]{mfpic}\n\
\n\
% de la doc de multicols\n\
%\\setlength{\\textwidth}{39pc}\n\
%\\setlength{\\textheight}{54pc}\n\
%\\setlength{\\parindent}{1em}\n\
%\\setlength{\\parskip}{0pt plus 1pt}\n\
%\\setlength{\\oddsidemargin}{0pc}\n\
%\\setlength{\\marginparwidth}{0pc}\n\
%\\setlength{\\topmargin}{-2.5pc}\n\
%\\setlength{\\headsep}{20pt}\n\
\n\
\n\
  \\newcommand\\invisiblesection[1]{%\n\
  \\refstepcounter{section}%\n\
  \\addcontentsline{toc}{section}{\\protect\\numberline{\\thesection}#1}%\n\
  \\sectionmark{#1}} \n\
\\newcommand*{\\authorfont}{\\fontfamily{ptm}\\fontsize{20}{25}\\fontshape{it}\\selectfont} \n\
\\newcommand*{\\titlefont}{\\fontfamily{ptm}\\fontsize{30}{35}\\fontshape{it}\\selectfont} \n\
\\newcommand*{\\commentfont}{\\fontfamily{ptm}\\fontsize{12}{15}\\fontshape{it}\\selectfont} \n\
\\newcommand*{\\commentafont}{\\fontfamily{ptm}\\fontsize{12}{15}\\fontshape{it}\\selectfont} \n\
\\newcommand*{\\lyricstitlefont}{\\color{black}\\fontfamily{ptm}\\fontsize{15}{15}\\fontshape{it}\\selectfont} \n\
\\newenvironment{lyricsfont}{\\fontfamily{ptm}\\fontsize{12}{12}\\selectfont}{} \n\
\\newenvironment{grillefont}{\\color{blue}\\fontfamily{ptm}\\fontsize{15}{15}\\selectfont}{} \n\
\\newcommand*{\\tabbox}[2][t]{%\n\
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
\\renewcommand\\headrulewidth{0.02in}\\renewcommand\\footrulewidth{0.02in}\
\\Needspace{5\\baselineskip}\n\
\\def\\mystrut(#1,#2){\\vrule height #1 depth #2 width 0pt}\n\
\\newcolumntype{C}[1]{%\n\
   >{\\mystrut(10ex,10ex)\\centering}%\n\
   p{#1}%\n\
   <{}}  \n\
" ;
}

const void Song::write(const Datamodel::Conf& la_conf) {

  std::cout << "Song::write " << filename_ << std::endl ;


  std::accumulate(
		  grilles_.begin(),grilles_.end(),0,
		  [this,&la_conf](int index,const Grille& g) {
		    grille_write_mp(mp_grille_filename(la_conf,*this,index),g) ;
		    return index+1 ;
		  }) ;

  std::accumulate(
		  tablatures_.begin(),tablatures_.end(),0,
		  [this,&la_conf](int index,const Tablature& t) {
		    tab_write_mp(*this,mp_tab_filename(la_conf,*this,index),t) ;
		    return index+1 ;
		  }) ;


  std::function<void(std::ofstream&)> write_1 =
    [this](std::ofstream&fout) {
    fout << "\
\\title{" << this->titre_ << "}\n\
\\author{" << this->auteur_ << "}\n\
\\fancyhead[L]{{\\titlefont " << this->titre_ << "} } \n\
\\fancyhead[R]{{\\authorfont "<<  this->auteur_ << "}} \n\
\\fancyhead[C]{} \n\
\n\
\\begin{document}\n\
\n\
"
    ;
  } ;



  std::function<void(std::ofstream&,const std::string&)>write_grilles =
    [this](std::ofstream&fout,const std::string &file_basename) {
    int count=0 ;
    fout << "\
\\begin{multicols}{2}\n\
\n\
" ;
    for (auto g:this->grilles_) {
      fout << "\
\\begin{tabular}{c}\n\
\\\\\n\
{\\commentfont \\hl{" << tex_of_string(g.t_.titre_) << "}}\n\
\\\\\n\
\n\
\\includegraphics{" << basename(file_basename) << "-grille-" << count << ".mps}\n\
\\end{tabular}\n\
" ;
      count++ ;
    }
    fout << "\
\\end{multicols}\n\
" ;
  } ;


  std::function<void(std::ofstream&,const std::string&)>write_tablatures =
    [this](std::ofstream&fout,const std::string &file_basename) {
    int count=0 ;
    fout << "\\begin{multicols}{2}\n" ;
    for (auto b:this->tablatures_) {
      fout << "\
\\begin{tabular}{c}\n\
\\\\\n\n\
{\\commentfont \\hl{" << tex_of_string(b.t_.titre_) << "}}\n\
\\\\\n\
\n\
\\includegraphics{" << basename(file_basename) << "-tab-" << count << ".mps}\n \
\\end{tabular}\n\
" ;
      count++ ;
    }
    fout << "\
\\end{multicols}\n\
" ;
  } ;


  std::function<void(std::ofstream&)>write_lyrics =
    [this](std::ofstream&fout) {
    // fout << "LLLLLLLLLLLLLL" << this->lyrics_.size() << std::endl; 
    fout << "\\begin{multicols}{2}\n" ;
    for (auto l:this->lyrics_) {
      fout << "\\begin{lyricsfont}\n" ;
      fout << "\\begin{verse}\n" ;
      fout << "{\\commentfont \\hl{" << tex_of_string(l.title_) << "}} \n" ;
      for (auto line:l.data_) {
	if (line=="\\") {
	  fout << "\n\\end{verse}\n\\begin{verse}\n" ;
	}
	else if (line.size()>2 && line[0]=='{') {
	  fout << "{\\sethlcolor{grey8}\\commentfont \\hl" << line << "}\\\\\n" ;
	}
	else {
	  fout << tex_of_string(replace_chord(line)) << "\\\\" << std::endl; 
	}
      }
      fout << "\\end{verse}\n" ;
      fout << "\\end{lyricsfont}\n\n" ;
    }
    fout << "\\end{multicols}\n\n" ;
  } ;

  std::string path (replace_extension(filename_,".tex")) ;
  path = replace_path(path,la_conf.srcdir_.c_str(),la_conf.builddir_.c_str()) ;
  std::cout << "Write tex file '" << path << "'" << std::endl ;
  std::ofstream fout(path) ;
  write_preamble(fout) ;
  write_1(fout) ;
  write_grilles(fout,replace_extension(path,"")) ;
  write_tablatures(fout,replace_extension(path,"")) ;
  write_lyrics(fout) ;
  fout << "\n\\end{document}\n" ;
}
