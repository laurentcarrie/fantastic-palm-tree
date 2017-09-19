#include <iostream>
#include <vector>
#include <assert.h>
#include <sstream>
#include <fstream>
#include <functional>
#include <ctime>
#include <iomanip>
#include <numeric>
#include <string>

#include "song.h"
#include "datamodel.h"
#include "read_util.h"


std::tuple<std::string,std::string,std::string>
mp_of_chord(const Datamodel::Accord::c& c) {
  std::string s(&c.note_,1) ;
  if (c.alteration_ == Datamodel::Accord::Flat) {
    s += "\\flatsharpfont{$\\flat$}" ;
  } else if (c.alteration_ == Datamodel::Accord::Sharp) {
    s += "\\flatsharpfont{$\\sharp$}" ;
  }

  if ( c.diminue_) {  s += "\\o" ; }

  std::string upperscript ; 
  std::string subscript ;

  if (c.minor_)  { subscript += "m" ; }
  if (c.minor7_) { upperscript += "7" ; }
  if (c.major7_) { subscript += "$\\bigtriangleup$" ; } 
  if (c.sus4_)   { upperscript += "4" ; } 

  return std::make_tuple(s,subscript,upperscript) ;
}


void tab_write_bar(std::ofstream& fout,const Tablature::bar& b) {
  fout << "\n\
%% write bar\n\
x1 := x0 + width ;\n\
draw(x0,base_line) -- (x0,base_line+height) ;\n\
" ;
   
  if (true) {
    std::vector<int> v({0,1,2,3}) ;
    for (int i : v) {
      fout << "\n\
xii := x0 + " << (2*i+1) << "*ux ;\n\
pickup pencircle scaled 0.02bp ;\n\
%draw(xii,base_line) -- (xii,base_line+5*1uy) ;\n\
pickup pencircle scaled 0.15bp ;\n\
" ;
    }
  }


  //  let _ = List.fold_left( fun pos p ->
  std::accumulate (
		   b.paquets_.begin(),
		   b.paquets_.end(),
		   1,
		   [&fout](int pos,const Tablature::paquet& p) {
		     std::string chord ;
		     {
		       std::ostringstream oss ;
		       if (p.chord_.has_chord_) {
			 auto infos = mp_of_chord(p.chord_.chord_) ;
			 oss << "label.top(btex " << std::get<0>(infos)<<"\\rlap{\\textsuperscript{" 
			     << std::get<1>(infos) << "}}{\\textsubscript{"
			     << std::get<2>(infos) << "}} etex scaled 0.5,(xi,base_line+height)) ;\n" ;
		       }
		       chord = oss.str() ;
		     }
		     fout <<  "\n\
% position " << pos << ", " << p.notes_.size() << " notes\n\
xi := x0+" << pos << "*ux ;\n\
pickup pencircle scaled 0.02bp ;\n\
%draw(xi,base_line) -- (xi,base_line+5*1uy) ;\n\
" << chord << "\n\
pickup pencircle scaled 0.15bp ;\n\
" ; 
		     for ( auto n : p.notes_) {
		       fout << "\n\
  ynote := base_line+(6-" << n.corde_ << ")*uy ;\n\
  label(\"" << n.frette_ << "\" infont defaultfont scaled 0.6,(xi,ynote)) ;\n\
%  draw(xi,ynote-uy) -- (xi,base_line-uy) ;\n\
" ;
		     }
		     int lowest_note ;
		     if (p.notes_.empty()) {
		       lowest_note = 6 ;
		     } else {
		       lowest_note = std::accumulate
		       (p.notes_.begin(),p.notes_.end(),1,
			[](int acc,const Tablature::note& n) {
			 std::cout << "acc = " << acc << " corde = " << n.corde_ << std::endl ;
			 if (acc>n.corde_) { return acc ; }
			 else { return n.corde_ ; }
		       }) ;
		     }
		     std::function<void(int c)> print_tail = 
		       [&p,&fout](int c) {
		       if (p.chord_.position_ == 1) {
			 //  double croche 
			 fout << "\n\
  ynote := base_line+(6-" << c << ")*uy ;\n\
  draw(xi,ynote-0.5*uy) -- (xi,base_line-1*uy) ;\n\
  draw(xi,base_line-1*uy) -- (xi+0.5*ux,base_line-0.8*uy) ;\n\
  draw(xi,base_line-0.6*uy) -- (xi+0.5*ux,base_line-0.4*uy) ;\n\
" ;
		       }
		       else if (p.chord_.position_ == 2) {
			   //  croche 
			   fout << "\n\
  ynote := base_line+(6-" << c << ")*uy ;\n\
  draw(xi,ynote-0.5*uy) -- (xi,base_line-1*uy) ;\n\
  draw(xi,base_line-1*uy) -- (xi+0.5*ux,base_line-0.8*uy) ;\n\
" ;
		       }
		       else if (p.chord_.position_ == 4) {
			   // noire 
			   fout << "\n\
  ynote := base_line+(6-" << c << ")*uy ;\n\
  draw(xi,ynote-0.5*uy) -- (xi,base_line-1*uy) ;\n\
  fill fullcircle scaled unote shifted (xi,base_line-1*uy) withcolor black ;\n\
" ;
		       }

		       else if (p.chord_.position_ == 8 ) {
			     // blanche 
			 fout << "\n\
  ynote := base_line+(6-" << c << ")*uy ;\n\
  draw(xi,ynote-0.5*uy) -- (xi,base_line-1*uy) ;\n\
  fill fullcircle scaled unote shifted (xi,base_line-1*uy) withcolor white ;\n\
  draw fullcircle scaled unote shifted (xi,base_line-1*uy) withcolor black ;\n\
" ;
		       }
		       else {
			 fout << "" ;
		       }
		     } ; // lambda print_tail
		       
		     print_tail(lowest_note) ;
		     return (pos + 1) ;
		   } 
		   ) ; // std::accumulate

  fout << "\n\
for i=0 upto 5 :\n\
   draw (x0,base_line+i*1uy) -- (x1,base_line+i*1uy) ;\n\
  endfor\n\
x0:=x1 ;\n\
" ;
} // write_bar


void tab_write_bar_line(std::ofstream& fout,const Tablature::line& line) {
  fout << "\n\
  x0:=0 ;\n\
\n\
"  ;

  std::accumulate(line.bars_.begin(),line.bars_.end(),0,
		  [&fout](int acc,const Tablature::bar& bar) {
		    tab_write_bar(fout,bar) ; return 0 ; }
		  ) ;

  fout << "\n\
draw(x1,base_line) -- (x1,base_line+5*1uy) ;\n\
base_line := base_line + 6*ux ;\n\
" ;
}

void tab_write_mp(const Song& song,const std::string& filename, const Tablature& tab) {

  std::cout << "tab_write_mp '" << filename << "'" << std::endl ;

  std::ofstream fout(filename) ;
  if (!fout.good()) {
    std::cout << "could not open for writing : '" << filename << "'" << std::endl ;
    assert(false) ;
  }
  fout << "\n\
verbatimtex \n\
%&latex\n\
\\documentclass[12pt]{article}\n\
\\usepackage{fixltx2e}\n\
\\begin{document}\n\
etex\n\
\n\
beginfig(1) ;\n\
  uy=0.2cm ;\n\
  unote=0.1cm ;\n\
  b=10*u ;\n\
  base_line=0*u ;\n\
  gap_base_line = 0*uy ;\n\
  height        = 5*uy ;\n\
  width         = 3cm ;\n\
  nb_croches    = " << song.nb_croches_ << " ;\n\
  ux            = width/(nb_croches+1) ;\n\
  pickup pencircle scaled 0.15bp ;\n\
  "  ;

  std::accumulate(tab.t_.lines_.begin(),tab.t_.lines_.end(),0,[&fout](int acc,const Tablature::line& l) 
		  { 
		    tab_write_bar_line(fout,l) ; 
		    return 0 ; }) ;
  fout << "\n\
endfig ;\n\
\\end{document}\n\
bye\n\
" ;


  /*

  {
    std::ostringstream oss ;
    oss << "mpost " << filename ; 
    std::string command(oss.str()) ;
    int ret = system(command) ;

    let ret = Unix.system command in
  let () = match ret with
    | Unix.WEXITED 0 -> ()
    | _ -> failwith "mpost failed"
  in
  let target = sprintf "%s-tab-%d.1" (Filename.basename name) count in
  let () = if Sys.file_exists target then (
    let target2 =  sprintf "%s-tab-%d.mps" (Filename.basename name) count  in
      Sys.rename target target2
  )
    else
      failwith "mpost failed, cannot find target file"
  in
    ()
  */
}

