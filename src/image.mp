%outputtemplate:="%j%c.mps";
beginfig(1) ;
u=0.2cm ;
  b=10*u ;
  nb=6 ;
%draw (2u,2u) -- (0,0) -- (0,3u) -- (3u,0) -- (0,0); 
%pickup pencircle scaled 12pt;
%for i=0 upto 2 :
%for j=0 upto 2 : 
%draw (i*u,j*u); 
%endfor
%endfor
  pickup pencircle scaled 0.2pt ;
  for i=0 upto 5 :
    draw (0,i*1u) -- (nb*b,i*1u) ;
  endfor
  for i=0 upto nb :
    draw (i*b,0*1u) -- (i*b,5*1u) ;
  endfor
endfig ;
bye

