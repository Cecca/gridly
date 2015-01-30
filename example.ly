\version "2.18.2"

\include "grid.ily"

\initMusicGrid #22 #'("soprano"
                      "tenore"
                      "alto"
                      "basso")


\gridPutMusic "soprano" #1
\relative c' {
  e2 g |
}

\gridPutMusic "soprano" #2
\relative c' {
  f2 d2 |
}

\gridPutMusic "tenore" #1
\relative c' {
  c1 |
}

\gridPutMusic "tenore" #2
\relative c' {
  e1 |
}

\gridPutMusic "tenore" #3
\with {
  opening = { \time 4/4 }
  
  %% Throws an error
  %% closing = #3
}
\relative c' {
  c1 |
}

\displayMusicGrid

\checkMusicGrid

sections = #'(1 . 2)

\score {
  <<
    \new ChoirStaff <<
      \new Staff \new Voice \gridGetMusic "soprano" \sections
      \new Staff \new Voice \gridGetMusic "alto" \sections
      \new Staff \new Voice \gridGetMusic "tenore" \sections
      \new Staff \new Voice \gridGetMusic "basso" \sections
    >>  
  >>

  \layout{}
}

