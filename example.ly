\version "2.18.2"

\include "grid.ily"

\initMusicGrid #22 #'("soprano"
                      "tenore"
                      "alto"
                      "basso")

\gridPutMusic "soprano" #2
\relative c' {
  c2 d2 |
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

\score {
  <<
    \gridGetMusic "tenore" #'(2 . 3)
    
  >>

  \layout{}
}

