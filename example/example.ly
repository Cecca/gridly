\version "2.18.2"

\include "grid.ily"

\initMusicGrid #22 #'("soprano"
                      "tenore"
                      "alto"
                      "basso")

\gridSetStructure #1
\relative c' {
  s1 |
}

\gridPutMusic "soprano" #1
\with {
  lyrics = \lyricmode { Fa la }
}
\relative c' {
  e2 g |
}

\gridPutMusic "soprano" #2
\with {
  lyrics = \lyricmode { la la! }
}
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

\gridPutMusic "alto" #1
\relative c' {
  e1 |
}

\gridPutMusic "alto" #2
\relative c' {
  e1 |
}

\gridPutMusic "basso" #1
\relative c' {
  c1 |
}

\gridPutMusic "basso" #2
\relative c' {
  b1 |
}


\displayMusicGrid

\checkMusicGrid

sections = #'(1 . 2)

\score {
  <<
    \new ChoirStaff <<
      \new Staff \new Voice = "soprano" \gridGetMusic "soprano" \sections
      \new Lyrics \lyricsto "soprano" \gridGetLyrics "soprano" \sections

      \new Staff \new Voice \gridGetMusic "alto" \sections
      \new Staff \new Voice \gridGetMusic "tenore" \sections
      \new Staff \new Voice \gridGetMusic "basso" \sections
    >>  
  >>

  \layout{}
  \midi{}
}

