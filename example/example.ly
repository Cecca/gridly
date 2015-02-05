\version "2.18.2"

\include "grid.ily"
\include "grid-templates.ily"

\gridVersion "0.1.0"

\initMusicGrid #2 #'("soprano"
                      "tenore"
                      "alto"
                      "basso")

\gridSetStructure #1
\with {
  lyrics = \lyricmode { Fa }
}
\relative c' {
  s1*0 \mark #1
  s1 |
}

\gridSetStructure #2
\with {
  lyrics = \lyricmode { la la! }
}
\relative c' {
  \mark #2
  s1 | s1 \bar "|." |
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
  lyrics = \lyricmode { la la la! }
}
\relative c' {
  f2 d2 | c1 |
}

\gridPutMusic "tenore" #1
\relative c' {
  c1 |
}

\gridPutMusic "tenore" #2
\relative c' {
  e1 | e1 |
}

\gridPutMusic "alto" #1
\relative c' {
  e1 |
}

\gridPutMusic "alto" #2
\relative c' {
  e1 | g1 |
}

\gridPutMusic "basso" #1
\relative c' {
  c1 |
}

\gridPutMusic "basso" #2
\relative c' {
  b1 | c1 |
}

\displayMusicGrid

\checkMusicGrid

sections = #'all

\score {
  
  \SATBChoir \sections

  \layout{}
  \midi{}
}

\rehearsalMidi {\SATBChoir \sections} "soprano"

\rehearsalMidi {\SATBChoir \sections} "alto"

\rehearsalMidi {\SATBChoir \sections} "tenore"

\rehearsalMidi {\SATBChoir \sections} "basso"
