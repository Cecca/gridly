\version "2.18.2"

\include "grid.ily"

SATBChoir =
#(define-music-function
   (parser location segments)
   (segment-selector?)
   #{
     <<
       \new ChoirStaff <<
         \new Staff <<
           \new Voice \gridGetStructure $segments
           \new Voice = "soprano" \gridGetMusic "soprano" $segments
           \new Lyrics \lyricsto "soprano" \gridGetLyrics "soprano" $segments
         >>

         \new Staff \new Voice = "alto" \gridGetMusic "alto" $segments
         \new Lyrics \lyricsto "alto" \gridGetLyrics "alto" $segments

         \new Staff \new Voice = "tenore" \gridGetMusic "tenore" $segments
         \new Lyrics \lyricsto "tenore" \gridGetLyrics "tenore" $segments

         \new Staff \new Voice = "basso" \gridGetMusic "basso" $segments
         \new Lyrics \lyricsto "basso" \gridGetLyrics "basso" $segments
       >>
     >>
   #})
