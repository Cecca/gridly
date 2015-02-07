Gridly - Simple segmented grid for LilyPond
===========================================

This package implements the "segmented grid" approach described in
[this blog post](http://lilypondblog.org/2014/10/segment-grid/) by Urs
Liska on lilypondblog.org.

This approach consists in dividing a multi-part score in many segments
that can be edited independently, possibly my many people at the same
time. From the post

> A score is naturally organized in a set of “voices” or “parts” – any
> LilyPond user would enter each part in a separate music variable and
> maintain it in its own file. But a score usually is also organized
> over time, usually with rehearsal marks, which form a natural way to
> define the “columns” of our grid.

Having each segment in its own file has several advantages, as pointed
out in the original blog post:

 - each file is small and manageable
 - single segments can be compiled standalone, thus reducing the time
   spent waiting for the LilyPond compiler while entering music
 - using many small files under version control reduces the risk of
   incurring into merge conflicts

Motivation
----------

I found the "segmented grid" approach extremely useful, even for
working on small scores on my own. Having the score split into small
segments sets some natural breakpoints for the work. Moreover you are
less likely to mess up everything because of misalignment errors,
because the grid, together with bar checks, keeps everything in its
place.

However, this approach has some drawbacks for me:

 - you have to preprocess the input files every time you start a new
   project, using some custom scripts
 - you can't easily change the structure of the grid, i.e. add new
   segments in the middle or changing the split point between
   segments.
 - if you want to select a subset of the segments (for instance you
   want to prepare a score with only segments from D to F) you have to
   create another LiliPond file that assembles only the desired subset
   of variables

In an ideal world, you never make mistakes, so you get the right
number of segments and the right separation points between them from
the beginning. In the real world, once the project is well on its way,
you may discover that a segment should really have started at some
other measure. In such a situation changing the grid structure is
really difficult, because you have to manually change all the bar
checks in all the files while changing the duration of some
segments. And you need to get it right, otherwise there will be errors
due to misalignment with error messages that are not useful at all.

This is due to the fact that all the segments are stored in LilyPond
variables, but LilyPond itself does not have any information about
these segments and their structure. The purpose of `gridly` is just
that: to provide some information about the structure, so that
refactoring the grid, slicing it and manipulate it in general can be
accomplished more easily.
