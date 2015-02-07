\version "2.18.2"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%                                Gridly
%%%                                ======
%%%
%%% A simple "segmented grid" framework.
%%%
%%% Changelog
%%% ---------
%%%
%%% * 0.2.0 - development version
%%%   This is a **breaking** release. The public interface changed.
%%%
%%%   - Some refactoring of public function names, to make the interface more
%%%     consistent. Now all the public music functions start with `grid`.
%%%      - \displayMusicGrid -> \gridDisplay
%%%      - \checkMusicGrid   -> \gridCheck
%%%      - \initMusicGrid    -> \gridInit
%%%
%%% * 0.1.0
%%%   Initial relase, featuring the following public functions:
%%%    - \gridVersion
%%%    - \displayMusicGrid
%%%    - \checkMusicGrid
%%%    - \initMusicGrid
%%%    - \gridSetStructure
%%%    - \gridPutMusic
%%%    - \gridGetMusic
%%%    - \gridGetOpening
%%%    - \gridGetLyrics
%%%    - \gridGetStructure
%%%    - \gridTest
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define grid-current-version "0.2.0")

gridVersion =
#(define-void-function
   (parser location expected-version) (string?)
   (if (not (equal? grid-current-version expected-version))
       (ly:error
        "You are running gridly version ~a, but the expected version is ~a"
        grid-current-version expected-version)))

#(use-modules (oop goops))
#(use-modules (ice-9 regex))

#(define-class <cell> ()
   (barcheck #:init-keyword #:barckeck
             #:getter cell:barcheck)
   (music #:init-keyword #:music
          #:getter cell:music)
   (lyrics #:init-keyword #:lyrics
           #:getter cell:lyrics)
   (opening #:init-keyword #:opening
            #:getter cell:opening)
   (closing #:init-keyword #:closing
            #:getter cell:closing))

%%% The association list holding all the music.
#(if (not (defined? 'music-grid))
     (define music-grid #f))

%%% Information that needs to be set up using \initMusicGrid
#(if (not (defined? 'music-grid-meta))
     (define music-grid-meta #f))

%%% Some utility functions

#(define (check-coords part segment)
   (cond
    ;; Check segment
    ((not (integer? segment))
     (ly:error "Segment must be an integer, was ~a" segment))
    ((> 1 segment)
     (ly:error "Segment must be > 1, was" segment))
    ((< (hash-ref music-grid-meta #:segments) segment)
     (ly:error "Segment must be less than ~a, was ~a"
               (hash-ref music-grid-meta #:segments) segment))
    ;; Check part
    ((not (string? part))
     (ly:error "Part must be a string"))
    ((not (member part (hash-ref music-grid-meta #:parts)))
     (ly:error "Part must be defined in \\gridInit: ~a" part))
    (#t #t)))

#(define (check-grid)
   (if (and music-grid music-grid-meta)
       #t
       (ly:error "You must first call \\initMusicGrid")))

#(define (display-spaces num-spaces)
   (for-each (lambda (x) (display " ")) (iota num-spaces)))

#(define (get-music-cell part segment)
   (check-coords part segment)
   (hash-ref music-grid (cons part segment)))

#(define (check-durations segment strict)
   (let* ((durations (map
                      (lambda (part)
                        (let ((cell (get-music-cell part segment)))
                          (cons part
                                (if cell
                                    (ly:moment-main (ly:music-length
                                                     (cell:music cell)))
                                    #f))))
                      (hash-ref music-grid-meta #:parts)))
          (defined-durations (filter cdr durations))
          (reference-duration (if (null? defined-durations)
                                  #f
                                  (cdar defined-durations))))
     (if reference-duration
         (for-each
          (lambda (d-pair)
            (if (not (equal? reference-duration (cdr d-pair)))
                (let ((msg-args
                       (list "Expected length of ~a for ~a:~a, got ~a"
                             reference-duration (car d-pair) segment (cdr d-pair))))
                  (if strict
                      (apply ly:error msg-args)
                      (apply ly:warning msg-args)))))
          defined-durations))))

gridDisplay =
#(define-void-function
   (parser location) ()
   (let* ((num-segments (hash-ref music-grid-meta #:segments))
          (segments (map (lambda (x) (+ 1 x)) (iota num-segments)))
          (parts (hash-ref music-grid-meta #:parts)))
     (newline)
     (display "=== Music grid ===")
     (newline)
     (let ((longest-name (reduce max 0
                                 (map string-length parts))))
       (display-spaces longest-name)
       (for-each (lambda (x) (display (ly:format " ~a" x))) segments)
       (for-each
        (lambda (part)
          (newline)
          (display part)
          (display-spaces (- longest-name (string-length part)))
          (for-each
           (lambda (seg)
             (display-spaces (string-length (number->string seg)))
             (if (hash-ref music-grid (cons part seg))
                 (display "o")
                 (display "-")))
           segments))
        parts))
     (newline)))

gridCheck =
#(define-void-function
   (parser location) ()
   (for-each
    (lambda (segment)
      (check-durations segment #f))
    (map (lambda (x) (+ 1 x))
         (iota (hash-ref music-grid-meta #:segments)))))

%%% This is taken from Lalily
#(define (test-location? parser location)
   (let ((outname (ly:parser-output-name parser))
         (locname (car (ly:input-file-line-char-column location))))
     (regexp-match? (string-match (format "^(.*/)?~A\\.i?ly$" outname) locname))))

%%% Grid initialization
gridInit =
#(define-void-function
   (parser location segments parts) (number? list?)
   (if music-grid
       (ly:debug "Music grid already initialized, skipping initialization")
       (set! music-grid (make-hash-table)))
   (if music-grid-meta
       (ly:debug
        "Music grid metadata already initialized, skipping initialization")
       (begin
         (set! music-grid-meta (make-hash-table))
         (hash-set! music-grid-meta #:segments segments)
         (hash-set! music-grid-meta #:parts (cons "<structure>" parts)))))


%%% Grid manipulation

#(define (context-mod->alist ctx-mod)
   (let ((props '()))
     (if ctx-mod
         (for-each
          (lambda (mod)
            (set! props
                  (assoc-set! props
                              (cadr mod) (caddr mod))))
          (ly:get-context-mods ctx-mod)))
     props))

gridPutMusic =
#(define-void-function
   (parser location part segment ctx-mod music)
   (string? number? (ly:context-mod?) ly:music?)
   (check-grid)
   (check-coords part segment)
   (let* ((props (context-mod->alist ctx-mod))
          (key (cons part segment))
          ;; This closure will look in the `props' alist for the given
          ;; symbol, returning the associated value. If the symbol is
          ;; not in the alist, then a default value is looked up in
          ;; the corresponding `<structure>' segment. If even there a
          ;; default value is not found, `default'
          (props-get (lambda (sym last-default)
                       (let ((res (assoc-ref props sym)))
                         (if res
                             res
                             (let ((cell-structure
                                    (get-music-cell "<structure>" segment)))
                               (if cell-structure
                                   (slot-ref cell-structure sym)
                                   last-default))))))
          (value (make <cell>
                   #:music music
                   #:lyrics (props-get 'lyrics #f)
                   #:opening (props-get 'opening #{ #})
                   #:closing (props-get 'closing #{ #}))))
     (hash-set! music-grid key value)))

gridSetStructure =
#(define-void-function
   (parser location segment ctx-mod music)
   (number? (ly:context-mod? #{ \with{} #}) ly:music?)
   (if (get-music-cell "<structure>" segment)
       (ly:debug "Skipping setting of <structure>:~a, already set" segment)
       #{
         \gridPutMusic "<structure>" $segment $ctx-mod $music
       #}))

#(define (segment-selector? x)
   (or (pair? x)
       (equal? 'all x)))

#(define (get-cell-range part start-end)
   (check-grid)
   (let ((start (if (equal? 'all start-end)
                    1
                    (car start-end)))
         (end (if (equal? 'all start-end)
                  (hash-ref music-grid-meta #:segments)
                  (cdr start-end))))
     (check-coords part start)
     (check-coords part end)
     (let* ((segments (map (lambda (x) (+ x start)) (iota (+ 1 (- end start)))))
            (elems (map (lambda (i)
                          (let ((cell (get-music-cell part i)))
                            (if cell
                                cell
                                (ly:error
                                 "Segment '~a' of part '~a' is still empty"
                                 i part))))
                        segments)))
       elems)))

gridGetMusic =
#(define-music-function
   (parser location part start-end) (string? segment-selector?)
   (let* ((cells (get-cell-range part start-end))
          (music (map cell:music cells)))
     (make-music
      'SequentialMusic
      'elements music)))

gridGetLyrics =
#(define-music-function
   (parser location part start-end) (string? segment-selector?)
   (let* ((cells (get-cell-range part start-end))
          (lyrics (map cell:lyrics cells)))
     (if (member #f lyrics)
         (ly:error "A segment is missing lyrics!")
         (make-music
          'SequentialMusic
          'elements lyrics))))

gridGetStructure =
#(define-music-function
   (parser location start-end) (segment-selector?)
   #{
     \gridGetMusic "<structure>" $start-end
   #})

gridTest =
#(define-void-function
   (parser location part segment)
   (string? number?)
   (check-grid)
   (check-coords part segment)
   (if (test-location? parser location)
       (begin
         (display "Compiling test file\n")
         (if (not (get-music-cell part segment))
             (ly:error "There is no music cell for ~a:~a"
                       part segment))
         (check-durations segment #f)
         (let* ((opening (cell:opening (get-music-cell part segment)))
                (closing (cell:closing (get-music-cell part segment)))
                (selector (cons segment segment))
                (book
                 #{
                    \book {
                      \score {
                              \new Staff \new Voice {
                                $opening
                                \gridGetMusic $part $selector
                                $closing
                              }
                         \midi{}
                         \layout{}
                      }
                    }
                  #}))
           (ly:book-process book
                            #{ \paper {} #}
                            #{ \layout {} #}
                            (ly:parser-output-name parser))))))
