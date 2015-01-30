\version "2.18.2"

#(use-modules (ice-9 format))

%%% The association list holding all the music.
#(define music-grid (make-hash-table))

%%% Information that needs to be set up using \initMusicGrid
#(define music-grid-meta #f)

%%% Some utility functions

#(define (stringfy . items)
   "Concatenates all the given items in a single string"
   (apply string-append
          (map (lambda (it)
                 (if (string? it)
                     it
                     (object->string it)))
               items)))

#(define (check-coords part segment)
   (cond
    ;; Check segment
    ((not (integer? segment)) (error "Segment must be an integer, was " segment))
    ((> 1 segment) (error "Segment must be > 1, was" segment))
    ((< (hash-ref music-grid-meta #:segments) segment)
     (error (stringfy "Segment must be less than "
                      (hash-ref music-grid-meta #:segments) ", was" )
            segment))
    ;; Check part
    ((not (string? part)) (error "Part must be a string"))
    ((not (member part (hash-ref music-grid-meta #:parts)))
     (error "Part must be defined in \\initMusicGrid"))
    (#t #t)))

#(define (check-grid)
   (if music-grid-meta
       #t
       (error "You must fist call \\initMusicGrid")))

#(define (display-cell cell)
   "Display the given cell: '((part . segment) . music)"
   (display (stringfy
             "==== Part " (caar cell)
             " segment " (cdar cell)
             " duration " (ly:moment-main (ly:music-length (cdr cell)))
             " ===="))
   (newline))

#(define (display-spaces num-spaces)
   (for-each (lambda (x) (display " ")) (iota num-spaces)))

#(define (get-music-cell part segment key)
   "Retrieves the music associated to `key` from the cell identified
    by `part` and `segment`"
   (check-coords part segment)
   (let ((cell (hash-ref music-grid (cons part segment))))
     (if cell
         (assoc-ref cell key)
         #f)))

displayMusicGrid =
#(define-scheme-function
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
       (for-each (lambda (x) (display (stringfy " " x))) segments)
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

%%% Grid initialization
initMusicGrid =
#(define-void-function
   (parser location segments parts) (number? list?)
   (set! music-grid-meta (make-hash-table))
   (hash-set! music-grid-meta #:segments segments)
   (hash-set! music-grid-meta #:parts parts))


%%% Grid manipulation

#(define (get-music alist key)
   (let ((res (assoc-ref alist key)))
     (if res
         (if (ly:music? res)
             res
             (error (stringfy "Expected music for key '" key "'! Got") res))
         #{ #})))

gridPutMusic =
#(define-void-function
   (parser location part segment ctx-mod music)
   (string? number? (ly:context-mod?) ly:music?)
   (check-coords part segment)
   (let ((props '()))
     (if ctx-mod
         (for-each
          (lambda (mod)
            (set! props
                  (assoc-set! props
                              (symbol->string (cadr mod)) (caddr mod))))
          (ly:get-context-mods ctx-mod)))
     (let ((key (cons part segment))
           (value (list
                   (cons #:music music)
                   (cons #:opening (get-music props "opening"))
                   (cons #:closing (get-music props "closing")))))
       (hash-set! music-grid key value))))

gridGetMusic =
#(define-music-function
   (parser location part start-end) (string? pair?)
   (check-coords part segment)
   (let* ((start (car start-end))
          (end (cdr start-end))
          (segments (map (lambda (x) (+ x start)) (iota (+ 1 (- end start)))))
          (elems (map (lambda (i) (get-music-cell part i #:music)) segments)))
     (make-music
      'SequentialMusic
      'elements elems)))
