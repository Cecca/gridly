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
    ((not (integer? segment))
     (ly:error "Segment must be an integer, was " segment))
    ((> 1 segment)
     (ly:error "Segment must be > 1, was" segment))
    ((< (hash-ref music-grid-meta #:segments) segment)
     (ly:error (stringfy "Segment must be less than "
                      (hash-ref music-grid-meta #:segments) ", was" )
            segment))
    ;; Check part
    ((not (string? part))
     (ly:error "Part must be a string"))
    ((not (member part (hash-ref music-grid-meta #:parts)))
     (ly:error "Part must be defined in \\initMusicGrid"))
    (#t #t)))

#(define (check-grid)
   (if music-grid-meta
       #t
       (ly:error "You must first call \\initMusicGrid")))

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

#(define (check-durations segment strict)
   (let* ((durations (map
                      (lambda (part)
                        (let ((cell (get-music-cell part segment #:music)))
                          (cons part
                                (if cell
                                    (ly:moment-main (ly:music-length cell))
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
                       (list "Expected length of ~a for part ~a segment ~a, got ~a"
                             reference-duration (car d-pair) segment (cdr d-pair))))
                  (if strict
                      (apply ly:error msg-args)
                      (apply ly:warning msg-args)))))
          defined-durations))))

displayMusicGrid =
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

checkMusicGrid =
#(define-void-function
   (parser location) ()
   (for-each
    (lambda (segment)
      (check-durations segment #f))
    (map (lambda (x) (+ 1 x))
         (iota (hash-ref music-grid-meta #:segments)))))

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
             (ly:error (stringfy "Expected music for key '" key "'! Got") res))
         #{ #})))

gridPutMusic =
#(define-void-function
   (parser location part segment ctx-mod music)
   (string? number? (ly:context-mod?) ly:music?)
   (check-grid)
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

#(define (segment-selector? x)
   (or (pair? x)
       (equal? 'all x)))

gridGetMusic =
#(define-music-function
   (parser location part start-end) (string? segment-selector?)
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
            (elems (map (lambda (i) (get-music-cell part i #:music)) segments)))
       (if (member #f elems)
           (ly:error "There is a segment still to be created!")
           (make-music
            'SequentialMusic
            'elements elems)))))
