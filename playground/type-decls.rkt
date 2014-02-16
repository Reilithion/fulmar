#lang typed/racket

(struct: Nothing ())
(struct: (a) Just ([v : a]))

(define-type (Maybe a) (U Nothing (Just a)))

(: cons-with-maybe (All (a) ((Maybe a)
                             (Listof a)
                             -> (Listof a))))
(define (cons-with-maybe ar dr)
  (cond
    [(Nothing? ar) dr]
    [(Just? ar) (let ([v (Just-v ar)]) (cons v dr))]))

(: segregate (All (a b) (((U a b) -> (Pairof (Maybe a) (Maybe b)))
                         (Listof (U a b))
                         -> (Pairof (Listof a) (Listof b)))))
(define (segregate sifter lst)
  (foldr
   (λ: ([e : (U a b)] [p : (Pairof (Listof a) (Listof b))])
     (let ([se (sifter e)])
       (let ([l (car se)]
             [r (cdr se)]
             [ls (car p)]
             [rs (cdr p)])
         (ann (cons (cons-with-maybe l ls)
                    (cons-with-maybe r rs)) (Pairof (Listof a) (Listof b))))))
   (ann '(() . ()) (Pairof (Listof a) (Listof b)))
   lst))

(define-type NDBoolean (U Boolean 'unspecified))
(define-type C++-base-type (U C++-pointable-type Symbol))
(define-type C++-type-size (U Null 'long 'short 'longlong))
(define-type C++-type-signedness (U Null 'signed 'unsigned))
(define-type C++-type-qualifier (U 'const 'volatile))
(define-type C++-float-type (U 'float 'double 'longdouble))

(struct: C++-type
  ([base : C++-base-type])
  #:transparent)

(struct: C++-qualified-type C++-type
  ([qualifiers : (Listof C++-type-qualifier)])
  #:transparent)

(struct: C++-pointable-type C++-qualified-type () #:transparent)
(struct: C++-reference-type C++-qualified-type () #:transparent)
(struct: C++-pointer-type C++-pointable-type () #:transparent)

(struct: C++-array-type C++-pointable-type
  ([length : Integer]) ; Should actually be someting like (U Integer Chunk)
  #:transparent)

; Primitive type stuff
(struct: C++-sizable-type C++-pointable-type
  ([size : C++-type-size])
  #:transparent)

(struct: C++-integer-type C++-sizable-type
  ([signedness : C++-type-signedness])
  #:transparent)

; Template type stuff
(struct: C++-templated-type C++-pointable-type
  ([parameters : (Listof C++-type)]) ; Should allow more kinds of parameters
  #:transparent)

;;; PUBLIC CONSTRUCTORS ;;;

(: fmr-float (C++-type-qualifier * -> C++-pointable-type))
(define (fmr-float . qualifiers)
  (C++-pointable-type 'float qualifiers))

(: fmr-double (C++-type-qualifier * -> C++-pointable-type))
(define (fmr-double . qualifiers)
  (C++-pointable-type 'double qualifiers))

(: fmr-long-double (C++-type-qualifier * -> C++-sizable-type))
(define (fmr-long-double . qualifiers)
  (C++-sizable-type 'double qualifiers 'long))

(: fmr-int (C++-type-size
            C++-type-signedness
            C++-type-qualifier * -> C++-integer-type))
(define (fmr-int size signedness . qualifiers)
  (C++-integer-type 'int qualifiers size signedness))

(: fmr-char (C++-type-signedness C++-type-qualifier * -> C++-integer-type))
(define (fmr-char signedness . qualifiers)
  (C++-integer-type 'char qualifiers '() signedness))

(: fmr-pointer (C++-pointable-type C++-type-qualifier * -> C++-pointer-type))
(define (fmr-pointer base . qualifiers)
  (C++-pointer-type base qualifiers))

(: fmr-reference (C++-pointable-type C++-type-qualifier * -> C++-reference-type))
(define (fmr-reference base . qualifiers)
  (C++-reference-type base qualifiers))

(: fmr-array (C++-pointable-type
              Integer
              C++-type-qualifier * -> C++-array-type))
(define (fmr-array base length . qualifiers)
  (C++-array-type base qualifiers length))

(: fmr-template-type (C++-base-type
                      (U C++-type C++-type-qualifier) * -> C++-templated-type))
(define (fmr-template-type base . qualifiers-and-params)
  (let: ([sqa : (Pairof (Listof C++-type) (Listof C++-type-qualifier))
              (segregate (λ: ([q-or-p : (U C++-type C++-type-qualifier)])
                           (if (C++-type? q-or-p)
                               (cons (Just q-or-p) (Nothing))
                               (cons (Nothing) (Just q-or-p))))
                         qualifiers-and-params)])
    
    (let ([params (car sqa)]
          [qualifiers (cdr sqa)])
      (C++-templated-type base qualifiers params))))