#lang racket

(provide default-source-mode
         normalize-source-mode
         (struct-out compile-profile)
         canonical-compile-profile
         canonical-compile-profile-jsexpr
         normalize-compile-profile
         compile-profile->jsexpr
         canonical-parser-profile
         canonical-parser-target-id)

(struct compile-profile (conj-assoc disj-assoc delay-placement) #:transparent)

(define canonical-parser-profile "surface->canonical")
(define canonical-parser-target-id "canonical/config")
(define default-source-mode "mini")

(define/match (compile-profile->jsexpr profile)
  [((compile-profile conj-assoc disj-assoc delay-placement))
   (hasheq 'conjAssoc conj-assoc
           'disjAssoc disj-assoc
           'delayPlacement delay-placement)])

(define canonical-compile-profile
  (compile-profile "left" "right" "relbody"))

(define canonical-compile-profile-jsexpr
  (compile-profile->jsexpr canonical-compile-profile))

(define (normalize-source-mode maybe-mode)
  (match maybe-mode
    [(or #f "") default-source-mode]
    [(or "mini" "micro") maybe-mode]
    [_ (error 'normalize-source-mode
              "unsupported sourceMode ~e; expected \"mini\" or \"micro\""
              maybe-mode)]))

(define (normalize-axis maybe-value valid-values key)
  (match maybe-value
    [#f #f]
    [`,v #:when (member v valid-values) maybe-value]
    [_ (error 'normalize-compile-profile
              "invalid compileProfile.~a ~e; expected one of ~e"
              key
              maybe-value
              valid-values)]))

(define (normalize-mini-compile-profile maybe-profile)
  (match maybe-profile
    [#f canonical-compile-profile]
    [(? compile-profile?) maybe-profile]
    [(? hash? profile)
     (match (list (normalize-axis (hash-ref profile 'conjAssoc #f)
                                  '("left" "right")
                                  'conjAssoc)
                  (normalize-axis (hash-ref profile 'disjAssoc #f)
                                  '("left" "right")
                                  'disjAssoc)
                  (normalize-axis (hash-ref profile 'delayPlacement #f)
                                  '("relbody" "relcall" "disj")
                                  'delayPlacement))
       [(list (? string? conj-assoc)
              (? string? disj-assoc)
              (? string? delay-placement))
        (compile-profile conj-assoc disj-assoc delay-placement)]
       [_ (error 'normalize-compile-profile
                 "compileProfile must contain conjAssoc, disjAssoc, and delayPlacement")])]
    [_ (error 'normalize-compile-profile
              "compileProfile must be a hash or compile-profile, got ~e"
              maybe-profile)]))

(define (normalize-compile-profile maybe-profile [source-mode default-source-mode])
  (match source-mode
    ["micro"
     (when maybe-profile
       (error 'normalize-compile-profile
              "compileProfile is only valid when sourceMode is \"mini\""))
     #f]
    ["mini"
     (normalize-mini-compile-profile maybe-profile)]
    [_ (error 'normalize-compile-profile
              "unsupported sourceMode ~e; expected \"mini\" or \"micro\""
              source-mode)]))
