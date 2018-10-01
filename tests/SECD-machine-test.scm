

(import chicken scheme)

    (use numbers test)

    (use commons SECD-machine)

    (let* ((v 'x)
           (e (Comb
               (Lambda v
                (Comb
                 (Comb
                  (Comb
                   (Id 'p)
                   (Comb
                    (Comb
                     (Id '*)
                     (Comb
                      (Id '²)
                      (Id v)))
                    (Id 'a)))
                  (Comb
                   (Comb
                    (Id '*)
                    (Id v))
                   (Id 'b)))
                 (Id 'c)))
               (Id 'd))))
    (test "((λ (x) (((p ((* (² x)) a)) ((* x) b)) c)) d)" (to-string e))
    (test (Lambda 'x (Id 'y)) (curryfy '(λ (x) y)))
    (test (Lambda 'x (Comb (Id 'y) (Id 'z))) (curryfy '(λ (x) (y z))))
    (test e (curryfy `((λ (,v) (p (* (² ,v) a) (* ,v b) c)) d)))
    (test 27 ((λ (x a b c) (+ (* (² x) a) (* x b) c)) 4 1 2 3))
    (let₁ (p (λ (x) (λ (y) (λ (z) (+ x y z)))))
     (test 27 ((λ (x a b c) (((p (((curry₁ *) (² x)) a)) (((curry₁ *) x) b)) c)) 4 1 2 3)))
    (let₁ (E ((extend E₀)
              `(a . ,1) `(b . ,2) `(c . ,3) `(d . ,4) `(o . ,-24)
              `(zero . 0) `(one . 1) `(two . 2)
              `(p . ,(lambda (x) (lambda (y) (lambda (z) (+ x y z)))))
              `(* . ,(lambda (x) (lambda (y) (* x y))))
              `(² . ,²) `(null? . ,null?) `(pair? . ,pair?)))
     (test 27 ((value E) e))

     (test "((λ (x) (λ (y) (((p ((* (² x)) a)) ((* x) b)) y))) d)"
      ((○ to-string curryfy) `((λ (,v y) (p (* (² ,v) a) (* ,v b) y)) d)))

     (test-assert ((○ procedure? (value E) curryfy)
                   `((λ (,v y) (p (* (² ,v) a) (* ,v b) y)) d)))

     (let₁ (e₁ `((λ (,v y) (p (* (² ,v) a) (* ,v b) y)) d o))
      (test "(((λ (x) (λ (y) (((p ((* (² x)) a)) ((* x) b)) y))) d) o)"
       ((○ to-string curryfy) e₁))
      (test 0 ((○ (value E) curryfy) e₁)))

     (let₁ (e₂ '(cond
                 ((null? l) zero)
                 ((pair? l) one)
                 (else two)))
      (test "(if (null? l) zero (if (pair? l) one two))"
       ((○ to-string curryfy) e₂))
      (test 0 ((○ (value ((extend E) `(l . ,(list)))) curryfy) e₂))
      (test 1 ((○ (value ((extend E) `(l . ,(list 1)))) curryfy) e₂))
      (test 2 ((○ (value ((extend E) `(l . ,3))) curryfy) e₂)))
     ))

    (define Y
     (lambda (f)
      (Φ (lambda (g) (f (lambda (x) ((g g) x)))))))

    (test 3 ((Y (lambda (L)
                 (lambda (l)
                  (cond/λ l
                   (null? (K 0))
                   (else (○ add1 L cdr)))))) 
             '(1 1 1)))

    (let* ((control (curryfy '((λ (f x) (f (f x))) ² three)))
           (control₁ (curryfy '((λ (f) (λ (x) (f (f x)))) ² three)))
           (control₂ (curryfy '((λ (f) (λ (x) (f (f x)))) (λ (x) (* x x)) three)))
           (length₀ '(λ (L)
                      (λ (l)
                       (cond
                        ((null? l) zero)
                        (else (add1 (L (cdr l))))))))
           (Y₀ (let₁ (h '(λ (g) (f (λ (x) ((g g) x)))))
                `(λ (f) (,h ,h))))
           (Y (curryfy `(,Y₀ ,length₀)))
           (Y₁ (curryfy `((,Y₀ ,length₀) ones)))
           (E ((extend E₀)
               `(² . ,²)
               `(three . 3)
               `(* . ,*)
               `(add1 . ,add1)
               `(null? . ,null?)
               `(cdr . ,cdr)
               `(zero . 0)
               `(ones . ,(list 1 1 1))
               `(one . 1)))
           (s₀ (status-init E (list control)))
           (s₁ (status-init '() ((○ compile expression->de-bruijn) control₁)))
           (s₁⁺ (status-init '() ((○ compile⁺ expression->de-bruijn) control₁)))
           (s₂⁺ (status-init '() ((○ compile⁺ expression->de-bruijn) control₂)))
           (Y⁺ (status-init '() ((○ compile⁺ expression->de-bruijn) Y)))
           (Y₁⁺ (status-init '() ((○ compile⁺ expression->de-bruijn) Y₁)))
           (F (lambda (→)
               (lambda (s)
                (format #t "~a\n" s)
                (→ s))))
           (→/interpreted* (rtc (F →/interpreted)))
           (→/compiled* (rtc (F (→/compiled E))))
           (→/compiled⁺* (rtc (F (→/compiled⁺ E)))))

     (test-assert (equal? control control₁))

     (test "(() () ((((λ (f) (λ (x) (f (f x)))) ²) three)) #<unspecified>)"
      (to-string (status-init E (list control))))

     #;(test "" (to-string (→/interpreted* s₀)))

     (test `(81 ,"(((three) . 3) ((²) . #<procedure (commons#² x394)>))" ,'() ,(void))
      (let₁ (s (last (→/interpreted* s₀)))
       (list
        ((○ car status-S) s)
        ((○ to-string E->alist status-E) s)
        (status-C s)
        (status-D s))))

     (let₁ (t '(λ (x) ((g x) (λ (y) ((λ (z) (x y z)) (f x))))))
      (test "(λ ((g 0) (λ ((λ ((2 1) 0)) (f 1)))))"
       ((○ to-string expression->de-bruijn curryfy) t)))


     #;(let ((x 3) (y 4) (z 5)
           (t (curryfy '((λ (p) (p 3 4 5)) +))))
      (test 12 ((○ (value₊ E⁺) expression->de-bruijn) t)))


    (let* ((E ((extend E₀) `(three . 3) `(four . 4) `(+ . ,(curry₁ +))))
           (t (curryfy `((λ (p x) (p x three)) + four))))
     (test 7 ((○ (value₊ E) expression->de-bruijn) t))
     #;(test 7 ((○ (value₊ E⁺) expression->de-bruijn) t)))

    (test "((Load three) (Load ²) (Closure ((Closure ((Position 0) (Position 1) Apply (Position 1) Apply)))) Apply Apply)"
     ((○ to-string compile expression->de-bruijn) control))

    (test "((Load three) (Load ²) Enter (Closure ((Position 0) (Position&Apply 1) (Position&Apply 1))) Exit Apply)"
     ((○ to-string compile⁺ expression->de-bruijn) control₁))

    (test "((Load three) (Closure ((Position 0) (Position 0) (Load *) Apply Apply)) Enter (Closure ((Position 0) (Position&Apply 1) (Position&Apply 1))) Exit Apply)"
     ((○ to-string compile⁺ expression->de-bruijn) control₂))

    (test "(λ (f) ((λ (g) (f (λ (x) ((g g) x)))) (λ (g) (f (λ (x) ((g g) x))))))"
     (to-string Y₀))

    (test "(λ ((λ (1 (λ ((1 1) 0)))) (λ (1 (λ ((1 1) 0))))))"
     ((○ to-string expression->de-bruijn curryfy) Y₀))

    (test "((Closure ((Closure ((Closure ((Position 0) (Position 1) (Position&Apply 1) Apply)) (Position&Apply 1))) Enter (Closure ((Position 0) (Position 1) (Position&Apply 1) Apply)) (Position&Apply 1) Exit)))"
     ((○ to-string compile⁺ expression->de-bruijn curryfy) Y₀))


    (test `(81 ,'() ,'() ,(void))
      (let₁ (s (last (→/compiled* s₁)))
       (list
        ((○ car status-S) s)
        (status-E s)
        (status-C s)
        (status-D s))))

    (test `(81 ,'() ,'() ,(void))
      (let₁ (s (last (→/compiled⁺* s₁)))
       (list
        ((○ car status-S) s)
        (status-E s)
        (status-C s)
        (status-D s))))

    (test `(81 ,'() ,'() ,(void))
      (let₁ (s (last (→/compiled⁺* s₁⁺)))
       (list
        ((○ car status-S) s)
        (status-E s)
        (status-C s)
        (status-D s))))

    (test "[((Position 0) (Load null?) Apply (Test ((Position 0) (Load cdr) Apply (Position&Apply 1) (Load add1) Apply)) (Load zero)) ([((Position 0) (Position 1) (Position&Apply 1) Apply) ([((Closure ((Position 0) (Position 1) (Position&Apply 1) Apply)) (Position&Apply 1)) ([((Closure ((Position 0) (Load null?) Apply (Test ((Position 0) (Load cdr) Apply (Position&Apply 1) (Load add1) Apply)) (Load zero)))) ()])] [((Closure ((Position 0) (Load null?) Apply (Test ((Position 0) (Load cdr) Apply (Position&Apply 1) (Load add1) Apply)) (Load zero)))) ()])])]" 
     ((○ to-string car status-S last →/compiled⁺*) Y⁺))

    (test `(3 ,'() ,'() ,(void))
      (let* ((s₁ (status-init '() ((○ compile expression->de-bruijn) Y₁)))
             (s (last (→/compiled* s₁))))
       (list
        ((○ car status-S) s)
        (status-E s)
        (status-C s)
        (status-D s))))

    (test 3 ((○ car status-S last →/compiled⁺*) Y₁⁺))

    )






