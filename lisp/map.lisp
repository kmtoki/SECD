(do
  (letrec 
    (map
      (lam (f ls)
        (do
          (if (atom ls)
            (if (eq ls #nil)
              #nil
              (f ls))
            (cons (f (car ls)) (map f (cdr ls)))))))

    (map (lam x (puts x)) (cons 0 (cons 1 #nil)))))
