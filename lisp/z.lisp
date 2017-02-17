(let 
  (z
    (lam f
      ((lam x
        (f
          (lam y ((x x) y))))
       (lam x
        (f
          (lam y ((x x) y)))))))

  (let 
    (sum
      (lam f
        (lam x
          (if (eq x 1)
            x
            (+ x (f (+ x -1)))))))
    
    ((z sum) 10)))
