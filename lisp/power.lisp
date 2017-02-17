(letrec 
  (* 
    (lam (a b)
      (if (eq b 1)
        a
        (+ a (* a (+ b -1))))))
(letrec
  (**
    (lam (a b)
      (if (eq b 1)
        a
        (* a (** a (+ b -1))))))

  (** 2 10)))
