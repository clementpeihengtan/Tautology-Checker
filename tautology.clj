(declare left)
(declare right)
(declare op)
(declare normalizing)
(declare normalizes)
(declare substitute)
(declare simplifying)
(declare if-test)
(declare if-then)
(declare if-else)

(def ifify
  (fn [P]
    (cond
      (not (seq? P))
        P
      (and (= (op P) 'not) (not (seq? (left P))) (not (seq? (right P))))
        (list 'if (left P) 'false 'true)
      (and (= (op P) 'and) (not (seq? (left P))) (not (seq? (right P))))
        (list 'if (left P) (right P) 'false)
      (and (= (op P) 'or) (not (seq? (left P))) (not (seq? (right P))))
        (list 'if (left P) 'true (right P))
      (and (= (op P) 'imply) (not (seq? (left P))) (not (seq? (right P))))
        (list 'if (left P) (right P) 'true)
      (and (= (op P) 'equiv) (not (seq? (left P))) (not (seq? (right P))))
        (list 'if (left P) (right P) (list 'if (right P) 'false 'true))
      (= (op P) 'not)
        (list 'if (ifify (left P)) 'false 'true)
      (= (op P) 'and)
        (list 'if (ifify (left P)) (ifify (right P)) 'false)
      (= (op P) 'or)
        (list 'if (ifify (left P)) 'true (ifify (right P)))
      (= (op P) 'imply)
        (list 'if (ifify (left P)) (ifify (right P)) 'true)
      (= (op P) 'equiv)
        (list 'if (ifify (left P)) (ifify (right P)) (list 'if (ifify (right P)) 'false 'true))
      :else
       '()
    )
  )
)

(def normalize
  (fn [C]
    (if (and (seq? (if-then C)) (seq? (if-test (if-then C))))
      (normalize (list 'if (if-test C) (normalizing (if-then C)) (if-else C)))
      (if (and (seq? (if-else C)) (seq? (if-test (if-else C))))
        (normalize (list 'if (if-test C) (if-test C) (normalizing (if-else C))))
        (if (seq? (if-test C))
          (normalize (normalizing C))
          (list 'if (if-test C) (if-then C) (if-else C))
        )
      )
    )
  )
)

(def normalizing
  (fn [C]
    (if (not (seq? C))
      C
      (list 'if (if-test (if-test C)) (list 'if (if-then (if-test C)) (if-then C) (if-else C))
      (list 'if (if-else (if-test C)) (if-then C) (if-else C)))
    )
  )
)

(def simplify
  (fn [C]
    (let [temp C]
      (if (not (seq? C))
      C
      (if (= temp (simplifying C))
        false
        (if (and (seq? C) (not (seq? (if-test C))) (not (seq? (if-then C))) (not (seq? (if-else C))))
          (simplify (simplifying C))
          (if (seq? C)
            (simplify (simplifying C))
            (simplify (list 'if (simplifying (if-test C)) (simplifying (if-then C)) (simplifying (if-else C))))
          )
        )
      )
    ))
  )
)

(def simplifying
  (fn [C]
    (cond
      (not (seq? C))
        C
      (= (if-test C) 'true)
        (if-then C)
      (= (if-test C) 'false)
        (if-else C)
      (= (if-then C) (if-else C))
        (if-then C)
      (and (= (if-then C) 'true) (= (if-else C) 'false))
        (if-test C)
      :else
        (list 'if (if-test C) (simplify (substitute (if-then C) (if-test C) 'true)) (simplify (substitute (if-else C) (if-test C) 'false)))
    )
  )
)

(def substitute
  (fn [C V B]
    (if (= C V)
      B
      (if (and (not (= C V)) (or (symbol? C) (instance? Boolean C)))
        C
        (if (seq? C)
          (list 'if (substitute (if-test C) V B) (substitute (if-then C) V B) (substitute (if-else C) V B))
        )
      )
    )
  )
)

(def tautology?
  (fn [P]
    (simplify (normalize (ifify P)))
  )
)

(def op
  (fn [S]
    (first S)
  )
)

(def left
  (fn [P]
    (first (rest P))
  )
)

(def right
  (fn [P]
    (first (rest (rest P)))
  )
)

(def if-test
  (fn [C]
    (second C)
  )
)

(def if-then
  (fn [C]
    (first (rest (rest C)))
  )
)

(def if-else
  (fn [C]
    (first (rest (rest (rest C))))
  )
)
