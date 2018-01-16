;; Small Lisp Interpeter
(defn my-eval
  "Evaluate a list of lisp form"
  [form env]
  (cond
    (integer? form)  form
    (string?  form)  form
    (symbol?  form)
    (if-let [value (form env)]
      value
      (throw (Exception. "Unknown variable")))

    (list? form)
    (let [[func & args] form]
      (apply (my-eval func env)
             (map #(my-eval % env) args)))
    :else (throw (Exception. "Unknown form"))))


(defn my-global-eval
  [form]
  (my-eval form {'* *
                 '/ /
                 '- -
                 '+ +
                 'x 10
                 'y 20}))


(defn my-repl
  []
  (print "> ")
  (flush)
  (print (my-global-eval (read)))
  (newline)
  (recur))

(my-repl)
