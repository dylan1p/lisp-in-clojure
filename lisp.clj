(defn boolean?
  [x]
  (or (= x true) (= x false)))

(declare my-eval)

;; Small Lisp Interpeter
(def global-env
  (atom
   {'* *
    '/ /
    '- -
    '= =
    '+ +
    'not not
    'print print}))


(defn my-eval-variable
  "Evaluate a variable in a local environment"
  [v local-env]
  (cond
    (contains? local-env v) (v local-env)
    (contains? @global-env v) (v @global-env)
    :else (throw (Exception. "Unknown variable"))))

(defn my-eval-funcall
  "Evaluate a function call in a local environment"
  [func args local-env]
  (apply (my-eval func local-env)
         (map #(my-eval % local-env) args)))

(defn my-eval
  "Evaluate a list of lisp form"
  [form local-env]
  (cond
    (integer? form)  form
    (string?  form)  form
    (symbol?  form)  (my-eval-variable form local-env)
    (boolean? form) form
    (list? form)
    (let [[op & args] form]
      (case op
        ;; Special forms
        quote      (first args)
        if         (let [[cond if-true if-false] args]
                     (if (my-eval cond local-env)
                       (my-eval if-true local-env)
                       (my-eval if-false local-env)))
        def        (let [[variable-name value] args]
                     (swap! global-env #(assoc % variable-name (my-eval value local-env)))
                     nil)
        lambda     (let [[fnargs body] args]
                     ;; fargs = (z)
                     ;; rest = (10)
                     ;; your new local-env =
                     ;; { z 10 ...old local-env }
                     (fn [& rest]
                       (let [newenv (into local-env (map vector fnargs rest))]
                         (my-eval body newenv))))


        do          (doall (map #(my-eval % local-env) args))

        ;; Otherwie, op must be a function
        (my-eval-funcall op args local-env)))

    :else (throw (Exception. "Unknown form"))))


(defn my-global-eval
  [form]
  (my-eval form {}))


(defn my-load-file
  [file]
  (with-open [in (java.io.PushbackReader. (clojure.java.io/reader file))]
    (let [eof (Object.)]
      (let [forms (take-while #(not= % eof) (repeatedly #(read in false eof)))]
        (doall (map #(my-global-eval %) forms))))))

(defn my-repl
  []
  (print "> ")
  (flush)
  (print (my-global-eval (read)))
  (newline)
  (recur))

(my-load-file "./core.lisp")
(my-repl)
