(ns genetic.core
	(:gen-class))

(defn count-fitness
  "Counts chromosome fitness by calculating it's similarity to the ideal one."
  [chromosome word]
  (let [similarities (map #(= %1 %2) chromosome word)
        text-length (count word)
        score (count (filter identity similarities))]
    (/ score text-length)))

; Lowercase alphabet
(def lowercase (reduce str
                       (map char
                            (range (int \a) (inc (int \z))))))

(def digits (reduce str (range 0 10)))

; Available signs
(def signs (str (clojure.string/upper-case lowercase) lowercase " .-!,;:()\"'<>&~?%" digits))

; Generates random letter.
(def rand-letter #(rand-nth signs))

(defn rand-string
  "Generates random n-length random string."
  [n]
  (reduce str
          (take n (repeatedly #(rand-letter)))))

; Determines how often chromosomes should mutate.
(def mutation-rate 0.05)

; Maximum size of population.
(def pop-max 500)

(defn split-chromosomes
  "Splits chromosomes at random index"
  [c1 c2]
  (let [len (count c1)
        index (int (* len (rand)))]
    (map #(split-at index %) [c1 c2])))

(defn crossover
  "Creates new chromosome by crossover"
  [c1 c2]
  (let [splitted (split-chromosomes c1 c2)
        first-splitted (first splitted)
        second-splitted (last splitted)]
    (reduce str (concat (first first-splitted) (last second-splitted)))))

(defn generate-pop
  "Generates new population of n-length strings."
  [n]
  (take pop-max (repeatedly #(rand-string n))))

(defn pick-best
  "Picks word with the best fitness."
  [population word]
  (last (sort-by #(count-fitness % word) population)))

(defn fitnesses
  "Fitnesses of all chromosomes in population"
  [population word]
  (map #(count-fitness % word) population))

(defn best-fitness
  "Finds chromosome with best fitness in population"
  [population word]
  (apply max (fitnesses population word)))

(defn sum-of-fitnesses
  "Counts sum of all fitnesses in population"
  [population word]
  (reduce + (fitnesses population word)))

(defn shares
  "Count shares of chromosomes in new gene pool"
  [population word]
  (let [fitnesses-calculated (fitnesses population word)
        sum-calculated (sum-of-fitnesses population word)]
    (map #(Math/ceil (* pop-max (/ % sum-calculated)))
         fitnesses-calculated)))

(defn mutate
  "Applies random mutation to given chromosome"
  [chromosome]
  (reduce str (assoc (vec (char-array chromosome)) (int (* (count chromosome) (rand))) (rand-letter))))

(defn gene-pool
  "Generates gene pool"
  [population word]
  (flatten (map #(take %1 (repeat %2)) (shares population word) population)))

(defn mutated
  "Generates population after mutations applied"
  [population]
  (map #(if (<= (rand) mutation-rate) (mutate %) %) population))

(defn better-population
  "Creates next generation"
  [population word]
  (let [new-gene-pool (gene-pool population word)
        random-gene #(rand-nth new-gene-pool)]
    (mutated (take pop-max (repeatedly #(crossover (random-gene) (random-gene)))))))

(defn print-info
  "Print current population status"
  [generation best-chromosome current-best-fitness]
  (do
    (println "Generation #" generation)
    (println "Best in population: " best-chromosome)
    (println "Current best fitness:" current-best-fitness)))

(defn simulate
  "Simulates infinite monkey theorem supported by genetic algorithm."
  [word]
  (loop [population (generate-pop (count word))
         generation 0]
    (let [best-chromosome (pick-best population word)
          current-best-fitness (count-fitness best-chromosome word)]
      (do
        (print-info generation best-chromosome current-best-fitness)
        (if (= current-best-fitness 1)
          (println "Finished!")
          (recur (better-population population word) (inc generation)))))))
(defn -main
  [& args]
  (try
  (if(empty? args)
	(do
		(println "Type a phrase for our simulation:")
		(simulate (read-line))
	(simulate (last args))))
	(catch Exception e "Exception occured")))
