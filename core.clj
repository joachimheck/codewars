(ns codewars.core)

(require '[clojure.set :as set])
(require '[clojure.string :as string])
(require '[clojure.test :refer :all])

(defn max-sequence [xs]
  (if (or (empty? xs) (every? #(< % 0) xs))
    0
    (let [sums (loop [sums {1 xs}
                      l 2]
                 (if (> l (count xs))
                   sums
                   (recur (assoc sums l (vec
                                         (for [i (range (- (count xs) (dec l)))]
                                           (+ (get-in sums [(dec l) i]) (get xs (+ i (dec l)))))))
                          (inc l))))]
      (apply max (flatten (vals sums))))))





(defn prime? [n]
  (cond (< n 2) false
        (= n 2) true
        (even? n) false
        :else (loop [d 3]
                (cond (= d n) true
                      (= 0 (rem n d)) false
                      :else (recur (+ d 2))))))

(defn format-factor [[n p]]
  (if (= p 1)
    (str "(" n ")")
    (str "(" n "**" p ")")))

(defn lowest-prime-factor [n]
  (let [factor (first (filter #(= 0 (rem n %)) (filter prime? (range (inc (Math/sqrt n))))))
        factor (if (nil? factor) n factor)]
    [factor (last (take-while #(= 0 (rem n (long (Math/pow factor %)))) (range 1 (inc n))))]))

(defn prime-factors [n]
  (string/join
   (map format-factor
        (loop [n n
               factors {}]
          (if (= n 1)
            factors
            (let [[factor power] (lowest-prime-factor n)]
              (recur (quot n (long (Math/pow factor power)))
                     (assoc factors factor power))))))))

(deftest test-prime-factors
  (is (= "(2**5)(5)(7**2)(11)" (prime-factors 86240)))
  (is (= "(7919)" (prime-factors 7919)))
  (is (= "(7537)(123863)" (prime-factors 933555431))))



(defn street-fighter-selection [fighters position moves]
  (loop [[x y :as position] position
         moves moves
         outputs []]
    (if (empty? moves)
      outputs
      (let [[new-x new-y :as new-position] (case (first moves)
                                             "up" [x (max (dec y) 0)]
                                             "down" [x (min (inc y) 1)]
                                             "left" [(mod (dec x) 6) y]
                                             "right" [(mod (inc x) 6) y])
            new-moves (rest moves)
            new-outputs (conj outputs (get-in fighters [new-y new-x]))]
        (recur new-position new-moves new-outputs)))))



(defn meeting [s]
  (as-> s s
    (string/upper-case s)
    (string/split s #";")
    (map #(string/split % #":") s)
    (sort-by first s)
    (sort-by second s)
    (map (fn [[first last]] (format "(%s, %s)" last first)) s)
    (string/join s)))

(deftest a-test1
 (testing "meeting"
   (is (= "(ARNO, ANN)(BELL, JOHN)(CORNWELL, ALEX)(DORNY, ABBA)(KERN, LEWIS)(KORN, ALEX)(META, GRACE)(SCHWARZ, VICTORIA)(STAN, MADISON)(STAN, MEGAN)(WAHL, ALEXIS)"
          (meeting "Alexis:Wahl;John:Bell;Victoria:Schwarz;Abba:Dorny;Grace:Meta;Ann:Arno;Madison:STAN;Alex:Cornwell;Lewis:Kern;Megan:Stan;Alex:Korn")))
   (is (= "(BELL, MEGAN)(CORNWELL, AMBER)(DORNY, JAMES)(DORRIES, PAUL)(GATES, JOHN)(KERN, ANN)(KORN, ANNA)(META, ALEX)(RUSSEL, ELIZABETH)(STEVE, LEWIS)(WAHL, MICHAEL)"
          (meeting "John:Gates;Michael:Wahl;Megan:Bell;Paul:Dorries;James:Dorny;Lewis:Steve;Alex:Meta;Elizabeth:Russel;Anna:Korn;Ann:Kern;Amber:Cornwell")))))




(def adjacent {\1 #{\1 \2 \4}
               \2 #{\2 \1 \3 \5}
               \3 #{\3 \2 \6}
               \4 #{\4 \1 \5 \7}
               \5 #{\5 \2 \4 \6 \8}
               \6 #{\6 \3 \5 \9}
               \7 #{\7 \4 \8}
               \8 #{\8 \5 \7 \9 \0}
               \9 #{\9 \6 \8}
               \0 #{\0 \8}})

(defn get-pins [observed]
  (if (empty? observed)
    '("")
    (map string/join
         (apply concat
                (map (fn [f] (map (fn [r] (list f r))
                                  (get-pins (rest observed))))
                     (adjacent (first observed)))))))




(defn longest-slide-down [pyramid]
  (first
   (reduce (fn [prev-row row]
             (mapv #(+ (get row %) (max (get prev-row %) (get prev-row (inc %))))
                   (range (count row))))
           (reverse pyramid))))

(deftest longest-slide-down-test
  (testing "small pyramid"
    (is (= 23 (longest-slide-down
      [[3] [7 4] [2 4 6] [8 5 9 3]]
    ))))
  (testing "medium pyramid"
    (is (= 1074 (longest-slide-down [
                    [75]
                   [95 64]
                  [17 47 82]
                 [18 35 87 10]
                [20  4 82 47 65]
               [19  1 23 75  3 34]
              [88  2 77 73  7 63 67]
             [99 65  4 28  6 16 70 92]
            [41 41 26 56 83 40 80 70 33]
           [41 48 72 33 47 32 37 16 94 29]
          [53 71 44 65 25 43 91 52 97 51 14]
         [70 11 33 28 77 73 17 78 39 68 17 57]
        [91 71 52 38 17 14 91 43 58 50 27 29 48]
       [63 66  4 68 89 53 67 30 73 16 69 87 40 31]
      [ 4 62 98 27 23  9 70 98 73 93 38 53 60  4 23]
      ])) "medium pyramid")))




(defn remove-first [x coll]
  (let [[n m] (split-with (partial not= x) coll)]
    (concat n (rest m))))

(defn scramble [s1 s2]
  (reduce (fn [s1 c]
            (if (some #{c} s1)
              (remove-first c s1)
              (reduced false)))
          s1
          s2))

  




(defn in-u? [n]
  (and (integer? n)
       (or (= n 1)
           (in-u? (/ (dec n) 2))
           (in-u? (/ (dec n) 3)))))

;; (defn dblinear [n]
;;   (loop [i 1
;;          u-index 0]
;;     (if (= u-index n)
;;       i
;;       (recur (inc i)
;;              (if (in-u? (inc i))
;;                (inc u-index)
;;                u-index)))))

(defn dblinear [n]
  (first (drop n (filter in-u? (iterate inc 1)))))




(defn double-boxes [maxk maxn]
  (apply +
          (for [k (range 1 (inc maxk))
                n (range 1 (inc maxn))]
            (/ (* k (Math/pow (inc n) (* 2 k)))))))




(def unit-times {"second" 1
                 "minute" 60
                 "hour" (* 60 60)
                 "day" (* 24 60 60)
                 "year" (* 365 24 60 60)})

(defn compute-times [secs]
  (first
   (reduce (fn [[counts duration] unit]
             (let [amount (quot duration (get unit-times unit))]
               [(assoc counts unit amount) (- duration (* amount (get unit-times unit)))]))
           [{} secs]
           '("year" "day" "hour" "minute" "second"))))

(defn basic-format [times]
  (remove empty?
          (map (fn [[k v]]
                 (if (= v 0)
                   ""
                   (format "%d %s%s" v k (if (> v 1) "s" ""))))
               times)))

(defn andize [strings]
  (if (> (count strings) 1)
    (concat (drop-last strings) (list " and " (last strings)))
    strings))

(defn commaize [strings]
  (if (> (count strings) 3)
    (concat (map #(format "%s, " %) (drop-last 3 strings)) (take-last 3 strings))
    strings))

(defn make-readable [times]
  (let [strings (->> times
                     basic-format
                     andize
                     commaize)]
    (if (= (count strings) 0)
      "now"
      (string/join strings))))

(defn formatDuration [secs]
  (make-readable (compute-times secs)))




(defn move-before [s l ref]
  (let [s (remove #{l} s)]
    (concat (take-while #(not= % ref) s) (list l) (drop-while #(not= % ref) s))))

(defn move-after [s l ref]
  (let [s (remove #{l} s)]
    (concat (take-while #(not= % ref) s) (list ref l) (remove #{ref} (drop-while #(not= % ref) s)))))

(defn swap [s a b]
  (replace {a b b a} s))

(defn apply-ordering [s [x y z :as triplet]]
  (condp = (filter (set triplet) s)
    [x y z] s
    [x z y] (swap s y z)
    [y x z] (swap s x y)
    [y z x] (move-before s x y)
    [z x y] (move-after s z y)
    [z y x] (swap s x z)))

(defn apply-orderings [s triplets]
  (reduce (fn [s t] (apply-ordering s t))
          s
          triplets))

(defn recover-secret [triplets]
  (let [letters (distinct (apply concat triplets))
        f (fn [s] (apply-orderings s triplets))]
    (string/join (first (first (drop-while (fn [[a b]] (not= a b)) (partition 2 1 (iterate f letters))))))))




(defn get-clue-for [clues [type n end]]
  (if (= type :column)
    (if (= end 0)
      (get clues n)
      (get clues (- 11 n)))
    (if (= end 0)
      (get clues (- 15 n))
      (get clues (+ 4 n)))))

(defn no-zero [x]
  (if (= x 0) " " x))

(defn print-grid [grid clues]
  (string/join
   "\n"
   (concat
    (list (str " " (string/join (for [i (range 4)] (format "%11s" (no-zero (get-clue-for clues [:column i 0])))))))
    (for [j (range 4)]
      (string/join
       (concat
        (list (no-zero (get-clue-for clues [:row j 0])) " ")
        (for [i (range 4)]
          (format "%11s" (get grid [i j])))
        (list (no-zero (get-clue-for clues [:row j 3])) " "))))
    (list (str " " (string/join (for [i (range 4)] (format "%11s" (no-zero (get-clue-for clues [:column i 3]))))))))))

(def empty-grid (into {} (for [i (range 4) j (range 4)] [[i j] #{1 2 3 4}])))

(def combinations
  (let [f (fn perms [xs]
            (if (empty? xs)
              '(())
              (mapcat (fn [first] (map #(vec (concat (list first) %)) (perms (remove #{first} xs)))) xs)))]
    (f [1 2 3 4])))

(defn get-spaces-for [clue-index]
  (cond (<= 0 clue-index 3) (map (fn [i] [clue-index i]) (range 4))
        (<= 4 clue-index 7) (map (fn [i] [(- 3 i) (- clue-index 4)]) (range 4))
        (<= 8 clue-index 11) (map (fn [i] [(- 11 clue-index) (- 3 i)]) (range 4))
        (<= 12 clue-index 15) (map (fn [i] [i (- 15 clue-index)]) (range 4))))

(defn get-cells-for [grid clue-index]
  (map #(get grid %) (get-spaces-for clue-index)))

(defn cross-spaces [[x y :as pos]]
  (remove #{pos}
          (concat
           (for [i (range 4)] [i y])
           (for [j (range 4)] [x j]))))

(defn possible-combinations [quad]
  (filter (fn [combo]
            (every? true? (for [i (range 4)]
                            (contains? (nth quad i) (get combo i)))))
          combinations))

(defn make-grid-cells [combinations]
  (reduce (fn [quad combo]
            (reduce (fn [quad i]
                      (update quad i #(conj % (get combo i))))
                    quad
                    (range 4)))
          (vec (repeat 4 #{}))
          combinations))

(defn count-visible [heights]
  (second
   (reduce (fn [[max-h visible] height] [(max max-h height) (if (> height max-h) (inc visible) visible)])
           [0 0]
           heights)))

(defn possible-sequences [clue quad]
  (filter #(= clue (count-visible %)) (possible-combinations quad)))

(defn apply-clues [grid clues]
  (let [clue-map (zipmap (iterate inc 0) clues)]
    (reduce (fn [grid [clue-index clue]]
              (if (zero? clue)
                grid
                (reduce (fn [grid [pos cell]]
                          (assoc grid pos cell))
                        grid
                        (map list (get-spaces-for clue-index) (make-grid-cells (possible-sequences clue (get-cells-for grid clue-index)))))))
            grid
            clue-map)))

(defn reduce-grid [grid clues]
  (reduce (fn [grid pos]
            (let [cell (get grid pos)
                  cross-cells (map #(get grid %) (cross-spaces pos))
                  cross-values (set (remove nil? (map #(if (= 1 (count %)) (first %)) cross-cells)))
                  new-possibles (set (remove cross-values cell))]
              (assoc grid pos new-possibles)))
          (apply-clues grid clues)
          (for [x (range 4) y (range 4)] [x y])))

(defn outputize [grid]
  (vec
   (for [j (range 4)]
     (vec
      (for [i (range 4)]
        (first (get grid [i j])))))))

(defn solve-puzzle [clues]
  (loop [grid empty-grid
         previous-grids []]
    (if (every? (comp #(= 1 %) count second) grid)
        (outputize grid)
        (recur (reduce-grid grid clues) (conj previous-grids grid)))))




;; Kata: Strip Comments
(defn strip-comments [text comment-symbols]
  (let [lines (string/split text #"\n")
        symbols (string/join comment-symbols)
        pattern (re-pattern (str "(.*?)([" symbols "].*)?"))]
    (string/join
     "\n"
     (for [line lines]
       (string/trimr (second (re-matches pattern line)))))))




;; Kata: Sum of Intervals
(defn overlap? [[a b] [c d]]
  (or (<= a c b)
      (<= a d b)
      (<= c a d)
      (<= c b d)))

(defn combine-intervals [[a b] [c d]]
  [(min a c) (max b d)])

(defn sum-intervals [intervals]
  (let [min-intervals (reduce (fn [acc i]
                                (let [overlap (first (for [x acc :when (overlap? i x)] x))]
                                  (if overlap
                                    (conj (disj acc overlap) (combine-intervals i overlap))
                                    (conj acc i))))
                              #{}
                              (sort intervals))]
    (apply + (map (fn [[a b]] (- b a)) min-intervals))))




;; Kata: Sum by Factors
(defn prime? [n]
  (cond (= 2 n) true
        (even? n) false
        :else (nil? (first (for [i (iterate inc 3)
                                 :while (<= i (Math/sqrt n))
                                 :when (= 0 (rem n i))] i)))))

(defn primes []
  (concat '(2) (filter prime? (iterate inc 3))))

(defn prime-factors [n]
  (let [n (Math/abs n)
        prime-factors (reduce (fn [factors p]
                                (if (> p (/ n 2))
                                  (reduced factors)
                                  (if (= 0 (rem n p))
                                    (conj factors p)
                                    factors)))
                              []
                              (primes))]
    (if (and (empty? prime-factors) (not= 1 n))
      [n]
      prime-factors)))

(defn sum-of-divided [lst]
  (sort
   (apply merge-with +
          (for [f (set (flatten (map prime-factors lst)))
                n lst
                :when (= 0 (rem n f))]
            {f n}))))
