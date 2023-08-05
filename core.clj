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
  (or (= n 2)
      (and (> n 2) (odd? n) (nil? (first (for [i (iterate #(+ 2 %) 3)
                                               :while (<= i (Math/sqrt n))
                                               :when (= 0 (rem n i))] i))))))

(defn primes []
  (filter prime? (iterate inc 2)))

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




;; Kata: Square into Squares
(defn decompose-simple-inner [n sum]
  (if (= sum 0)
    [[]]
    (apply concat
           (for [x (reverse (range (min n (inc (Math/sqrt sum)))))]
             (map #(conj % x) (decompose-simple-inner x (- sum (* x x))))))))

(defn decompose-simple [n]
  (println "decompose" n)
  (seq (first (decompose-simple-inner n (* n n)))))


(defn decompose-inner [n sum]
  (if (= sum 0)
    [[]]
    (first
       (for [x (iterate dec (dec (min n (inc (int (Math/sqrt sum))))))
               :while (> x 0)
               :let [diff (- sum (* x x))]
               decomposed (decompose-inner x diff)]
           (list (conj decomposed x))))))

(defn decompose [n]
  (first (decompose-inner n (* n n))))




;; Kata: Fabergé Easter Eggs crush test
;; (defn build-tree [eggs tries]
;;   (cond (or (= eggs 0) (= tries 0))
;;         (list {:eggs eggs :tries tries :floors 0})
;;         (or (= eggs 1) (= tries 1))
;;         (list {:eggs eggs :tries tries :floors (inc tries)} '() '())
;;         :else
;;         (let [left (build-tree eggs (dec tries))
;;               right (build-tree (dec eggs) (dec tries))]
;;           ;; (println left right (:floors (first left)) (:floors (first right)))
;;           (list {:eggs eggs :tries tries :floors (+ (:floors (first left)) (:floors (first right)))} left right))))

;; (defn height
;;   "Given n eggs and m tries, what's the greatest height
;;   for which the safe drop height for the eggs can be calculated?"
;;   [n m]
;;   (dec (:floors (first (build-tree n m)))))

;; depth = tries
;; if (n >= m), height = (dec (Math/pow 2 m))
;; The difference comes from a partially-summed pascal triangle (thanks, oeis.org!)
;;  0: 1
;;  1: 1  2
;;  2: 1  3  4
;;  3: 1  4  7  8
;;  4: 1  5 11 15 16

;; The result comes from Bernoulli's triangle (thanks, oeis.org!)
;; The Bernoulli value is the partial sum of a row of Pascal's triangle.
(defn height [n m]
  (let [row m
        col (min n m)]
    (reduce (fn [[sum prev] k]
              (if (> k col)
                (reduced (dec sum))
                (let [term (* prev (/ (- (inc row) k) k))]
                  [(+ sum term) term])))
            [1 1]
            (iterate inc 1))))




;; Kata: Getting Along with Integer Partitions
(def enum-cache (atom {}))

(defn enum-memo [n]
  (if-not (get @enum-cache n)
    (reset! enum-cache
                (assoc @enum-cache n
                       (if (= n 0)
                         '([])
                         (apply concat
                                (for [i (range n)]
                                  (map #(apply conj [(- n i)] %)
                                       (filter #(or (empty? %) (<= (first %) (- n i))) (enum-memo i)))))))))
  (get @enum-cache n))

(defn prod-memo [n]
  (sort (distinct (map #(reduce * %) (enum-memo n)))))

(defn part [n]
  (reset! enum-cache {})
  (let [p (prod-memo n)
        size (count p)
        range (- (last p) (first p))
        average (/ (reduce + p) size)
        median (if (odd? size)
                 (nth p (/ (dec size) 2))
                 (/ (+ (nth p (/ size 2)) (nth p (dec (/ size 2)))) 2))]
    (format "Range: %d Average: %.2f Median: %.2f" range (double average) (double median))))




;; Kata: Smallest Possible Sum
(defn solution [arr]
  (loop [s (into (sorted-set) arr)]
    (if (apply = s)
      (* (count arr) (first s))
      (recur (into (sorted-set)
                   (map (fn [[a b]] (if (and b (> a b)) (- a b) a))
                        (partition-all 2 1 (rseq s))))))))




;; Kata: Counting Change Combinations
(defn count-change-inner [money coins]
  (if (= money 0)
    [[]]
    (apply concat
           (for [c (reverse (sort coins)) :when (<= c money)]
             (map #(conj % c)
                  (count-change-inner (- money c) coins))))))

(defn count-change
  "Gives the number of ways to make change for some money given a set of coins"
  [money coins]
  (count (distinct (map sort (count-change-inner money coins)))))




;; Kata: Total increasing or decreasing numbers up to a power of 10

(def inc-dec-cache (atom {}))

(defn count-dec-first-and-digits [n digits]
  (or (get @inc-dec-cache [n digits :dec])
      (do (reset! inc-dec-cache
                  (assoc @inc-dec-cache
                         [n digits :dec]
                         (if (= digits 1)
                           1
                           (apply + (for [i (range (inc n))]
                                      (count-dec-first-and-digits i (dec digits)))))))
          (get @inc-dec-cache [n digits :dec]))))

(defn count-inc-first-and-digits [n digits]
  (or (get @inc-dec-cache [n digits :inc])
      (do (reset! inc-dec-cache
                  (assoc @inc-dec-cache
                         [n digits :inc]
                         (if (= digits 1)
                           1
                           (apply + (for [i (range n 10)]
                                      (count-inc-first-and-digits i (dec digits)))))))
          (get @inc-dec-cache [n digits :inc]))))

(defn total-inc-dec [p]
  (reset! inc-dec-cache {})
  (let [digits p]
    (inc
     (apply +
            (for [d (range 1 (inc digits))]
              (apply +
                     (for [n (range 1 10)]
                       (dec (+ (count-dec-first-and-digits n d) (count-inc-first-and-digits n d))))))))))



;; Kata: Hamming Numbers

(defn hamming [n]
  (loop [i 0
         open-set (sorted-set 1)]
    (let [new-h (first open-set)]
      (if (= i n)
        new-h
        (recur (inc i)
               (conj (disj open-set new-h) (* 2 new-h) (* 3 new-h) (* 5 new-h)))))))




;; Kata: NIM the Game
(defn choose-move [game-state]
  (let [nim-sum (apply bit-xor game-state)
        indexed (map-indexed list game-state)]
    (if (zero? nim-sum) ; we lose, do anything
      (first (filter #(pos? (second %)) indexed))
      (let [[i x] (first (filter (fn [[i x]] (< (bit-xor nim-sum x) x)) indexed))]
        [i (- x (bit-xor nim-sum x))]))))




;; Kata: Which are In?
(defn in-array [array1 array2]
  (sort (distinct (for [s1 array1 :when (some #(string/includes? % s1) array2)] s1))))




;; Kata: Tribonacci Sequence
(defn tribonacci [[a b c :as signature] n]
  (let [sequence (loop [s signature]
                   (if (>= (count s) n)
                     s
                     (recur (conj s (apply + (subvec s (- (count s) 3)))))))]
    (subvec sequence 0 n)))




;; Kata: Directions Reduction
(defn opposite? [a b]
  (some #{(set [a b])} '(#{"NORTH" "SOUTH"} #{"EAST" "WEST"})))

(defn dirReduc [arr]
  (seq
   (reduce #(if (opposite? (last %1) %2)
              (vec (drop-last %1))
              (conj %1 %2))
           []
           arr)))




;; Kata: Backwards Read Primes
(defn backwards-prime [start stop]
  (for [n (range start (inc stop))
        :let [reverse-n (Long/parseLong (string/join (reverse (str n))))]
        :when (and (not= n reverse-n)
                   (prime? n)
                   (prime? reverse-n))]
    n))




;; Kata: Positions Average
(defn common-positions [a b]
  (count (remove false? (map = a b))))

(defn pos-average [s]
  (let [strings (string/split s #", ")
        n (count strings)
        total (* (count (first strings)) (/ (* n (dec n)) 2))
        common (apply + (for [i (range n)
                              j (range (inc i) n)]
                          (common-positions (nth strings i) (nth strings j))))]
    (double (* 100 (/ common total)))))




;; Kata: Steps in K-primes
(defn all-prime-factors [n]
  (loop [m n
         factors []]
    (if-let [prime-factor (first (filter #(= 0 (rem m %))
                                         (take-while #(<= % (inc (Math/sqrt m))) (primes))))]
      (recur (/ m prime-factor) (conj factors prime-factor))
      (cond (and (empty? factors) (> n 1))
            [n]
            (prime? m)
            (conj factors m)
            :else
            factors))))

(defn k-primes-from [k start]
  (filter #(= k (count (all-prime-factors %)))
          (iterate inc (int (max start (Math/pow 2 k))))))

(defn steps [[first & xs]]
  (if (empty? xs)
    '()
    (concat
     (for [x xs] (list (- x first) [first x]))
     (steps xs))))

(defn kprimes-step [k step start end]
  (map second
       (filter #(= step (first %))
               (steps (take-while #(<= % end) (k-primes-from k start))))))




;; Kata: Sums of Parts
;; Should have used reductions!
(defn parts-sums [ls]
  (reduce (fn [parts-sums x]
            (conj parts-sums (- (get parts-sums (dec (count parts-sums))) x)))
          [(apply + ls)]
          ls))




;; Kata: Common Denominators
(defn prime-factorization [n]
  (loop [m n
         factors {}]
    (if-let [prime-factor (first (filter #(= 0 (rem m %))
                                         (take-while #(<= % (inc (Math/sqrt m))) (primes))))]
      (let [pow (last (take-while #(= 0 (rem n (long (Math/pow prime-factor %)))) (iterate inc 1)))]
        (recur (quot m (long (Math/pow prime-factor pow))) (assoc factors prime-factor pow)))
      (if (= m 1)
        factors
        (assoc factors m 1)))))

(defn reduce-fraction [n d]
  (let [n-pfs (prime-factorization n)
        d-pfs (prime-factorization d)
        shared-factors (filter #(get d-pfs %) (keys n-pfs))
        gcm (apply * (map #(* % (min (get n-pfs %) (get d-pfs %))) shared-factors))]
    [(quot n gcm) (quot d gcm)]))

(defn pow
  ([x p] (pow x p 1))
  ([x p i]
   (if (= p 0)
     (* i 1)
     (recur x (dec p) (* i x)))))

(defn least-common-multiple [ns]
  (long (apply *'
              (map (fn [[k v]] (pow k v))
                   (apply merge-with max (map #(prime-factorization %) ns))))))

(defn convert-fracts [lst]
  (let [reduced-lst (map (fn [[n d]] (reduce-fraction n d)) lst)
        lcd (least-common-multiple (map second reduced-lst))]
    (map (fn [[n d]]
           [(* n (quot lcd d)) lcd])
         reduced-lst)))




;; Kata: Rectangle Rotation

(defn rotate [x y phi]
  (let [r (Math/sqrt (+ (* x x) (* y y)))
        theta (+ phi (Math/atan2 y x))]
    [(* r (Math/cos theta)) (* r (Math/sin theta))]))

(defn maximums [coords]
  (mapv int
        (reduce (fn [[minx miny maxx maxy] [x y]]
                  [(min minx x) (min miny y) (max maxx x) (max maxy y)])
                [Integer/MAX_VALUE Integer/MAX_VALUE Integer/MIN_VALUE Integer/MIN_VALUE]
                coords)))

(defn intercept [[x1 y1] [x2 y2] y]
  (let [m (/ (- y2 y1) (- x2 x1))
        b (- y1 (* m x1))]
    (int (Math/floor (/ (- y b) m)))))

(defn rectangle-rotation [a b]
  (let [half-a (/ a 2)
        half-b (/ b 2)
        coords [[half-a half-b] [(- half-a) half-b] [(- half-a) (- half-b)] [half-a (- half-b)]]
        rot-coords (mapv (fn [[x y]] (rotate x y (/ Math/PI 4))) coords)
        [_ miny _ maxy] (maximums rot-coords)]
    (apply + 
           (for [j (range miny (inc maxy))
                 :let [left-intercept (if (>= j (get-in rot-coords [1 1]))
                                        (intercept (get rot-coords 0) (get rot-coords 1) j)
                                        (intercept (get rot-coords 1) (get rot-coords 2) j))
                       right-intercept (if (>= j (get-in rot-coords [3 1]))
                                         (intercept (get rot-coords 3) (get rot-coords 0) j)
                                         (intercept (get rot-coords 2) (get rot-coords 3) j))]]
             (- right-intercept left-intercept)))))




;; Kata: Consecutive Strings
(defn longest-cons [strarr k]
  (->> strarr
       (partition k 1)
       (map string/join)
       (reduce (fn [longest s] (second (sort-by count [s longest]))) "")))




;; Kata: Mexican Wave
(defn wave [s]
  (remove #(= % s)
          (for [i (range (count s))]
            (string/join (concat (take i s)
                                 (list (string/upper-case (str (get s i))))
                                 (drop (inc i) s))))))




;; Kata: Getting along with Bernoulli's numbers
(def factorial-cache (atom {}))

(defn factorial [n]
  (if-not (get @factorial-cache n)
    (swap! factorial-cache assoc n
           (if (= n 0) 1 (* (bigint n) (factorial (dec n))))))
  (get @factorial-cache n))

(defn binomial [n k]
  (/ (factorial n) (* (factorial k) (factorial (- n k)))))

(def bernoulli-cache (atom {}))

(defn bernoulli [n]
  (if-not (get @bernoulli-cache n)
    (swap! bernoulli-cache assoc n
           (- (if (= n 0) 1 0)
              (reduce (fn [acc k] (+ acc (/ (* (binomial n k) (bernoulli k)) (+ n (- k) 1))))
                      0
                      (range n)))))
  (get @bernoulli-cache n))

(defn abs [n]
  (if (neg? n) (-' n) n))

(defn series [k nb]
  (reset! factorial-cache {})
  (reset! bernoulli-cache {})
  (cond (and (odd? k) (> k 2))
        (apply + (for [n (range 1 (inc nb))] (/ 1 (Math/pow n k))))
        (and (even? k) (>= k 2))
        (* 0.5 (abs (double (bernoulli k))) (/ (Math/pow (* 2 Math/PI) k) (factorial k)))
        (< k 0)
        (* (Math/pow -1 (abs k)) (/ (bernoulli (inc (abs k))) (inc (abs k))))))




;; Kata: Split Strings
(defn solution [s]
  (->> (if (even? (count s)) s (string/join (list s \_)))
       (partition-all 2 2)
       (map string/join)))




;; Kata: My smallest code interpreter (aka Brainf**k)
(defn jump-forward [ip dp source memory]
  (if (= (get memory dp) 0)
    (loop [new-ip (inc ip)
           depth 1]
      (if (= depth 0)
        new-ip
        (case (get source new-ip)
          \[ (recur (inc new-ip) (inc depth))
          \] (recur (inc new-ip) (dec depth))
          (recur (inc new-ip) depth))))
    (inc ip)))

(defn jump-backward [ip dp source memory]
  (if (not= (get memory dp) 0)
    (loop [new-ip (dec ip)
           depth 1]
      (if (= depth 0)
        (+ 2 new-ip)
        (case (get source new-ip)
          \] (recur (dec new-ip) (inc depth))
          \[ (recur (dec new-ip) (dec depth))
          (recur (dec new-ip) depth))))
    (inc ip)))

(defn execute-string [source input]
  (println "execute-string" source (map int input))
  (loop [ip 0
         dp 0
         memory (vec (repeat 30000 0))
         input (map int input)
         output []]
    (if (or (< ip 0) (>= ip (count source)))
      (if (empty? output) nil (string/join (map char output)))
      (case (get source ip)
        \> (recur (inc ip) (inc dp) memory input output)
        \< (recur (inc ip) (dec dp) memory input output)
        \+ (recur (inc ip) dp (update memory dp #(mod (inc %) 256)) input output)
        \- (recur (inc ip) dp (update memory dp #(mod (dec %) 256)) input output)
        \. (recur (inc ip) dp memory input (conj output (get memory dp)))
        \, (recur (inc ip) dp (assoc memory dp (first input)) (rest input) output)
        \[ (recur (jump-forward ip dp source memory) dp memory input output)
        \] (recur (jump-backward ip dp source memory) dp memory input output)))))




;; Kata: Exponentials as Fractions
(defn digits
  ([n] (digits n '()))
  ([n ds]
   (if (= n 0)
     (if (empty? ds) '(0) ds)
     (recur (quot n 10) (conj ds (rem n 10))))))

(defn rational-pow
  ([n k] (rational-pow (rationalize n) k (bigint 1)))
  ([n k result]
   (if (= k 0)
     result
     (recur n (dec k) (* result n)))))

(defn expand [x n-digits]
  (reduce (fn [sum k]
            (if (and (ratio? sum) (>= (count (digits (numerator sum))) n-digits))
              (reduced [(numerator sum) (denominator sum)])
              (+ sum (/ (rational-pow x k) (factorial k)))))
          1
          (iterate inc 1)))




;; Strings Mix
(defn lower-freqs [s]
  (into {} (filter #(> (second %) 1) (frequencies (string/join (re-seq #"[a-z]" s))))))

(defn merge-freqs [freqs1 freqs2]
  (for [c (distinct (concat (keys freqs1) (keys freqs2)))
        :let [diff ((fnil compare 0 0) (get freqs1 c) (get freqs2 c))]]
    (cond (> diff 0) [c \1 (get freqs1 c)]
          (< diff 0) [c \2 (get freqs2 c)]
          :else [c \= (get freqs1 c)])))

(defn mix [s1 s2]
  (string/join
   "/"
   (map (fn [[c s n]]
          (string/join (concat (list s ":") (repeat n c))))
        (sort-by (comp - last) (sort-by #(get {\1 1 \2 2 \= 3} (second %)) (sort-by first (merge-freqs (lower-freqs s1) (lower-freqs s2))))))))




;; Perimeter of squares in a rectangle
(def fibonacci-cache (atom {}))

(defn fibonacci [n]
  (case n
    0 1
    1 1
    (do
      (if-not (get @fibonacci-cache n)
        (swap! fibonacci-cache assoc n (+' (fibonacci (- n 1)) (fibonacci (- n 2)))))
      (get @fibonacci-cache n))))

(defn perimeter [n]
  (* 4 (apply +' (for [i (range (inc n))] (fibonacci i)))))




;; One line task: Is the King in Check?

;; 527 (defn is-check [b](let [r (range 8)](boolean (some (set (for [y r x r :when (= "♔" (get-in b [y x]))] [y x]))(let [t [[1 1][1 -1][-1 1][-1 -1]] u [[1 0][-1 0][0 1][0 -1]] vrs {"♟" [[[1 1][-1 1]][0]] "♞" [[[-1 -2][1 -2][-1 2][1 2][2 -1][2 1][-2 -1][-2 1]][0]] "♝" [t r] "♜" [u r] "♛" [(concat t u) r] "♔" [[] nil] " " [[] nil]}](apply concat (for [y r x r](let [[vs r] (get vrs (get-in b [y x]))](for [[l m] vs](first (filter #(not= " " (get-in b %)) (map #(vector (+ y (* (inc %) m)) (+ x (* (inc %) l))) r))))))))))))

;; 411 (defn is-check [b](contains? (let [r (range 8) t [[1 1][1 -1][-1 1][-1 -1]] u [[1 0][-1 0][0 1][0 -1]] vrs {"♟" [[[1 1][-1 1]][0]] "♞" [[[-1 -2][1 -2][-1 2][1 2][2 -1][2 1][-2 -1][-2 1]][0]] "♝" [t r] "♜" [u r] "♛" [(concat t u) r]}] (set (for [y r x r :let [[vs r] (get vrs (get-in b [y x]) [])] [l m] vs] (first (remove #{" "} (map #(get-in b [(+ y (* m %)) (+ x (* l %))] " ") (map inc r))))))) "♔"))

;; 352 (defn is-check [b](contains? (let [g get-in r (range 8) n -1 t [[1 1][1 n][n 1][n n]] u [[1 0][n 0][0 1][0 n]] m {"♟"[[[1 1][n 1]][0]]"♞"[[[1 2][2 1][n 2][-2 1]][0 -2]]"♝"[t r]"♜"[u r]"♛"[(concat t u) r]}] (set(for [y r x r :let [[q r](get m (g b [y x]))][o p]q] (first(remove #{" "}(map #(g b [(+ y(* p %)) (+ x(* o %))])(map inc r)))))))"♔"))

;; 327 (defn is-check[b](.contains(let[g get-in r(range 8)n -1 t[[1 1][1 n][n 1][n n]]u[[1 0][n 0][0 1][0 n]]m {"♟"[[[1 1][n 1]][0 1]]"♞"[[[1 2][2 1][n 2][-2 1]][0 1 n]]"♝"[t r]"♜"[u r]"♛"[(into t u)r]}](for[y r x r :let[[q s](get m(g b[y x]))][o p]q](first(remove #{" "}(map #(g b[(+ y(* p %))(+ x(* o %))])(rest s))))))"♔"))

;; 352 (defn is-check[b](reduce #(or %1 %2)false(let [g get-in r(range 8) n -1 t[[1 1][1 n][n 1][n n]] u[[1 0][n 0][0 1][0 n]] m{"♟"[[[1 1][n 1]][0 1]]"♞"[[[1 2][2 1][n 2][-2 1]][0 1 n]]"♝"[t r]"♜"[u r]"♛"[(into t u) r]}](for[y r x r :let [[q s](get m(g b [y x]))][o p]q](="♔"(first(remove #{" "}(map #(g b[(+ y (* p %))(+ x (* o %))])(rest s)))))))))


;; (is-check (quote [[" " " " " " " " " " " " " " " "][" " " " " " " " " " " " " " " "][" " " " " " " " " " " " " " " "][" " " " " " " " " " " " " " " "][" " " " " " " " " " " " " " " "][" " " " "♔" " " " " "♜" " " " "][" " " " " " " " " " " " " " " "][" " " " " " " " " " " " " " " "]]))

;; 338 (defn is-check[b](let[g get-in r(range 8)n -1 t[[1 1][1 n][n 1][n n]]u[[1 0][n 0][0 1][0 n]]m {"♟"[[[1 1][n 1]][0 1]]"♞"[[[1 2][2 1][n 2][-2 1]][0 1 n]]"♝"[t r]"♜"[u r]"♛"[(into t u)r]}](.contains(for[y r x r :let[[q s](get m(g b[y x]))][o p]q](first(for[w(rest s):let[v(g b[(+ y(* p w))(+ x(* o w))])]:when(not= " " v)]v)))"♔")))

;; 325 (defn is-check[b](let[g get-in r(range 8)n -1 t[[1 1][1 n][n 1][n n]]u[[1 0][n 0][0 1][0 n]]m {"♟"[[[1 1][n 1]][0 1]]"♞"[[[1 2][2 1][n 2][2 n]][0 1 n]]"♝"[t r]"♜"[u r]"♛"[(into t u)r]}](.contains(for[y r x r :let[[q s](get m(g b[y x]))][o p]q](first(remove #{" "}(rest(map #(g b[(+ y(* p %))(+ x(* o %))])s)))))"♔")))

;; 317 (defn is-check[b](.contains (for[r[(range 8)]y r x r :let[g get-in n -1 t[[1 1][1 n][n 1][n n]]u[[1 0][n 0][0 1][0 n]][q s](get{"♟"[[[1 1][n 1]][0 1]]"♞"[[[1 2][2 1][n 2][2 n]][0 1 n]]"♝"[t r]"♜"[u r]"♛"[(into t u)r]}(g b[y x]))][o p]q](first(remove #{" "}(rest(map #(g b[(+ y(* p %))(+ x(* o %))])s)))))"♔"))

;; 306 (defn is-check[b](.contains(for[r[(range 8)]y r x r :let[n -1 t[[1 1][1 n][n 1][n n]]u[[1 0][n 0][0 1][0 n]][q[_ & s]]({"♟"[[[1 1][n 1]][0 1]]"♞"[[[1 2][2 1][n 2][2 n]][0 1 n]]"♝"[t r]"♜"[u r]"♛"[(into t u)r]}((b y)x))][o p]q](first(remove #{" "}(map #(get-in b[(+ y(* p %))(+ x(* o %))])s))))"♔"))

;; 305 (defn is-check[b](.contains(for[r[(range 8)]y r x r :let[n -1 t[[1 1][1 n][n 1][n n]]u[[1 0][n 0][0 1][0 n]][q[_ & s]]({"♟"[[[1 1][n 1]][0 1]]"♞"[[[1 2][2 1][n 2][2 n]][0 1 n]]"♝"[t r]"♜"[u r]"♛"[(into t u)r]}((b y)x))][o p]q](first(remove #{\ }(map #(get-in b[(+ y(* p %))(+ x(* o %))0])s))))\♔))

;; 306 (defn is-check[b](.contains(for[r[(range 8)]y r x r :let[n -1 t[[1 1][1 n][n 1][n n]]u[[1 0][n 0][0 1][0 n]][q[_ & s]]({"♟"[[[1 1][n 1]][0 1]]"♞"[[[1 2][2 1][n 2][2 n]][0 1 n]]"♝"[t r]"♜"[u r]"♛"[(into t u)r]}((b y)x))][o p]q](some #{\♟\♞\♝\♜\♛\♔}(map #(get-in b[(+ y(* p %))(+ x(* o %))0])s)))\♔))

(defn is-check [b]
  (.contains (for [r [(range 8)]
                   y r
                   x r
                   :let [n -1
                         t [[1 1] [1 n] [n 1] [n n]]
                         u [[1 0] [n 0] [0 1] [0 n]]
                         [q [_ & s]] ({"♟" [[[1 1] [n 1]] [0 1]]
                                       "♞" [[[1 2] [2 1] [n 2] [2 n]] [0 1 n]]
                                       "♝" [t r]
                                       "♜" [u r]
                                       "♛" [(into t u) r]}
                                      ((b y) x))]
                   [o p] q]
               (first (remove #{\ } (map #(get-in b [(+ y(* p %)) (+ x(* o %)) 0]) s)))
               ;; (some #{\♟\♞\♝\♜\♛\♔}(map #(get-in b [(+ y (* p %))(+ x (* o %))0])s))
               ;; (some #(if (#{\ } %) nil %) (map #(get-in b [(+ y (* p %))(+ x (* o %))0])s))
               )
             \♔))




;; Kata: Bernoulli Numbers
(defn bernoulli_number [n]
  (println "bernoulli_number" n)
  (if (and (odd? n) (> n 1))
    0
    (bernoulli n)))




;; Kata: RGB To Hex Conversion
(defn rgb [r g b]
  (->> [r g b]
       (map #(max % 0))
       (map #(min % 255))
       (apply (partial format "%02X%02X%02X"))))




;; Kata: Unique in Order
(defn unique-in-order [input]
  (reduce (fn [acc c]
            (if (= c (last acc))
              acc
              (conj acc c)))
          []
          input))




;; Kata: Sum consecutives
(defn sum-consecutives [a]
  (map #(apply + %) (partition-by identity a)))




;; Kata: Your Ride is Here
(defn ride [group comet]
  (let [comet-num (fn [s] (mod (apply * (map (comp #(- % 64) int) s)) 47))]
    (if (apply = (map comet-num [group comet])) "GO" "STAY")))




;; Kata: Routes in a Square Grid
(defn pascal-triangle-row [n]
  (reductions (fn [acc k] (* acc (/ (+ n 1 (- k)) k)))
              1
              (take n (iterate inc 1))))

(defn routes [n]
  (if (> n 0)
    (nth (pascal-triangle-row (* 2 n)) n)
    0))




;; When greatest is less than smallest
(defn greatest-common-divisor [a b]
  (let [[l g] (sort [a b])]
    (if (= 0 l)
      g
      (greatest-common-divisor l (mod g l)))))

(defn least-common-multiple [a b]
  (/ (* (abs a) (abs b)) (greatest-common-divisor a b)))

(defn greatest [x y n]
  (let [lcm (least-common-multiple [x y])]
    (if (>= lcm n)
      0
      (* lcm (quot n lcm)))))

(defn smallest [x y n]
  (let [lcm (least-common-multiple [x y])]
    (* lcm (inc (quot n lcm)))))




;; Consecutive k-Primes
(defn consec-kprimes [k xs]
  (apply + (map #(dec (count %)) (filter #(= k (first %)) (partition-by identity (map #(apply + (vals %)) (map prime-factorization xs)))))))




;; Gradient Interpolation
(defn gradient-fn [a b]
  (fn [x] (mapv #(int (+ %1 (* (/ x 100) (- %2 %1)))) a b)))




;; Bad Apples
(defn find-bad-apples [packages]
  (keep-indexed (fn [i p] (if (some #{0} p) [i p])) packages))

(defn bad-apples [input]
  (loop [apples (vec (remove #(apply = 0 %) input))]
    (let [bads (find-bad-apples apples)
          [i b1] (first bads)
          [j b2] (second bads)]
      (case (count bads)
        0 apples
        1 (recur (vec (apply conj (subvec apples 0 i) (subvec apples (inc i)))))
        (recur (vec (concat
                     (subvec apples 0 i)
                     [(vec (remove #{0} (into b1 b2)))]
                     (subvec apples (inc i) j)
                     (subvec apples (inc j)))))))))




;; Array.diff
(defn array-diff [a b]
  (remove (set b) a))




;; Function Composition
(defn compose [f g]
  (comp f g))




;; Josephus Survivor
(defn josephus-survivor [n k]
  (reduce (fn [prev n] (inc (mod (+ prev k -1) n)))
          1
          (take n (iterate inc 1))))




;; Numbers that are a power of their sum of digits
(defn power-sums [n]
  (filter #(= (first %) n)
           (keep-indexed (fn [i x] (let [ds (digits x)]
                                     (if (> (count ds) 1)
                                       (list (apply + ds) (inc i) x))))
                         (take-while #(<= % (* (bigint 100000000000) Long/MAX_VALUE)) (reductions *' n (repeat n))))))

(defn power-sum-dig-term [n]
  (last (nth (sort-by last (take 58 (keep identity (mapcat #(power-sums %) (iterate inc 2))))) (dec n))))




;; Emirps
(defn from-digits [digits]
  (reduce #(+ (* 10 %1) %2) digits))

(defn emirps-below [n]
  (->> (range 2 n)
       (filter prime?)
       (map (fn [n] (let [ds (digits n)] (list n (from-digits (reverse ds))))))
       (remove #(apply = %))
       (filter (comp prime? second))
       (map first)))

(defn find-emirp [n]
  (let [emirps (emirps-below n)]
    [(count emirps) (last emirps) (apply + emirps)]))




;; Transform to Prime
(defn minimum-number [numbers]
  (let [sum (apply + numbers)]
    (- (first (drop-while #(< % sum) (primes))) sum)))




;; The Poet and the Pendulum
(defn pendulum [numbers]
  (let [[n & ns] (sort numbers)
        [l r] (reduce (fn [[l r] [a b]] [(conj l b) (conj r a)])
                      [[] []]
                      (partition-all 2 ns))]
    (remove nil? (concat (reverse l) [n] r))))




;; Counting Duplicates
(defn duplicate-count [text]
  (count (filter #(> (second %) 1) (frequencies (string/lower-case text)))))




;; Simple Pig Latin
(defn pig-it [s]
  (clojure.string/join
   " "
   (map (fn [[c & cs :as word]]
          (if (re-matches #"[^A-Za-z]" word)
            (str word)
            (clojure.string/join (conj (vec cs) c \a \y))))
        (clojure.string/split s #" "))))




;; Stop gninnipS My sdroW!
(defn spin-words [string]
  (->> (string/split string #" ")
       (map #(if (>= (count %) 5) (string/join (reverse %)) %))
       (string/join " ")))




;; Triangle number check
(defn is-triangle-number [number]
  (= number
     (first (drop-while #(< % number)
                        (map #(/ (+ (* % %) %) 2) (iterate inc 1))))))




;; TwoSum
(defn twosum [numbers target]
  (first
   (filter
    (fn [[a b]] (= target (+ (nth numbers a) (nth numbers b))))
    (for [n (range (count numbers))
          m (range (count numbers))
          :when (not= n m)]
      [n m]))))




;; Vector Affinity
(defn vector-affinity [a b]
  (float
   (if (= a b)
     1
     (/ (->> (map list a b)
             (filter #(apply = %))
             (count))
        (max (count a) (count b))))))




;; When Sigma1 Function Has Equals Values For an Integer and Its Reversed One
(defn divisors [n]
  (filter #(= 0 (rem n %)) (range 1 (inc n))))

(defn sigma-1-no-memo [n]
  (apply + (divisors n)))

(def sigma-1 (memoize sigma-1-no-memo))

(defn sigma-1-equals-reversed? [n]
  (let [reverse-n (Long/parseLong (clojure.string/join (reverse (str n))))]
    (and (not= n reverse-n) (= (sigma-1 n) (sigma-1 reverse-n)))))

(defn equal-sigma1 [hmax]
  (apply + (filter sigma-1-equals-reversed? (range (inc hmax)))))




;; Sum of Digits / Digital Root
(defn digital-root [n]
  (if (< n 10)
    n
    (digital-root (apply + (digits n)))))




;; Human Readable Time
(defn human-readable [x]
  (let [hours (quot x 3600)
        mins (quot (- x (* 3600 hours)) 60)
        secs (- x (+ (* 3600 hours) (* 60 mins)))]
    (format "%02d:%02d:%02d" hours mins secs)))




;; Triangle Type
;; cosine rule: cos C = (a^2 + b^2 - c) / 2ab
(defn triangle-type [a b c]
  (let [[A B C] (map (fn [[a b c]] (if (some #(= 0 %) [a b c]) Double/NaN
                                       (Math/acos (/ (+ (* a a) (* b b) (- (* c c))) (* 2 a b)))))
                     [[b c a] [c a b] [a b c]])]
    (cond (or (> (Math/abs (- Math/PI (+ A B C))) 0.000001)
              (some #(<= % 0.0) [A B C])
              (some #(Double/isNaN %) [A B C])) 0
          (every? #(< % (/ Math/PI 2)) [A B C]) 1
          (some #(= % (/ Math/PI 2)) [A B C]) 2
          :else 3)))




;; Equal Sides of an Array
(defn find-even-index [arr]
  ((fnil identity -1)
   (some #(if (= (apply + (take % arr)) (apply + (drop (inc %) arr))) %)
         (range (count arr)))))




;; Decompose a Number
(defn decompose-inner [n base]
  (if (< n (* base base))
    [n]
    (let [xs (take-while #(<= % n) (reductions * base (repeat base)))]
      (concat [(count xs)]
              (decompose-inner (- n (last xs)) (inc base))))))

(defn decompose [n]
  (let [result (decompose-inner n 2)]
    [(vec (drop-last result)) (last result)]))




;; Playing with Digits
(defn dig-pow [n p]
  (let [pow-sum (apply + (map #(long (Math/pow %1 %2))
                              (map #(Long/parseLong (str %)) (str n))
                              (iterate inc p)))]
    (if (= 0 (mod pow-sum n))
      (quot pow-sum n)
      -1)))




;; Factorial Decomposition
(defn decomp [n]
  (clojure.string/join " * "
   (map (fn [[n p]] (if (> p 1) (format "%d^%d" n p) (str n)))
        (sort
         (apply merge-with +
                (for [i (range 2 (inc n))]
                  (prime-factorization i)))))))




;; Basics 08: Find next higher number with same Bits (1's)
(defn next-higher [n]
  (let [count-bits (fn [n] (count (filter #{\1} (Integer/toString n 2))))
        c (count-bits n)]
    (first (filter #(= c (count-bits %)) (rest (iterate inc n))))))
