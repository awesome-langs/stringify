(defn my-string-to-int [s] 
    (parse-long s))

(defn my-string-to-double [s]
    (parse-double s))

(defn my-int-to-string [i]
    (str i))

(defn my-double-to-string [d]
    (format "%.6f" d))

(defn my-bool-to-string [b]
    (if b "true" "false"))

(defn my-int-to-nullable [i]
    (if (> i 0) i
        (if (< i 0) (- i)
            nil)))

(defn my-nullable-to-int [i]
    (if (nil? i) -1 i))

(defn my-list-sorted [lst]
    (sort lst))

(defn my-list-sorted-by-length [lst]
    (sort-by count lst))

(defn my-list-filter [lst]
    (filter #(= (mod % 3) 0) lst))

(defn my-list-map [lst]
    (map #(* % %) lst))

(defn my-list-reduce [lst]
    (reduce #(+ (* %1 10) %2) 0 lst))

(defn my-list-operations [lst]
    (->> lst
        (filter (fn [x] (= (mod x 3) 0)))
        (map (fn [x] (* x x)))
        (reduce (fn [acc x] (+ (* acc 10) x)) 0)))

(defn my-list-to-dict [lst]
    (zipmap lst (map #(* % %) lst)))

(defn my-dict-to-list [dict]
    (map (fn [[k v]] (+ k v)) (sort dict)))

(defn my-print-string [s]
    (println s))

(defn my-print-string-list [lst]
    (doseq [x lst]
        (print (str x " ")))
    (println))

(defn my-print-int-list [lst]
    (my-print-string-list (map my-int-to-string lst)))

(defn my-print-dict [dict]
    (doseq [[k v] dict]
        (print (str (my-int-to-string k) "->" (my-int-to-string v) " ")))
    (println))

(my-print-string "Hello, World!")
(my-print-string (my-int-to-string (my-string-to-int "123")))
(my-print-string (my-double-to-string (my-string-to-double "123.456")))
(my-print-string (my-bool-to-string false))
(my-print-string (my-int-to-string (my-nullable-to-int (my-int-to-nullable 18))))
(my-print-string (my-int-to-string (my-nullable-to-int (my-int-to-nullable -15))))
(my-print-string (my-int-to-string (my-nullable-to-int (my-int-to-nullable 0))))
(my-print-string-list (my-list-sorted ["e" "dddd" "ccccc" "bb" "aaa"]))
(my-print-string-list (my-list-sorted-by-length ["e" "dddd" "ccccc" "bb" "aaa"]))
(my-print-string (my-int-to-string (my-list-reduce (my-list-map (my-list-filter [3 12 5 8 9 15 7 17 21 11])))))
(my-print-string (my-int-to-string (my-list-operations [3 12 5 8 9 15 7 17 21 11])))
(my-print-dict (my-list-to-dict [3 1 4 2 5 9 8 6 7 0]))
(my-print-int-list (my-dict-to-list {3 9, 1 1, 4 16, 2 4, 5 25, 9 81, 8 64, 6 36, 7 49, 0 0}))