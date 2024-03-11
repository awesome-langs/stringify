(require '[clojure.string :as string])  

(defrecord PolyEvalType [type-str type-name value-type key-type])

(defn new-poly-eval-type__ [type-str type-name value-type key-type]
  (PolyEvalType. type-str type-name value-type key-type))

(declare s-to-type__)
(declare val-to-s__)
(declare stringify__)

(defn s-to-type__ [type-str]
    (if (not (re-find #"<" type-str))
        (new-poly-eval-type__ type-str type-str nil nil)
        (let [idx (.indexOf type-str "<")
            type-name (subs type-str 0 idx)
            other-str (subs type-str (inc idx) (- (count type-str) 1))]
        (if (not (re-find #"," other-str))
            (let [value-type (s-to-type__ other-str)]
            (new-poly-eval-type__ type-str type-name value-type nil))
            (let [idx (.indexOf other-str ",")
                key-type (s-to-type__ (subs other-str 0 idx))
                value-type (s-to-type__ (subs other-str (inc idx)))]
            (new-poly-eval-type__ type-str type-name value-type key-type))))))

(defn escape-string__ [s]
    (let [new-s (map (fn [c]
                        (cond
                            (= c \\) "\\\\"
                            (= c \") "\\\""
                            (= c \newline) "\\n"
                            (= c \tab) "\\t"
                            (= c \return) "\\r"
                            :else (str c))) s)]
        (apply str new-s)))

(defn by-bool__ [value]
    (if value "true" "false"))

(defn by-int__ [value]
    (str (int value)))

(defn by-double__ [value]
    (let [v (double value)]
        (let [vs (atom (format "%.6f" v))]
            (while (string/ends-with? @vs "0")
                (reset! vs (subs @vs 0 (- (count @vs) 1))))
            (if (string/ends-with? @vs ".")
                (reset! vs (str @vs "0"))
                (if (= @vs "-0.0")
                    (reset! vs "0.0")))
            @vs)))

(defn by-string__ [value]
    (str "\"" (escape-string__ value) "\""))

(defn by-list__ [value ty]
    (let [v-strs (map (fn [v] (val-to-s__ v (.-value-type ty))) value)]
        (str "[" (apply str (interpose ", " v-strs)) "]")))

(defn by-ulist__ [value ty]
    (let [v-strs (map (fn [v] (val-to-s__ v (.-value-type ty))) value)]
        (str "[" (apply str (interpose ", " (sort v-strs))) "]")))
    
(defn by-dict__ [value ty]
    (let [v-strs (map (fn [[k v]] (str (val-to-s__ k (.-key-type ty)) "=>" (val-to-s__ v (.-value-type ty)))) value)]
        (str "{" (apply str (interpose ", " (sort v-strs))) "}")))

(defn by-option__ [value ty]
    (if (nil? value)
        "null"
        (val-to-s__ value (.-value-type ty))))

(defn val-to-s__ [value ty]
    (let [type-name (.-type-name ty)]
    (cond
        (= type-name "bool") (if (not (boolean? value))
                                (throw (IllegalArgumentException. "Type mismatch"))
                                (by-bool__ value))
        (= type-name "int") (if (not (int? value))
                                (throw (IllegalArgumentException. "Type mismatch"))
                                (by-int__ value))
        (= type-name "double") (if (not (float? value))
                                    (throw (IllegalArgumentException. "Type mismatch"))
                                    (by-double__ value))
        (= type-name "str") (if (not (string? value))
                                (throw (IllegalArgumentException. "Type mismatch"))
                                (by-string__ value))
        (= type-name "list") (if (not (vector? value))
                                (throw (IllegalArgumentException. "Type mismatch"))
                                (by-list__ value ty))
        (= type-name "ulist") (if (not (vector? value))
                                (throw (IllegalArgumentException. "Type mismatch"))
                                (by-ulist__ value ty))
        (= type-name "dict") (if (not (map? value))
                                (throw (IllegalArgumentException. "Type mismatch"))
                                (by-dict__ value ty))
        (= type-name "option") (by-option__ value ty)
        :else (throw (IllegalArgumentException. (str "Unknown type " type-name))))))
    
(defn stringify__ [value type-str]
    (str (val-to-s__ value (s-to-type__ type-str)) ":" type-str))

(def tfs (str (stringify__ true "bool") "\n"
            (stringify__ 3 "int") "\n"
            (stringify__ 3.141592653 "double") "\n"
            (stringify__ 3.0 "double") "\n"
            (stringify__ "Hello, World!" "str") "\n"
            (stringify__ "!@#$%^&*()\\\"\n\t" "str") "\n"
            (stringify__ [1 2 3] "list<int>") "\n"
            (stringify__ [true false true] "list<bool>") "\n"
            (stringify__ [3 2 1] "ulist<int>") "\n"
            (stringify__ {1 "one" 2 "two"} "dict<int,str>") "\n"
            (stringify__ {"one" [1 2 3] "two" [4 5 6]} "dict<str,list<int>>") "\n"
            (stringify__ nil "option<int>") "\n"
            (stringify__ 3 "option<int>") "\n"))
(spit "stringify.out" tfs)