(require '[clojure.string :as string])  

(defrecord PolyEvalType [type-str type-name value-type key-type])

(defn __new-poly-eval-type [type-str type-name value-type key-type]
  (PolyEvalType. type-str type-name value-type key-type))

(declare __s-to-type)
(declare __val-to-s)
(declare __stringify)

(defn __s-to-type [type-str]
    (if (not (re-find #"<" type-str))
        (__new-poly-eval-type type-str type-str nil nil)
        (let [idx (.indexOf type-str "<")
            type-name (subs type-str 0 idx)
            other-str (subs type-str (inc idx) (- (count type-str) 1))]
        (if (not (re-find #"," other-str))
            (let [value-type (__s-to-type other-str)]
            (__new-poly-eval-type type-str type-name value-type nil))
            (let [idx (.indexOf other-str ",")
                key-type (__s-to-type (subs other-str 0 idx))
                value-type (__s-to-type (subs other-str (inc idx)))]
            (__new-poly-eval-type type-str type-name value-type key-type))))))

(defn __escape-string [s]
    (let [new-s (map (fn [c]
                        (cond
                            (= c \\) "\\\\"
                            (= c \") "\\\""
                            (= c \newline) "\\n"
                            (= c \tab) "\\t"
                            (= c \return) "\\r"
                            :else (str c))) s)]
        (apply str new-s)))

(defn __by-bool [value]
    (if value "true" "false"))

(defn __by-int [value]
    (str (int value)))

(defn __by-double [value]
    (let [v (double value)]
        (let [vs (atom (format "%.6f" v))]
            (while (string/ends-with? @vs "0")
                (reset! vs (subs @vs 0 (- (count @vs) 1))))
            (if (string/ends-with? @vs ".")
                (reset! vs (str @vs "0"))
                (if (= @vs "-0.0")
                    (reset! vs "0.0")))
            @vs)))

(defn __by-string [value]
    (str "\"" (__escape-string value) "\""))

(defn __by-list [value ty]
    (let [v-strs (map (fn [v] (__val-to-s v (.-value-type ty))) value)]
        (str "[" (apply str (interpose ", " v-strs)) "]")))

(defn __by-ulist [value ty]
    (let [v-strs (map (fn [v] (__val-to-s v (.-value-type ty))) value)]
        (str "[" (apply str (interpose ", " (sort v-strs))) "]")))
    
(defn __by-dict [value ty]
    (let [v-strs (map (fn [[k v]] (str (__val-to-s k (.-key-type ty)) "=>" (__val-to-s v (.-value-type ty)))) value)]
        (str "{" (apply str (interpose ", " (sort v-strs))) "}")))

(defn __by-option [value ty]
    (if (nil? value)
        "null"
        (__val-to-s value (.-value-type ty))))

(defn __val-to-s [value ty]
    (let [type-name (.-type-name ty)]
    (cond
        (= type-name "bool") (if (not (boolean? value))
                                (throw (IllegalArgumentException. "Type mismatch"))
                                (__by-bool value))
        (= type-name "int") (if (not (int? value))
                                (throw (IllegalArgumentException. "Type mismatch"))
                                (__by-int value))
        (= type-name "double") (if (not (float? value))
                                    (throw (IllegalArgumentException. "Type mismatch"))
                                    (__by-double value))
        (= type-name "str") (if (not (string? value))
                                (throw (IllegalArgumentException. "Type mismatch"))
                                (__by-string value))
        (= type-name "list") (if (not (vector? value))
                                (throw (IllegalArgumentException. "Type mismatch"))
                                (__by-list value ty))
        (= type-name "ulist") (if (not (vector? value))
                                (throw (IllegalArgumentException. "Type mismatch"))
                                (__by-ulist value ty))
        (= type-name "dict") (if (not (map? value))
                                (throw (IllegalArgumentException. "Type mismatch"))
                                (__by-dict value ty))
        (= type-name "option") (__by-option value ty)
        :else (throw (IllegalArgumentException. (str "Unknown type " type-name))))))
    
(defn __stringify [value type-str]
    (str (__val-to-s value (__s-to-type type-str)) ":" type-str))

(def tfs (str (__stringify true "bool") "\n"
            (__stringify 3 "int") "\n"
            (__stringify 3.141592653 "double") "\n"
            (__stringify 3.0 "double") "\n"
            (__stringify "Hello, World!" "str") "\n"
            (__stringify "!@#$%^&*()\\\"\n\t" "str") "\n"
            (__stringify [1 2 3] "list<int>") "\n"
            (__stringify [true false true] "list<bool>") "\n"
            (__stringify [3 2 1] "ulist<int>") "\n"
            (__stringify {1 "one" 2 "two"} "dict<int,str>") "\n"
            (__stringify {"one" [1 2 3] "two" [4 5 6]} "dict<str,list<int>>") "\n"
            (__stringify nil "option<int>") "\n"
            (__stringify 3 "option<int>") "\n"))
(spit "stringify.out" tfs)