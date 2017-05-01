(ns everything-except.core
  (:require [clojure.string :as s])
  (:gen-class))

(def keycode-prefix "KC_")
(def keycode-prefix-length (count keycode-prefix))
(def line-width 7)
(def vertical-separator "|")
(def cell-seperator "+")
(def modifier-function-regex #"[A-Z]+\([A-Z]+_([A-Z]+)\)")

(defn- get-last-letter [a-string]
  (let [length (count a-string)]
        (subs a-string (dec length) length)))

(defn- get-keys-from-line [line]
  (let [raw-tokens (s/split line #"\{|\}|,")]
    (->>
     raw-tokens
     (map s/trim)
     (filter not-empty))))

(defn- get-suffix [key]
  (subs key keycode-prefix-length))

(defn- parse-modifier-function [key]
  (let [modifier-function (re-find modifier-function-regex key)]
    (if (nil? modifier-function)
      key
      (last modifier-function))))

(defn- get-short-form [key]
  (if (s/starts-with? key keycode-prefix)
    (get-suffix key)
    (parse-modifier-function key)))

(defn- divide-and-round-up [dividend divisor]
  (inc (Math/floorDiv dividend divisor)))

(defn- divide-and-round-down [dividend divisor]
  (Math/floorDiv dividend divisor))

(defn- get-margins [empty-cols]
  (let [right-margin (divide-and-round-down empty-cols 2)
        increment (if (odd? empty-cols) 1 0)]
    {:left (+ increment right-margin)
     :right right-margin}))

(defn- repeat-char [a-char n]
  (apply str (repeat n a-char)))

(defn- get-key [key]
  (let [blank-cols (- line-width (count key))
        margin (get-margins blank-cols)]
    (apply str (repeat-char " " (:left margin))
          key
          (repeat-char " " (:right margin)))))

(defn- print-cell-base []
  (print cell-seperator)
  (print (repeat-char "-" line-width)))

(defn- print-base-line [num-cells]
  (if (= 0 num-cells)
    (println)
    (do
      (print-cell-base)
      (print-base-line (dec num-cells)))))

(defn- get-line [keys]
  (let [short-form-keys (map get-short-form keys)]
    (loop [ks short-form-keys line-acc []]
      (if (empty? ks)
        line-acc
        (let [head (first ks)
              tail (rest ks)]
          (recur tail
                 (conj line-acc
                       (get-key head))))))))

(defn generate-olkb-keymap-map [keymap]
  (let [lines (s/split keymap #"\n")]
    (->> lines
        (map get-keys-from-line)
        (map get-line))))
