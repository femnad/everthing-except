(ns everything-except.core
  (:require [clojure.string :as s])
  (:gen-class))

(def keycode-prefix "KC_")
(def keycode-prefix-length (count keycode-prefix))
(def line-width 7)
(def vertical-separator "|")
(def line-base-column-char "-")
(def cell-seperator "+")
(def modifier-function-regex #"[A-Z]+\([A-Z]+_([A-Z]+)\)")

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

(defn- get-lines-from-keymap [keymap]
  (->> (s/split keymap #"\n")
        (map get-keys-from-line)
        (map get-line)))

(defn- delimit-line [line]
  (concat
   (conj (interpose vertical-separator line) vertical-separator)
   (list vertical-separator)))

(defn- get-line-base [num-items]
  (->> (repeat-char line-base-column-char line-width)
       (repeat num-items ,)
       (interpose cell-seperator ,)
       (apply str)))
