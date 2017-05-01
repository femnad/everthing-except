(ns everything-except.core
  (:require [clojure.string :as s])
  (:gen-class))

(def keycode-prefix "KC_")
(def keycode-prefix-length (count keycode-prefix))
(def line-width 7)
(def separator "|")
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

(defn- is-simple-key? [key]
  (= (count key) (inc keycode-prefix-length)))

(defn- get-keycodeless-suffix [key]
  (subs key keycode-prefix-length))

(defn- get-special-key [key]
  (let [modifier-function (re-find modifier-function-regex key)]
    (if (nil? modifier-function)
      (get-keycodeless-suffix key)
      (last modifier-function))))

(defn- get-short-form [key]
  (if (s/starts-with? key keycode-prefix)
    (if (is-simple-key? key)
      (get-last-letter key)
      key)
    (get-special-key key)))

(defn- divide-and-round-up [dividend divisor]
  (inc (Math/floorDiv dividend divisor)))

(defn- divide-and-round-down [dividend divisor]
  (Math/floorDiv dividend divisor))

(defn- print-char-x-times [char x]
  (when-not (= 0 x)
    (do
      (print char)
      (print-char-x-times char (dec x)))))

(defn- get-margins [empty-cols]
  (let [right-margin (divide-and-round-down empty-cols 2)
        increment (if (odd? empty-cols) 1 0)]
    {:left (+ increment right-margin)
     :right right-margin}))

(defn- draw-key [key]
  (let [blank-cols (- line-width (count key))
        margin (get-margins blank-cols)]
    (print separator)
    (print-char-x-times " " (:left margin))
    (print key)
    (print-char-x-times " " (:right margin))))

(defn- repeat-char [a-char n]
  (apply str (repeat n a-char)))

(defn- print-cell-base []
  (print cell-seperator)
  (print (repeat-char "-" line-width)))

(defn- print-base-line [num-cells]
  (if (= 0 num-cells)
    (println)
    (do
      (print-cell-base)
      (print-base-line (dec num-cells)))))

(defn- draw-line [keys]
  (let [short-form-keys (map get-short-form keys)]
    (loop [ks short-form-keys]
      (if (empty? ks)
        (do
          (println)
          (print-base-line (count short-form-keys)))
        (do
          (draw-key (first ks))
          (recur (rest ks)))))))

(defn generate-olkb-keymap-map [keymap]
  (let [lines (s/split keymap #"\n")]
    (->> lines
        (map get-keys-from-line)
        (map draw-line))))
