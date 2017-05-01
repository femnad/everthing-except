(ns everything-except.core
  (:require [clojure.string :as s])
  (:gen-class))

(def keycode-prefix "KC_")
(def keycode-prefix-length (count keycode-prefix))
(def line-width 7)
(def separator "|")

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

(defn- get-short-form [key]
  (if (s/starts-with? key keycode-prefix)
    (if (is-simple-key? key)
      (get-last-letter key)
      (get-keycodeless-suffix key))))

(defn- divide-and-round-up [dividend divisor]
  (inc (Math/floorDiv dividend divisor)))

(defn- divide-and-round-down [dividend divisor]
  (Math/floorDiv dividend divisor))

(defn- print-char-x-times [char x]
  (when-not (= 0 x)
    (do
      (print char)
      (print-char-x-times char (dec x)))))

(defn- draw-key [key]
  (let [blank-cols (- line-width (count key))
        left-margin (divide-and-round-down blank-cols 2)
        right-margin (divide-and-round-up blank-cols 2)]
    (print separator)
    (print-char-x-times " " left-margin)
    (print key)
    (print-char-x-times " " right-margin)))

(defn- draw-line [keys]
  (let [short-form-keys (map get-short-form keys)]
    (loop [ks short-form-keys]
      (if (empty? ks)
        (println)
        (do
          (draw-key (first ks))
          (recur (rest ks)))))))

(defn generate-olkb-keymap-map [keymap]
  (let [lines (s/split keymap #"\n")]
    (->> lines
        (map get-keys-from-line)
        (map draw-line))))
