(ns dot.cljnect4wss
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as g]
   [clojure.spec.test.alpha :as stest]
   [taoensso.timbre :as log])
  (:gen-class))

(s/def ::x (s/and int? #(<= 0 %)))
(s/def ::y (s/and int? #(<= 0 %)))
(s/def ::occupant (s/nilable #{:R :B}))
(s/def ::space (s/keys :req-un [::x ::y ::occupant]))
(s/def ::spaces (s/coll-of ::space))
(s/def ::width pos-int?)
(s/def ::height pos-int?)
(s/def ::dimensions (s/keys :req-un [::width ::height]))

(s/def ::direction #{:NORTH :NORTHEAST :EAST :SOUTHEAST
                     :SOUTH :SOUTHWEST :WEST :NORTHWEST})

(defn generate-board
  "Generates an empty connect 4 board with the given dimensions."
  [w h]
  (letfn [(from-zero-up-to [x] (range 0 x))]
    {:dimensions {:width w :height h}
     :spaces
     (for [x (from-zero-up-to w)
           y (from-zero-up-to h)]
       {:x x :y y :occupant nil})}))

(s/def ::board (s/with-gen
                 (s/keys :req-un [::dimensions ::spaces])
                 #(s/gen
                    #{(generate-board
                        (inc (rand-int 16))
                        (inc (rand-int 16)))})))

(comment
  (generate-board 2 2)
  (generate-board 7 6))

(defn space? [m] (s/valid? ::space m))

(defn space-at [board x y]
  (letfn [(our-space? [space] (and (-> space :x (= x))
                                   (-> space :y (= y))
                                   space))]
    (some our-space? (:spaces board))))

(s/fdef neighbours-of
  :args (s/cat :b ::board
               :s ::space)
  :ret (s/map-of ::direction (s/nilable ::space))
  ;; confirm we don't violate the boundaries of the board
  :fn #(let [board (-> % :args :b)
             space (-> % :args :s)
             {:keys [x y]} space
             {:keys [SOUTHWEST SOUTH SOUTHEAST EAST
                     NORTHEAST NORTH NORTHWEST WEST] :as dirs} (:ret %)
             all-spaces? (fn [] (s/valid? ::spaces (vals dirs)))]
         (and
           (if (= x 0) (= nil SOUTHWEST WEST NORTHWEST) true)
           (if (= y 0) (= nil SOUTHWEST SOUTH SOUTHEAST) true)
           (if (= x (-> board :dimensions :width dec))
             (= nil EAST NORTHEAST SOUTHEAST)
             true)
           (if (= y (-> board :dimensions :height dec))
             (= nil NORTHEAST NORTH NORTHWEST)
             true)
           (if (and (< 0 y (-> board :dimensions :height dec)) ; if we're not on an edge,
                    (< 0 x (-> board :dimensions :width dec))) ; all neighbours should be spaces
             (all-spaces?)
             (not (all-spaces?))))))
(defn neighbours-of
  "Return the neighbouring spaces for each cardinal direction
  N,NE,E,SE,S,SW,NW. A space on an edge of the board will yield some cardinal
  directions with the value `nil`.
  There will be no neighbours for a space which doesn't exist."
  [board {:keys [x y] :as _space}]
  (letfn [(point [x y] (space-at board x y))]
    {:NORTH     (point x       (inc y))
     :NORTHEAST (point (inc x) (inc y))
     :NORTHWEST (point (dec x) (inc y))
     :SOUTH     (point x       (dec y))
     :SOUTHEAST (point (inc x) (dec y))
     :SOUTHWEST (point (dec x) (dec y))
     :EAST      (point (inc x) y)
     :WEST      (point (dec x) y)}))

(comment
  (g/generate (s/gen ::board))
  (g/generate (s/gen ::space))
  (s/exercise-fn `neighbours-of)
  (stest/check `neighbours-of)
  (def ex-board (generate-board 7 6))
  (def ex-space (space-at ex-board 4 4))
  (neighbours-of ex-board ex-space))

(defn get-horizontal-neighbours [some-board some-space] nil)

(defn greet
  "Callable entry point to the application."
  [data]
  (println (str "Hello, " (or (:name data) "World") "!")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (greet {:name (first args)}))
