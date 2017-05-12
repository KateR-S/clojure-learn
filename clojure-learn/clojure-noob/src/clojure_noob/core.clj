(ns clojure-noob.core
  (:gen-class :main true)
  (:require [clj-http.client :as client]
            [cheshire.core :refer :all]
            [clojure.math.numeric-tower :as math]
            [clojure-noob.api :as api]))

(def card-values
  { "0" "10"
   "J" "11"
   "Q" "12"
   "K" "13"
   "A" "14"})

(def player-values
  { 0 :p1
    1 :p2})

(def reason-codes
  {0 "GOT BORED AND GAVE UP"
   1 "PLAYER HAS NO CARDS LEFT!"
   2 "NO SNAPS EXIST!"})

(def value-players
  (clojure.set/map-invert player-values))

(defn handle-deck
  "Creating a deck of cards and getting back some deck stuff"
  [action type & items]
  (decode (get (client/get (get (apply api/get-urls items) type) {:insecure? true}) :body)))

(defn just-hands
  "return just the hands from the round details"
  [round-details]
  (into [] (vals (select-keys round-details [:p1 :p2]))))

(defn- new-deck-id
  []
  (let [get-return (handle-deck :get :new)]
    (if (get-return "success")
      (get-return "deck_id")
      nil)))
; TODO: Handle nil return value from this function

(defn- draw-half
  [deck_id]
  (mapv #(% "code") ((handle-deck :get :draw deck_id "26") "cards")))

(defn- take-value
  [card-vector]
  (mapv #(subs % 0 1) card-vector))

(defn- make-cards-numeric
  [card-vector]
  (->>
      card-vector
      take-value
      (replace card-values)
      (mapv read-string)))

(defn- deal-half
  "Deal half the deck to a player"
  [deck_id player]
  (->>
      deck_id
      draw-half
      make-cards-numeric
      (assoc {} player)))

(defn create-piles
  "Cut the deck in half and assign to players"
  [deck_id]
  {:pre [(not (nil? deck_id))]}
  (into {} (map #(deal-half deck_id %) [:p1 :p2])))

(defn initialise-round
  "Create round structure and add initial piles to that"
  [piles]
  (assoc piles :snap_location -1 :exit 0 :round 0 :score [0 0] :winner -1 :cards [(piles :p1) (piles :p2)]))

; TODO: add-player-vector is just an intermediate object to get rid of the p1 p2 map thing
(defn initialse-deck
  "Get cards, split into two decks, and initialise round details"
  []
  (-> (new-deck-id)
      (create-piles)
      (initialise-round)))

(defn increment-round
  "Increment the round number"
  [round-details]
  (update-in round-details [:round] inc))

(defn calc-winner [score]
  "Currently winner calculation is just random 50-50"
  (rand-int 2))

(defn define-winner
  "Define who is the winner of the round"
  [round-details]
  (let [winner (calc-winner (round-details :score))]
    (assoc round-details
      :score (update-in (round-details :score) [winner] inc)
      :winner-val winner
      :winner (player-values winner)
      :loser (apply keyword (vals (dissoc player-values winner))))))

(defn replicated-cards [thing times]
  (reduce into (replicate times thing)))

(defn looped-hands
  "Make the hands the same length if they're not already"
  [hands]
  (let [hand_size (map count hands)
        new_hand_size (apply math/lcm hand_size)
        repeat_values (map #(/ new_hand_size %) hand_size)
        flat_map (map replicated-cards hands repeat_values)
; writing out to :p1 and :p2 here
        output (zipmap '(:p1 :p2) flat_map)]
    (assoc output :hand_size hand_size)))

(defn settle-hand-size
  "Return hands of the same size"
  [round-details]
  (merge round-details (looped-hands (just-hands round-details))))

(defn matched-cards
  "where the snap location is"
  [round-details]
  (let [hands (just-hands round-details)]
    (.indexOf (apply map = hands) true)))

(defn find-snap-location
  "Find out if there is a snap"
  [round-details]
  (let [looped_hands (settle-hand-size round-details)
        snap_location (matched-cards looped_hands)]
    (assoc round-details :snap_location snap_location :snap_locations (mapv #(rem snap_location %) (looped_hands :hand_size)))))

(defn check-for-snap-exit
  "If snap is equal to -1 then exit"
  [round-details]
  (if (= -1 (round-details :snap_location))
    (assoc round-details :exit 1 :reason-code 2)
    round-details))

(defn round-cards
  "stuff"
  [cards locations]
  (subvec cards 0 (+ 1 locations)))

(defn set-hand-stats
  "Set some stats about the hands"
  [round-details]
  (let [hand_sizes (map count (just-hands round-details))
        next_hand_sizes (map - hand_sizes (round-details :snap_locations))]
    (assoc round-details
      :hand_sizes hand_sizes
      :next_hand_sizes next_hand_sizes)))

(defn reassign-cards
  "stuff"
  [round-details]
  (let [cards-values (just-hands round-details)
        snap_location (round-details :snap_locations)
        hand_sizes (map count cards-values)
        will_be_empty (mapv = (map inc snap_location) hand_sizes)]
    (if (true? (some true? will_be_empty))
      round-details
      (let [
            round_cards (into [] (flatten (mapv round-cards cards-values snap_location)))
            winner (round-details :winner)
            winnerval (round-details :winner-val)
            loser (round-details :loser)
            win_hand (apply vector (flatten (conj (subvec (nth cards-values winnerval) (+ 1 (nth snap_location winnerval))) round_cards)))
            lose_hand (subvec (nth cards-values (value-players loser)) (+ 1 (nth snap_location (value-players loser))))]
        (println "In round details")
        (println round_cards)
        (println (str "WINNER IS " (value-players winner)))
        (println (str "cards are " cards-values " with round cards " round_cards))
        (assoc round-details winner win_hand loser lose_hand)))))

(defn check-for-champ
  "stuff"
  [round-details]
  (if (>= 1 (apply min (map count (just-hands round-details))))
    (assoc round-details :exit 1 :reason-code 1)
    round-details))

(defn check-round
  "Check the round number; exit if 20 have been played with no result"
  [round-details]
  (if (= 20 (round-details :round))
    (assoc round-details :exit 1 :reason-code 0)
    round-details))

(defn play-round
  "play a round"
  [round-details]
  (let [rd (->  round-details
                set-hand-stats
                reassign-cards
                check-for-champ
                check-round)]
    rd))

(defn -main []
  (loop [round-details (initialse-deck)]
    (if (= 0 (round-details :exit))
      (recur
        (as-> round-details rd
          (find-snap-location rd)
          (check-for-snap-exit rd)
          (increment-round rd)
          (define-winner rd)
          (if (= 0 (get rd :exit))
            (play-round rd)
            rd)))
      (println round-details)))
  (println "end"))

(-main)
