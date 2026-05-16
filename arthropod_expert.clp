;; Freshwater Arthropod Identification

(clear)

(deftemplate answer
  (slot question)
  (slot value))

(deftemplate state
  (slot current-question))

(deftemplate user-input
  (slot value)
  (slot valid))

(deffacts initial
  (state (current-question start))
  (user-input (valid no)))

;; ========== BANNER ==========
(defrule show-banner
  ?s <- (state (current-question start))
  =>
  (printout t crlf "=== Freshwater Arthropod Identification ===" crlf)
  (printout t "Answer questions to identify freshwater arthropods" crlf crlf)
  (assert (answer (question banner) (value shown)))
  (modify ?s (current-question Q1)))

;; ========== Q1: >6 LEGS? ==========
(defrule Q1_prompt
  (state (current-question Q1))
  (not (answer (question Q1)))
  (user-input (valid no))
  =>
  (printout t "1. Does the arthropod have more than 6 legs? (yes/no): ")
  (bind ?response (lowcase (read)))
  (assert (user-input (value ?response) (valid yes))))

(defrule Q1_valid_yes
  ?s <- (state (current-question Q1))
  ?i <- (user-input (value yes) (valid yes))
  (not (answer (question Q1)))
  =>
  (assert (answer (question Q1) (value yes)))
  (modify ?s (current-question Q1A))
  (retract ?i)
  (assert (user-input (valid no))))

(defrule Q1_valid_no
  ?s <- (state (current-question Q1))
  ?i <- (user-input (value no) (valid yes))
  (not (answer (question Q1)))
  =>
  (assert (answer (question Q1) (value no)))
  (modify ?s (current-question Q1B))
  (retract ?i)
  (assert (user-input (valid no))))

(defrule Q1_invalid
  ?i <- (user-input (value ?v&:(not (or (eq ?v yes) (eq ?v no)))) (valid yes))
  (state (current-question Q1))
  =>
  (printout t "Please answer yes or no." crlf)
  (retract ?i)
  (assert (user-input (valid no))))

;; ========== Q1A: 8 LEGS? ==========
(defrule Q1A_prompt
  (state (current-question Q1A))
  (answer (question Q1) (value yes))
  (not (answer (question Q1A)))
  (user-input (valid no))
  =>
  (printout t "2. Does it have 8 legs (4 pairs)? (yes/no): ")
  (bind ?response (lowcase (read)))
  (assert (user-input (value ?response) (valid yes))))

(defrule Q1A_valid_yes
  ?s <- (state (current-question Q1A))
  ?i <- (user-input (value yes) (valid yes))
  (answer (question Q1) (value yes))
  (not (answer (question Q1A)))
  =>
  (assert (answer (question Q1A) (value yes)))
  (modify ?s (current-question conclusion))
  (retract ?i)
  (assert (user-input (valid no))))

(defrule Q1A_valid_no
  ?s <- (state (current-question Q1A))
  ?i <- (user-input (value no) (valid yes))
  (answer (question Q1) (value yes))
  (not (answer (question Q1A)))
  =>
  (assert (answer (question Q1A) (value no)))
  (modify ?s (current-question Q1A2))
  (retract ?i)
  (assert (user-input (valid no))))

(defrule Q1A_invalid
  ?i <- (user-input (value ?v&:(not (or (eq ?v yes) (eq ?v no)))) (valid yes))
  (state (current-question Q1A))
  =>
  (printout t "Please answer yes or no." crlf)
  (retract ?i)
  (assert (user-input (valid no))))

;; ========== Q1A2: BIG CLAWS? ==========
(defrule Q1A2_prompt
  (state (current-question Q1A2))
  (answer (question Q1) (value yes))
  (answer (question Q1A) (value no))
  (not (answer (question Q1A2)))
  (user-input (valid no))
  =>
  (printout t "3. Does it have big claws? (yes/no): ")
  (bind ?response (lowcase (read)))
  (assert (user-input (value ?response) (valid yes))))

(defrule Q1A2_valid_yes
  ?s <- (state (current-question Q1A2))
  ?i <- (user-input (value yes) (valid yes))
  (answer (question Q1) (value yes))
  (answer (question Q1A) (value no))
  (not (answer (question Q1A2)))
  =>
  (assert (answer (question Q1A2) (value yes)))
  (modify ?s (current-question conclusion))
  (retract ?i)
  (assert (user-input (valid no))))

(defrule Q1A2_valid_no
  ?s <- (state (current-question Q1A2))
  ?i <- (user-input (value no) (valid yes))
  (answer (question Q1) (value yes))
  (answer (question Q1A) (value no))
  (not (answer (question Q1A2)))
  =>
  (assert (answer (question Q1A2) (value no)))
  (modify ?s (current-question conclusion))
  (retract ?i)
  (assert (user-input (valid no))))

(defrule Q1A2_invalid
  ?i <- (user-input (value ?v&:(not (or (eq ?v yes) (eq ?v no)))) (valid yes))
  (state (current-question Q1A2))
  =>
  (printout t "Please answer yes or no." crlf)
  (retract ?i)
  (assert (user-input (valid no))))

;; ========== Q1B: TAILS PRESENT? ==========
(defrule Q1B_prompt
  (state (current-question Q1B))
  (answer (question Q1) (value no))
  (not (answer (question Q1B)))
  (user-input (valid no))
  =>
  (printout t "2. Are tails present? (yes/no): ")
  (bind ?response (lowcase (read)))
  (assert (user-input (value ?response) (valid yes))))

(defrule Q1B_valid_yes
  ?s <- (state (current-question Q1B))
  ?i <- (user-input (value yes) (valid yes))
  (answer (question Q1) (value no))
  (not (answer (question Q1B)))
  =>
  (assert (answer (question Q1B) (value yes)))
  (modify ?s (current-question Q1B1))
  (retract ?i)
  (assert (user-input (valid no))))

(defrule Q1B_valid_no
  ?s <- (state (current-question Q1B))
  ?i <- (user-input (value no) (valid yes))
  (answer (question Q1) (value no))
  (not (answer (question Q1B)))
  =>
  (assert (answer (question Q1B) (value no)))
  (assert (answer (question tail-count) (value 0)))
  (modify ?s (current-question Q1B2))
  (retract ?i)
  (assert (user-input (valid no))))

(defrule Q1B_invalid
  ?i <- (user-input (value ?v&:(not (or (eq ?v yes) (eq ?v no)))) (valid yes))
  (state (current-question Q1B))
  =>
  (printout t "Please answer yes or no." crlf)
  (retract ?i)
  (assert (user-input (valid no))))

;; ========== Q1B1: TAIL COUNT ==========
(defrule Q1B1_prompt
  (state (current-question Q1B1))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value yes))
  (not (answer (question tail-count)))
  (user-input (valid no))
  =>
  (printout t "3. How many tails? (1/2/>1): ")
  (bind ?response (read))
  (assert (user-input (value ?response) (valid yes))))

(defrule Q1B1_valid_1
  ?s <- (state (current-question Q1B1))
  ?i <- (user-input (value 1) (valid yes))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value yes))
  (not (answer (question tail-count)))
  =>
  (assert (answer (question tail-count) (value 1)))
  (modify ?s (current-question Q1B1A))
  (retract ?i)
  (assert (user-input (valid no))))

(defrule Q1B1_valid_2
  ?s <- (state (current-question Q1B1))
  ?i <- (user-input (value 2) (valid yes))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value yes))
  (not (answer (question tail-count)))
  =>
  (assert (answer (question tail-count) (value 2)))
  (modify ?s (current-question Q1B1B))
  (retract ?i)
  (assert (user-input (valid no))))

(defrule Q1B1_valid_more1
  ?s <- (state (current-question Q1B1))
  ?i <- (user-input (value >1) (valid yes))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value yes))
  (not (answer (question tail-count)))
  =>
  (assert (answer (question tail-count) (value >1)))
  (modify ?s (current-question Q1B1B))
  (retract ?i)
  (assert (user-input (valid no))))

(defrule Q1B1_valid_integer_more
  ?s <- (state (current-question Q1B1))
  ?i <- (user-input (value ?v) (valid yes))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value yes))
  (not (answer (question tail-count)))
  (test (integerp ?v))
  (test (>= ?v 3))
  =>
  (assert (answer (question tail-count) (value >1)))
  (modify ?s (current-question Q1B1B))
  (retract ?i)
  (assert (user-input (valid no))))

(defrule Q1B1_invalid
  ?i <- (user-input (value ?v) (valid yes))
  (state (current-question Q1B1))
  (test (not (or (eq ?v 1) (eq ?v 2) (eq ?v >1) (and (integerp ?v) (>= ?v 3)))))
  =>
  (printout t "Please answer 1, 2, or >1." crlf)
  (retract ?i)
  (assert (user-input (valid no))))

;; ========== Q1B1A: FEATHERY TAIL? ==========
(defrule Q1B1A_prompt
  (state (current-question Q1B1A))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value yes))
  (answer (question tail-count) (value 1))
  (not (answer (question feathery)))
  (user-input (valid no))
  =>
  (printout t "4. Does it have a feathery tail? (yes/no): ")
  (bind ?response (lowcase (read)))
  (assert (user-input (value ?response) (valid yes))))

(defrule Q1B1A_valid_yes
  ?s <- (state (current-question Q1B1A))
  ?i <- (user-input (value yes) (valid yes))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value yes))
  (answer (question tail-count) (value 1))
  (not (answer (question feathery)))
  =>
  (assert (answer (question feathery) (value yes)))
  (modify ?s (current-question conclusion))
  (retract ?i)
  (assert (user-input (valid no))))

(defrule Q1B1A_valid_no
  ?s <- (state (current-question Q1B1A))
  ?i <- (user-input (value no) (valid yes))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value yes))
  (answer (question tail-count) (value 1))
  (not (answer (question feathery)))
  =>
  (assert (answer (question feathery) (value no)))
  (modify ?s (current-question conclusion))
  (retract ?i)
  (assert (user-input (valid no))))

(defrule Q1B1A_invalid
  ?i <- (user-input (value ?v&:(not (or (eq ?v yes) (eq ?v no)))) (valid yes))
  (state (current-question Q1B1A))
  =>
  (printout t "Please answer yes or no." crlf)
  (retract ?i)
  (assert (user-input (valid no))))

;; ========== Q1B1B: EXACTLY 2 TAILS? / or >1 path ==========
(defrule Q1B1B_prompt_2
  (state (current-question Q1B1B))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value yes))
  (answer (question tail-count) (value 2))
  (not (answer (question exactly-2)))
  (user-input (valid no))
  =>
  (printout t "4. Exactly 2 tails? (yes/no): ")
  (bind ?response (lowcase (read)))
  (assert (user-input (value ?response) (valid yes))))

(defrule Q1B1B_valid_2_yes
  ?s <- (state (current-question Q1B1B))
  ?i <- (user-input (value yes) (valid yes))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value yes))
  (answer (question tail-count) (value 2))
  (not (answer (question exactly-2)))
  =>
  (assert (answer (question exactly-2) (value yes)))
  (modify ?s (current-question conclusion))
  (retract ?i)
  (assert (user-input (valid no))))

(defrule Q1B1B_valid_2_no
  ?s <- (state (current-question Q1B1B))
  ?i <- (user-input (value no) (valid yes))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value yes))
  (answer (question tail-count) (value 2))
  (not (answer (question exactly-2)))
  =>
  (assert (answer (question exactly-2) (value no)))
  (modify ?s (current-question conclusion))
  (retract ?i)
  (assert (user-input (valid no))))

(defrule Q1B1B_prompt_more
  (state (current-question Q1B1B))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value yes))
  (answer (question tail-count) (value >1))
  (not (answer (question tails-longer)))
  (user-input (valid no))
  =>
  (printout t "4. Are tails longer than body width? (yes/no): ")
  (bind ?response (lowcase (read)))
  (assert (user-input (value ?response) (valid yes))))

(defrule Q1B1B_valid_more_yes
  ?s <- (state (current-question Q1B1B))
  ?i <- (user-input (value yes) (valid yes))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value yes))
  (answer (question tail-count) (value >1))
  (not (answer (question tails-longer)))
  =>
  (assert (answer (question tails-longer) (value yes)))
  (modify ?s (current-question Q1B1B2))
  (retract ?i)
  (assert (user-input (valid no))))

(defrule Q1B1B_valid_more_no
  ?s <- (state (current-question Q1B1B))
  ?i <- (user-input (value no) (valid yes))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value yes))
  (answer (question tail-count) (value >1))
  (not (answer (question tails-longer)))
  =>
  (assert (answer (question tails-longer) (value no)))
  (modify ?s (current-question conclusion))
  (retract ?i)
  (assert (user-input (valid no))))

;; ========== Q1B1B2: THIN AND HAIRY TAILS? ==========
(defrule Q1B1B2_prompt
  (state (current-question Q1B1B2))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value yes))
  (answer (question tail-count) (value >1))
  (answer (question tails-longer) (value yes))
  (not (answer (question thin-hairy)))
  (user-input (valid no))
  =>
  (printout t "5. Are the tails thin and hairy? (yes/no): ")
  (bind ?response (lowcase (read)))
  (assert (user-input (value ?response) (valid yes))))

(defrule Q1B1B2_valid_yes
  ?s <- (state (current-question Q1B1B2))
  ?i <- (user-input (value yes) (valid yes))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value yes))
  (answer (question tail-count) (value >1))
  (answer (question tails-longer) (value yes))
  (not (answer (question thin-hairy)))
  =>
  (assert (answer (question thin-hairy) (value yes)))
  (modify ?s (current-question conclusion))
  (retract ?i)
  (assert (user-input (valid no))))

(defrule Q1B1B2_valid_no
  ?s <- (state (current-question Q1B1B2))
  ?i <- (user-input (value no) (valid yes))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value yes))
  (answer (question tail-count) (value >1))
  (answer (question tails-longer) (value yes))
  (not (answer (question thin-hairy)))
  =>
  (assert (answer (question thin-hairy) (value no)))
  (modify ?s (current-question conclusion))
  (retract ?i)
  (assert (user-input (valid no))))

;; ========== Q1B2: SEGMENTED ABDOMEN? ==========
(defrule Q1B2_prompt
  (state (current-question Q1B2))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value no))
  (answer (question tail-count) (value 0))
  (not (answer (question segmented)))
  (user-input (valid no))
  =>
  (printout t "3. Does it have a segmented abdomen? (yes/no): ")
  (bind ?response (lowcase (read)))
  (assert (user-input (value ?response) (valid yes))))

(defrule Q1B2_valid_yes
  ?s <- (state (current-question Q1B2))
  ?i <- (user-input (value yes) (valid yes))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value no))
  (answer (question tail-count) (value 0))
  (not (answer (question segmented)))
  =>
  (assert (answer (question segmented) (value yes)))
  (modify ?s (current-question Q1B2A))
  (retract ?i)
  (assert (user-input (valid no))))

(defrule Q1B2_valid_no
  ?s <- (state (current-question Q1B2))
  ?i <- (user-input (value no) (valid yes))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value no))
  (answer (question tail-count) (value 0))
  (not (answer (question segmented)))
  =>
  (assert (answer (question segmented) (value no)))
  (modify ?s (current-question Q1B2B))
  (retract ?i)
  (assert (user-input (valid no))))

;; ========== Q1B2A: STUBBY WINGS? ==========
(defrule Q1B2A_prompt
  (state (current-question Q1B2A))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value no))
  (answer (question tail-count) (value 0))
  (answer (question segmented) (value yes))
  (not (answer (question stubby)))
  (user-input (valid no))
  =>
  (printout t "4. Does it have stubby wings? (yes/no): ")
  (bind ?response (lowcase (read)))
  (assert (user-input (value ?response) (valid yes))))

(defrule Q1B2A_valid_yes
  ?s <- (state (current-question Q1B2A))
  ?i <- (user-input (value yes) (valid yes))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value no))
  (answer (question tail-count) (value 0))
  (answer (question segmented) (value yes))
  (not (answer (question stubby)))
  =>
  (assert (answer (question stubby) (value yes)))
  (modify ?s (current-question conclusion))
  (retract ?i)
  (assert (user-input (valid no))))

(defrule Q1B2A_valid_no
  ?s <- (state (current-question Q1B2A))
  ?i <- (user-input (value no) (valid yes))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value no))
  (answer (question tail-count) (value 0))
  (answer (question segmented) (value yes))
  (not (answer (question stubby)))
  =>
  (assert (answer (question stubby) (value no)))
  (modify ?s (current-question conclusion))
  (retract ?i)
  (assert (user-input (valid no))))

;; ========== Q1B2B: WINGS OVERLAP? ==========
(defrule Q1B2B_prompt
  (state (current-question Q1B2B))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value no))
  (answer (question tail-count) (value 0))
  (answer (question segmented) (value no))
  (not (answer (question wings-overlap)))
  (user-input (valid no))
  =>
  (printout t "4. Do the wings overlap diagonally? (yes/no): ")
  (bind ?response (lowcase (read)))
  (assert (user-input (value ?response) (valid yes))))

(defrule Q1B2B_valid_yes
  ?s <- (state (current-question Q1B2B))
  ?i <- (user-input (value yes) (valid yes))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value no))
  (answer (question tail-count) (value 0))
  (answer (question segmented) (value no))
  (not (answer (question wings-overlap)))
  =>
  (assert (answer (question wings-overlap) (value yes)))
  (modify ?s (current-question conclusion))
  (retract ?i)
  (assert (user-input (valid no))))

(defrule Q1B2B_valid_no
  ?s <- (state (current-question Q1B2B))
  ?i <- (user-input (value no) (valid yes))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value no))
  (answer (question tail-count) (value 0))
  (answer (question segmented) (value no))
  (not (answer (question wings-overlap)))
  =>
  (assert (answer (question wings-overlap) (value no)))
  (modify ?s (current-question conclusion))
  (retract ?i)
  (assert (user-input (valid no))))

;; ========== CONCLUSION RULES ==========
(defrule conclusion_water_spider
  (state (current-question conclusion))
  (answer (question Q1) (value yes))
  (answer (question Q1A) (value yes))
  =>
  (printout t crlf "==> Identification: Water Spider or Mite" crlf)
  (printout t "Why: 8 legs (4 pairs)" crlf crlf)
  (halt))

(defrule conclusion_crayfish
  (state (current-question conclusion))
  (answer (question Q1) (value yes))
  (answer (question Q1A) (value no))
  (answer (question Q1A2) (value yes))
  =>
  (printout t crlf "==> Identification: Freshwater Crayfish" crlf)
  (printout t "Why: big claws" crlf crlf)
  (halt))

(defrule conclusion_shrimp
  (state (current-question conclusion))
  (answer (question Q1) (value yes))
  (answer (question Q1A) (value no))
  (answer (question Q1A2) (value no))
  =>
  (printout t crlf "==> Identification: Freshwater Shrimp or Hog louse" crlf)
  (printout t "Why: no big claws" crlf crlf)
  (halt))

(defrule conclusion_alderfly
  (state (current-question conclusion))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value yes))
  (answer (question tail-count) (value 1))
  (answer (question feathery) (value yes))
  =>
  (printout t crlf "==> Identification: Alderfly Larva" crlf)
  (printout t "Why: single feathery tail" crlf crlf)
  (halt))

(defrule conclusion_water_stick
  (state (current-question conclusion))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value yes))
  (answer (question tail-count) (value 1))
  (answer (question feathery) (value no))
  =>
  (printout t crlf "==> Identification: Water Stick Insect or Water Scorpion" crlf)
  (printout t "Why: single tail, not feathery" crlf crlf)
  (halt))

(defrule conclusion_stonefly
  (state (current-question conclusion))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value yes))
  (answer (question tail-count) (value 2))
  (answer (question exactly-2) (value yes))
  =>
  (printout t crlf "==> Identification: Stonefly Nymph" crlf)
  (printout t "Why: exactly 2 tails" crlf crlf)
  (halt))

(defrule conclusion_mayfly
  (state (current-question conclusion))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value yes))
  (answer (question tail-count) (value >1))
  (answer (question tails-longer) (value yes))
  (answer (question thin-hairy) (value yes))
  =>
  (printout t crlf "==> Identification: Mayfly Nymph" crlf)
  (printout t "Why: multiple thin hairy tails longer than body" crlf crlf)
  (halt))

(defrule conclusion_damselfly
  (state (current-question conclusion))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value yes))
  (answer (question tail-count) (value >1))
  (answer (question tails-longer) (value yes))
  (answer (question thin-hairy) (value no))
  =>
  (printout t crlf "==> Identification: Damselfly Nymph" crlf)
  (printout t "Why: multiple tails longer than body, not thin/hairy" crlf crlf)
  (halt))

(defrule conclusion_dragonfly_more
  (state (current-question conclusion))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value yes))
  (answer (question tail-count) (value >1))
  (answer (question tails-longer) (value no))
  =>
  (printout t crlf "==> Identification: Dragonfly Nymph" crlf)
  (printout t "Why: multiple tails not longer than body" crlf crlf)
  (halt))

(defrule conclusion_water_boatman
  (state (current-question conclusion))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value no))
  (answer (question tail-count) (value 0))
  (answer (question segmented) (value no))
  (answer (question wings-overlap) (value yes))
  =>
  (printout t crlf "==> Identification: Water Boatman" crlf)
  (printout t "Why: wings overlap diagonally" crlf crlf)
  (halt))

(defrule conclusion_water_beetle
  (state (current-question conclusion))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value no))
  (answer (question tail-count) (value 0))
  (answer (question segmented) (value no))
  (answer (question wings-overlap) (value no))
  =>
  (printout t crlf "==> Identification: Water Beetle" crlf)
  (printout t "Why: no segmented abdomen, wings don't overlap diagonally" crlf crlf)
  (halt))

(defrule conclusion_dragonfly
  (state (current-question conclusion))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value no))
  (answer (question tail-count) (value 0))
  (answer (question segmented) (value yes))
  (answer (question stubby) (value yes))
  =>
  (printout t crlf "==> Identification: Dragonfly Nymph" crlf)
  (printout t "Why: segmented abdomen, stubby wings" crlf crlf)
  (halt))

(defrule conclusion_caddis
  (state (current-question conclusion))
  (answer (question Q1) (value no))
  (answer (question Q1B) (value no))
  (answer (question tail-count) (value 0))
  (answer (question segmented) (value yes))
  (answer (question stubby) (value no))
  =>
  (printout t crlf "==> Identification: Caseless Caddis Fly or Beetle Larvae" crlf)
  (printout t "Why: segmented abdomen, no stubby wings" crlf crlf)
  (halt))

(defrule fallback
  (state (current-question conclusion))
  =>
  (printout t crlf "Unable to identify. Please check your answers." crlf)
  (halt))