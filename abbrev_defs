;;-*-coding: utf-8;-*-

;; The creation of this table is assisted by ChatGPT4:
;; https://chat.openai.com/share/441c339e-1032-463d-afcb-9ca65f38d5e8

(defconst math-abbrevs-table
  '(;; binary operators
    ("xxpm"            "±")
    ("xxcap"           "∩")
    ("xxdiamond"       "⋄")
    ("xxoplus"         "⊕")
    ("xxmp"            "∓")
    ("xxcup"           "∪")
    ("xxtriangleup"    "△")
    ("xxominus"        "⊖")
    ("xxtimes"         "×")
    ("xxuplus"         "⊎")
    ("xxtriangledown"  "▽")
    ("xxotimes"        "⊗")
    ("xxdiv"           "÷")
    ("xxsqcap"         "⊓")
    ("xxtriangleleft"  "◃")
    ("xxoslash"        "⊘")
    ("xxast"           "∗")
    ("xxsqcup"         "⊔")
    ("xxtriangleright" "▹")
    ("xxodot"          "⊙")
    ("xxstar"          "⋆")
    ("xxvee"           "∨")
    ("xxbigcirc"       "◯")
    ("xxcirc"          "∘")
    ("xxdagger"        "†")
    ("xxwedge"         "∧")
    ("xxbullet"        "∙")
    ("xxsetminus"      "∖")
    ("xxddagger"       "‡")
    ("xxcdot"          "⋅")
    ("xxwr"            "≀")
    ("xxamalg"         "⨿")
    ;; set and/or logic notations
    ("xxemptyset"     "∅")
    ("xxN"            "ℕ")
    ("xxZ"            "ℤ")
    ("xxQ"            "ℚ")
    ("xxA"            "𝔸")
    ("xxR"            "ℝ")
    ("xxC"            "ℂ")
    ("xxH"            "ℍ")
    ("xxO"            "𝕆")
    ("xxS"            "𝕊")
    ("xxin"           "∈")
    ("xxnotin"        "∉")
    ("xxowns"         "∋")
    ("xxsubset"       "⊂")
    ("xxsubseteq"     "⊆")
    ("xxsupset"       "⊃")
    ("xxsupseteq"     "⊇")
    ("xxunion"        "∪")
    ("xxintersection" "∩")
    ("xxsetminus"     "∖")
    ;; Calligraphy: \mathcal
    ("xxmathcalA" "𝒜")
    ("xxmathcalB" "ℬ")
    ("xxmathcalC" "𝒞")
    ("xxmathcalD" "𝒟")
    ("xxmathcalE" "ℰ")
    ("xxmathcalF" "ℱ")
    ("xxmathcalG" "𝒢")
    ("xxmathcalH" "ℋ")
    ("xxmathcalI" "ℐ")
    ("xxmathcalJ" "𝒥")
    ("xxmathcalK" "𝒦")
    ("xxmathcalL" "ℒ")
    ("xxmathcalM" "ℳ")
    ("xxmathcalN" "𝒩")
    ("xxmathcalO" "𝒪")
    ("xxmathcalP" "𝒫")
    ("xxmathcalQ" "𝒬")
    ("xxmathcalR" "ℛ")
    ("xxmathcalS" "𝒮")
    ("xxmathcalT" "𝒯")
    ("xxmathcalU" "𝒰")
    ("xxmathcalV" "𝒱")
    ("xxmathcalW" "𝒲")
    ("xxmathcalX" "𝒳")
    ("xxmathcalY" "𝒴")
    ("xxmathcalZ" "𝒵")
    ;; Fraktur: \mathfrak
    ("xxmathfrakA" "𝔄")
    ("xxmathfrakB" "𝔅")
    ("xxmathfrakC" "𝔆")
    ("xxmathfrakD" "𝔇")
    ("xxmathfrakE" "𝔈")
    ("xxmathfrakF" "𝔉")
    ("xxmathfrakG" "𝔊")
    ("xxmathfrakH" "𝔋")
    ("xxmathfrakI" "𝔌")
    ("xxmathfrakJ" "𝔍")
    ("xxmathfrakK" "𝔎")
    ("xxmathfrakL" "𝔏")
    ("xxmathfrakM" "𝔐")
    ("xxmathfrakN" "𝔑")
    ("xxmathfrakO" "𝔒")
    ("xxmathfrakP" "𝔓")
    ("xxmathfrakQ" "𝔔")
    ("xxmathfrakR" "𝔕")
    ("xxmathfrakS" "𝔖")
    ("xxmathfrakT" "𝔗")
    ("xxmathfrakU" "𝔘")
    ("xxmathfrakV" "𝔙")
    ("xxmathfrakW" "𝔚")
    ("xxmathfrakX" "𝔛")
    ("xxmathfrakY" "𝔜")
    ("xxmathfrakZ" "ℨ")
    ;; /misc
    ("xxmultimap" "⊸")
    ("xxforall"   "∀")
    ("xxbsquare" "■")
    ("xxsquare"  "□")
    ("xx[[" "⟦")
    ("xx]]" "⟧")
    ))

(defconst greek-abbrevs-table
  '(("xxalpha"   "α")
    ("xxbeta"    "β")
    ("xxgamma"   "γ")
    ("xxdelta"   "δ")
    ("xxepsilon" "ε")
    ("xxzeta"    "ζ")
    ("xxeta"     "η")
    ("xxtheta"   "θ")
    ("xxiota"    "ι")
    ("xxkappa"   "κ")
    ("xxlambda"  "λ")
    ("xxmu"      "μ")
    ("xxnu"      "ν")
    ("xxxi"      "ξ")
    ("xxomicron" "ο")
    ("xxpi"      "π")
    ("xxrho"     "ρ")
    ("xxsigma"   "σ")
    ("xxtau"     "τ")
    ("xxupsilon" "υ")
    ("xxphi"     "φ")
    ("xxchi"     "χ")
    ("xxpsi"     "ψ")
    ("xxomega"   "ω")
    ("xxAlpha"   "Α")
    ("xxBeta"    "Β")
    ("xxGamma"   "Γ")
    ("xxDelta"   "Δ")
    ("xxEpsilon" "Ε")
    ("xxZeta"    "Ζ")
    ("xxEta"     "Η")
    ("xxTheta"   "Θ")
    ("xxIota"    "Ι")
    ("xxKappa"   "Κ")
    ("xxLambda"  "Λ")
    ("xxMu"      "Μ")
    ("xxNu"      "Ν")
    ("xxXi"      "Ξ")
    ("xxOmicron" "Ο")
    ("xxPi"      "Π")
    ("xxRho"     "Ρ")
    ("xxSigma"   "Σ")
    ("xxTau"     "Τ")
    ("xxUpsilon" "Υ")
    ("xxPhi"     "Φ")
    ("xxChi"     "Χ")
    ("xxPsi"     "Ψ")
    ("xxOmega"   "Ω")))

(define-abbrev-table 'global-abbrev-table
  (append math-abbrevs-table greek-abbrevs-table))