;;-*-coding: utf-8;-*-
;;
;; refs:
;; - https://oeis.org/wiki/List_of_LaTeX_mathematical_symbols
;; - https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode
(defconst math-symbols-abbrevs-table
  '(;; common binary operators
    ("xxcdot"          "⋅")
    ("xxcirc"          "∘")
    ("xxCirc"          "◯")
    ("xxoplus"         "⊕")
    ("xxodot"          "⊙")
    ("xxotimes"        "⊗")
    ("xxoslash"        "⊘")
    ("xxominus"        "⊖")
    ("xxuplus"         "⊎")
    ("xxtimes"         "×")
    ("xxdiv"           "÷")
    ("xxpm"            "±")
    ("xxmp"            "∓")
    ("xxcap"           "∩")
    ("xxcup"           "∪")
    ("xxdiamond"       "⋄")
    ("xxtriangleleft"  "◃")
    ("xxtriangleright" "▹")
    ("xxtriangleup"    "▵")
    ("xxtriangledown"  "▿")
    ("xxTriangleleft"  "◁")
    ("xxTrianglerigt"  "▷")
    ("xxTriangleup"    "△")
    ("xxTriangledown"  "▽")
    ("xxsquare"        "□")
    ("xxblacksquare"   "■")
    ("xxsqcap"         "⊓")
    ("xxsqcup"         "⊔")
    ("xxdagger"        "†")
    ("xxddagger"       "‡")
    ("xxwr"            "≀")
    ("xxamalg"         "⨿")
    ("xxbullet"        "∙")
    ("xxstar"          "⋆")
    ("xxast"           "∗")
    ;; set symbols
    ("xxemptyset"     "∅")
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
    ;; logic symbols: https://en.wikipedia.org/wiki/List_of_logic_symbols
    ("xxneg"         "¬")
    ("xxtop"         "⊤")
    ("xxbot"         "⊥")
    ("xxwedge"       "∧")
    ("xxvee"         "∨")
    ("xxvdash"       "⊢")
    ("xxvDash"       "⊨")
    ("xxnvdash"      "⊭")
    ("xxnvDash"      "⊨")
    ("xxforall"      "∀")
    ("xxexists"      "∃")
    ("xxBox"         "□") ;; necessity; LTL: Globally
    ("xxDiamond"     "◇") ;; possibility;
    ("xxlozenge"     "◊") ;; LTL: Finally
    ("xxtherefor"    "∴")
    ("xxbecause"     "∵")
    ;; Equalities: https://math.stackexchange.com/questions/864606/difference-between-%E2%89%88-%E2%89%83-and-%E2%89%85
    ("xxequiv"  "≡")
    ("xxsim"    "∼")
    ("xxapprox" "≈")
    ("xxsimeq"  "≃")
    ("xxcong"   "≅")
    ("coloneqq" "≔")
    ;; fancy arrows:
    ;; - https://latextutorials.com/symbols/arrows/
    ;; - https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode
    ("xximplies"        "→")
    ("xxmultimap"       "⊸")
    ("xxleadsto"        "↝") ;; ↝ (U+219D) vs. ⤳ (U+2933)
    ("xxRightarrow      "⇒")
    ("xxLeftarrow       "⇐")
    ("xxLeftrightarrow" "⇔")
    ("xxleftrightarrow" "↔")
    ;; fancy brackets
    ("xx[[" "⟦")
    ("xx]]" "⟧")
))

;; https://en.wikipedia.org/wiki/Mathematical_Alphanumeric_Symbols
(defconst math-alphabet-abbrevs-table
  '(;; common symbols
    ("xxN" "ℕ") ;; natural numbers
    ("xxZ" "ℤ") ;; integers
    ("xxQ" "ℚ") ;; rational numbers
    ("xxR" "ℝ") ;; real numbers
    ("xxC" "ℂ") ;; complex numbers
    ;; Blackboard bold: https://en.wikipedia.org/wiki/Blackboard_bold
    ("xxmathbbA" "𝔸") ;; U+1D538
    ("xxmathbbB" "𝔹") ;; U+1D539
    ("xxmathbbC" "ℂ") ;; U+2102
    ("xxmathbbD" "𝔻") ;; U+1D53B
    ("xxmathbbE" "𝔼") ;; U+1D53C
    ("xxmathbbF" "𝔽") ;; U+1D53D
    ("xxmathbbG" "𝔾") ;; U+1D53E
    ("xxmathbbH" "ℍ") ;; U+210D
    ("xxmathbbI" "𝕀") ;; U+1D540
    ("xxmathbbJ" "𝕁") ;; U+1D541
    ("xxmathbbK" "𝕂") ;; U+1D542
    ("xxmathbbL" "𝕃") ;; U+1D543
    ("xxmathbbM" "𝕄") ;; U+1D544
    ("xxmathbbN" "ℕ") ;; U+2115
    ("xxmathbbO" "𝕆") ;; U+1D546
    ("xxmathbbP" "ℙ") ;; U+2119
    ("xxmathbbQ" "ℚ") ;; U+211A
    ("xxmathbbR" "ℝ") ;; U+211D
    ("xxmathbbS" "𝕊") ;; U+1D54A
    ("xxmathbbT" "𝕋") ;; U+1D54B
    ("xxmathbbU" "𝕌") ;; U+1D54C
    ("xxmathbbV" "𝕍") ;; U+1D54D
    ("xxmathbbW" "𝕎") ;; U+1D54E
    ("xxmathbbX" "𝕏") ;; U+1D54F
    ("xxmathbbY" "𝕐") ;; U+1D550
    ("xxmathbbZ" "ℤ") ;; U+2124
    ;; Calligraphy Bold: \mathcal
    ("xxmathcalA" "𝓐")
    ("xxmathcalB" "𝓑")
    ("xxmathcalC" "𝓒")
    ("xxmathcalD" "𝓓")
    ("xxmathcalE" "𝓔")
    ("xxmathcalF" "𝓕")
    ("xxmathcalG" "𝓖")
    ("xxmathcalH" "𝓗")
    ("xxmathcalI" "𝓘")
    ("xxmathcalJ" "𝓙")
    ("xxmathcalK" "𝓚")
    ("xxmathcalL" "𝓛")
    ("xxmathcalM" "𝓜")
    ("xxmathcalN" "𝓝")
    ("xxmathcalO" "𝓞")
    ("xxmathcalP" "𝓟")
    ("xxmathcalQ" "𝓠")
    ("xxmathcalR" "𝓡")
    ("xxmathcalS" "𝓢")
    ("xxmathcalT" "𝓣")
    ("xxmathcalU" "𝓤")
    ("xxmathcalV" "𝓥")
    ("xxmathcalW" "𝓦")
    ("xxmathcalX" "𝓧")
    ("xxmathcalY" "𝓨")
    ("xxmathcalZ" "𝓩")
    ;; Calligraphy Normal: \mathscr
    ("xxmathscrA" "𝒜")
    ("xxmathscrB" "ℬ")
    ("xxmathscrC" "𝒞")
    ("xxmathscrD" "𝒟")
    ("xxmathscrE" "ℰ")
    ("xxmathscrF" "ℱ")
    ("xxmathscrG" "𝒢")
    ("xxmathscrH" "ℋ")
    ("xxmathscrI" "ℐ")
    ("xxmathscrJ" "𝒥")
    ("xxmathscrK" "𝒦")
    ("xxmathscrL" "ℒ")
    ("xxmathscrM" "ℳ")
    ("xxmathscrN" "𝒩")
    ("xxmathscrO" "𝒪")
    ("xxmathscrP" "𝒫")
    ("xxmathscrQ" "𝒬")
    ("xxmathscrR" "ℛ")
    ("xxmathscrS" "𝒮")
    ("xxmathscrT" "𝒯")
    ("xxmathscrU" "𝒰")
    ("xxmathscrV" "𝒱")
    ("xxmathscrW" "𝒲")
    ("xxmathscrX" "𝒳")
    ("xxmathscrY" "𝒴")
    ("xxmathscrZ" "𝒵")
    ;; Fraktur Normal: \mathfrak
    ("xxmathfrakA" "𝔄")
    ("xxmathfrakB" "𝔅")
    ("xxmathfrakC" "ℭ")
    ("xxmathfrakD" "𝔇")
    ("xxmathfrakE" "𝔈")
    ("xxmathfrakF" "𝔉")
    ("xxmathfrakG" "𝔊")
    ("xxmathfrakH" "ℌ")
    ("xxmathfrakI" "ℑ")
    ("xxmathfrakJ" "𝔍")
    ("xxmathfrakK" "𝔎")
    ("xxmathfrakL" "𝔏")
    ("xxmathfrakM" "𝔐")
    ("xxmathfrakN" "𝔑")
    ("xxmathfrakO" "𝔒")
    ("xxmathfrakP" "𝔓")
    ("xxmathfrakQ" "𝔔")
    ("xxmathfrakR" "ℜ")
    ("xxmathfrakS" "𝔖")
    ("xxmathfrakT" "𝔗")
    ("xxmathfrakU" "𝔘")
    ("xxmathfrakV" "𝔙")
    ("xxmathfrakW" "𝔚")
    ("xxmathfrakX" "𝔛")
    ("xxmathfrakY" "𝔜")
    ("xxmathfrakZ" "ℨ")
))

(defconst greek-alphabet-abbrevs-table
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


(defconst emoji-abbrevs-table
  '(("xxgreencheck"   "✅")
    ("xxredquestion"  "❓")
    ("xxwip"          "🚧")
    ("xxwarning"      "⚠️")
    ("xxstar"         "⭐")
    ("xxglowingstar"  "🌟")
    ("xxnoentry"      "⛔")
    ("xxgreencircle"  "🟢")
    ("xxorangecircle" "🟠")
    ("xxredcircle"    "🔴")
    ("xxshrug"        "¯\_(ツ)_/¯")))

(define-abbrev-table
  'global-abbrev-table
  (append math-symbols-abbrevs-table
          math-alphabet-abbrevs-table
          greek-alphabet-abbrevs-table
          emoji-abbrevs-table))
