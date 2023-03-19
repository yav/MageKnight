function newCards() {
  const cardWidth  = 220
  const cardHeight = 308

  function joinName(x,y) { return x + " / " + y }

  function getName(deed) {
    if (!deed.deedNamePower) return deed.deedName
    return joinName(deed.deedName,deed.deedNamePower)
  }

  const dims =
    { Action:
        { width: 7000, heght: 5600, rows: 4, cols: 7, ho: 0.22, vo: [6.3]
        , cards:
            [ "March"
            , "Tranquility"
            , "Concentration"
            , "Savage Harvesting"
            , ""
            , "Rejuvenate"
            , "Will Focus"

            , "Swiftness"
            , "Promise"
            , "Mana Draw"
            , "Swift Reflexes"
            , ""
            , "Noble Manners"
            , "Mana Pull"

            , "Stamina"
            , "Crystallize"
            , "Determination"
            , "Tirelessness"
            , ""
            , "Crystal Joy"
            , "Cold Toughness"

            , "Rage"
            , "Threaten"
            , "Improvisation"
            , "Battle Versatility"
            , ""
            , "Ruthless Coercion"
            , "Instinct"
            ]
        }

    , AdvancedAction:
        { width: 8000, heght: 8400, rows: 6, cols: 8, ho: 0.22, vo: [6.3]
        , cards:
            [ "Fire Bolt"
            , "Ice Bolt"
            , "Swift Bolt"
            , "Crushing Bolt"
            , "Blood Rage"
            , "Ice Shield"
            , "Agility"
            , "Refreshing Walk"

            , "Intimidate"
            , "Frost Bridge"
            , "Song of Wind"
            , "Path Finding"
            , "Blood Ritual"
            , "Pure Magic"
            , "Heroic Tale"
            , "Regeneration"

            , "Into the Heat"
            , "Steady Tempo"
            , "Diplomacy"
            , "In Need"
            , "Decompose"
            , "Crystal Mastery"
            , "Mana Storm"
            , "Ambush"

            , "Maximal Effect"
            , "Magic Talent"
            , "Learning"
            , "Training"
            , "Counterattack"
            , "Ritual Attack"
            , "Blood of Ancients"
            , "Shield Bash"

            , "Temporal Portal"
            , "Spell Forge"
            , "Chivalry"
            , "Peaceful Moment"
            , "Dodge and Weave"
            , "Stout Resolve"
            , "Force of Nature"
            , "Mountain Lore"
            ]
         }

    , units_regular:
        { width: 4000, heght: 5600, rows: 4, cols: 4, ho: 0.22, vo: [6.3]
        , cards:
            []
        }

    , units_elite:  
        { width: 4000, heght: 5600, rows: 4, cols: 4, ho: 0.22, vo: [6.3]
        , cards:
            []
        }

    , artifacts:
        { width: 5000, heght: 7000, rows: 5, cols: 5, ho: 0.22, vo: [6.0]
        , cards:
            []
        }

    , Spell:
        { width: 6000, heght: 5600, rows: 4, cols: 6, ho: 0.24, vo: [0.3,6.5]
        , cards:
            [ joinName("Energy Flow"         , "Energy Steal")
            , joinName("Cure"                , "Disease")
            , joinName("Meditation"          , "Trance")
            , joinName("Tremor"              , "Earthquake")
            , joinName("Underground Travel"  , "Underground Attack")
            , joinName("Restoration"         , "Rebirth")
            , joinName("Mana Meltdown"       , "Mana Radiance")
            , joinName("Demolish"            , "Disintegrate")
            , joinName("Burning Shield"      , "Exploding Shield")
            , joinName("Fireball"            , "Firestorm")
            , joinName("Flame Wall"          , "Flame Wave")
            , joinName("Offering"            , "Sacrifice")
            , joinName("Mana Claim"          , "Mana Curse")
            , joinName("Chill"               , "Lethal Chill")
            , joinName("Mistw Form"          , "Veil of Mist")
            , joinName("Snowstorm"           , "Blizzard")
            , joinName("Space Bending"       , "Time Bending")
            , joinName("Mana Bolt"           , "Mana Thunderbolt")
            , joinName("Mind Read"           , "Mind Steal")
            , joinName("Expose"              , "Mass Expose")
            , joinName("Call to Arms"        , "Call to Glory")
            , joinName("Charm"               , "Possess")
            , joinName("Whirlwind"           , "Tornado")
            , joinName("Wings of Wind"       , "Wings of Night")
            ]
        }

    , Wound:
        { width: 1000, height: 1400, rows: 1, cols: 1, ho: 0.22, vo: [11.4]
        , cards: ["Wound"]
        }
    }

    const cardLoc = {}
    for (const ty in dims) {
      const inf = dims[ty]
      const cs = inf.cards
      for (let i = 0; i < cs.length; ++i) {
        const r = Math.floor(i / inf.cols)
        cardLoc[cs[i]] = [ r, i - r * inf.cols ]
      }
    }


  function drawDeed(deed) {
    console.log(deed)
    console.log(getName(deed))
    const [r,c] = cardLoc[getName(deed)]
    return newCard(deed.deedType.tag,r,c)
  }

  function newCard(cardTy,r,c) {

    const url = "img/cards/" + cardTy + ".jpg"
    const info = dims[cardTy]

    function card() {
      const dom = html.div("card element")
      html.setSize(dom,"backgroundSize", info.cols * cardWidth)
      dom.style.backgroundImage = "url(\"" + url + "\")"
      html.setSize(dom,"borderRadius", cardWidth / 30)
      return dom
    }

    const vf   = 0.08
    const hf   = 0.70

    function small(d,i) {
      const hoff = cardWidth * hf * info.ho
      const voff = cardHeight * vf * info.vo[i]

      html.setDim(d, cardWidth * hf, cardHeight * vf)
      html.setSize2(d,"backgroundPosition",
                          -c * cardWidth -hoff, -r * cardHeight - voff)

    }

    function big(d) {
      d.classList.add("big")
      html.setDim(d, cardWidth, cardHeight)
      html.setSize2(d,"backgroundPosition", -c * cardWidth, -r * cardHeight)
    }

    const cardC = html.div("card")
    html.setDim(cardC, cardWidth * hf, cardHeight * vf * info.vo.length)

    const bigCard = card()
    big(bigCard)
    bigCard.classList.add("hidden")
    cardC.appendChild(bigCard)

    const smallCard = html.div("small card")

    for (let i = 0; i < info.vo.length; ++i) {
      const s = card()
      small(s,i)
      if (i !== 0) smallCard.appendChild(html.br())
      smallCard.appendChild(s)
    }

    cardC.appendChild(smallCard)
    newTooltip(cardC, bigCard)

    return cardC
  }

  function drawSet(set) {
    const info = dims[set]
    const col = html.div("card-container")
    for (let r = 0; r < info.rows; ++r)
      for (let c = 0; c < info.cols; ++c)
        col.appendChild(newCard(set,r,c))

    gui.container.appendChild(col)
  }



  const obj = {}
  obj.drawDeed = drawDeed
  obj.drawSet  = drawSet

  return obj

}
