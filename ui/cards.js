function newCards() {
  const cardWidth  = 220
  const cardHeight = 308

  // Size of small card as fraction of full card
  const vf         = 0.08
  const hf         = 0.70

  function joinName(x,y) { return x + " / " + y }

  function getName(deed) {
    if (!deed.deedNamePower) return deed.deedName
    return joinName(deed.deedName,deed.deedNamePower)
  }

  const dims =
    { Action:
        { width: 7000, heght: 5600, rows: 4, cols: 7, ho: 0.22, vo: [6.3]
        , boxes: [ [0.57, 0.15], [0.72, 0.25] ]
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
        , boxes: [ [0.57, 0.15], [0.72, 0.25] ]
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
        , boxes: []
        , cards:
            []
        }

    , units_elite:  
        { width: 4000, heght: 5600, rows: 4, cols: 4, ho: 0.22, vo: [6.3]
        , boxes: []
        , cards:
            []
        }

    , artifacts:
        { width: 5000, heght: 7000, rows: 5, cols: 5, ho: 0.22, vo: [6.0]
        , boxes: []
        , cards:
            []
        }

    , Spell:
        { width: 6000, heght: 5600, rows: 4, cols: 6, ho: 0.24, vo: [0.3,6.5]
        , boxes: [ [0.02, 0.48], [0.5, 0.48] ]
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
    const name    = getName(deed)
    const [r,c]   = cardLoc[name]
    const cardTy  = deed.deedType.tag
    const url     = "img/cards/" + cardTy + ".jpg"
    const info    = dims[cardTy]

    function card(ty) {
      const dom = html.div("card " + ty)
      html.setSize(dom,"backgroundSize", info.cols * cardWidth)
      dom.style.backgroundImage = "url(\"" + url + "\")"
      html.setSize(dom,"borderRadius", cardWidth / 30)
      return dom
    }

    function small() {

      const dom = html.div("small card")

      for (let i = 0; i < info.vo.length; ++i) {
        const s    = card("element")
        const hoff = cardWidth * hf * info.ho
        const voff = cardHeight * vf * info.vo[i]

        html.setDim(s, cardWidth * hf, cardHeight * vf)
        html.setSize2(s,"backgroundPosition",
                           -c * cardWidth -hoff, -r * cardHeight - voff)

        if (i !== 0) dom.appendChild(html.br())
        dom.appendChild(s)
      }

      return dom
    }

    function big(d) {
      const dom = card("big")
      html.setDim(dom, cardWidth, cardHeight)
      html.setSize2(dom,"backgroundPosition", -c * cardWidth, -r * cardHeight)

      return dom
    }

    function selectorBoxes(dom) {
      const outline = 0.025
      const boxes = info.boxes

      const bs = []
      for (let i = 0; i < boxes.length; ++i) {
        const [boxY,boxH] = boxes[i]
        const box = html.div("box hidden")
        html.setSize(box,"border-radius",cardWidth/30)
        html.setDim(box, (1 - 2 * outline) * cardWidth, boxH * cardHeight )
        html.setSize(box, "left", outline * cardWidth)
        html.setSize(box, "top",  boxY * cardHeight)
        bs[i] = box
        dom.appendChild(box)
      }

      return {
        get:  (i) => bs[i],
        hide: (i) => bs[i].classList.add("hidden"),
        show: (i) => bs[i].classList.remove("hidden"),
      }
    }

    const obj = {}
    obj.name = name
    obj.small = small
    obj.big = big
    obj.selectors = selectorBoxes
    return obj
  }

  const obj = {}
  obj.width       = cardWidth
  obj.height      = cardHeight
  obj.getName     = getName
  obj.drawDeed    = drawDeed

  return obj

}
