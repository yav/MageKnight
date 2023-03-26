function newCards() {

  function joinName(x,y) { return x + " / " + y }

  function getName(deed) {
    if (!deed.deedNamePower) return deed.deedName
    return joinName(deed.deedName,deed.deedNamePower)
  }

  const dims =
    { Action:
        { cols: 7, ho: 0.22, vo: [6.3]
        , boxes: [ [0.57, 0.21], [0.72, 0.37] ]
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
        { cols: 8, ho: 0.22, vo: [6.3]
        , boxes: [ [0.57, 0.21], [0.72, 0.37] ]
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

    , Artifact:
        { cols: 5, ho: 0.22, vo: [6.0]
        , boxes: []
        , cards:
            []
        }

    , Spell:
        { cols: 6, ho: 0.24, vo: [0.3,6.5]
        , boxes: [ [0.02, 0.67], [0.5, 0.67] ]
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
        { cols: 1, ho: 0.22, vo: [11.4]
        , cards: ["Wound"]
        }
    }
    const cardLoc = {}
    for (const ty in dims) {
      const inf = dims[ty]
      const cs = inf.cards
      const cols = inf.cols
      for (let i = 0; i < cs.length; ++i) {
        const r = Math.floor(i / cols)
        cardLoc[cs[i]] = [ r, i - r * cols ]
      }
    }

  function drawDeed(deed) {
    const name    = getName(deed)
    const [r,c]   = cardLoc[name]
    const cardTy  = deed.deedType.tag
    const info    = dims[cardTy]

    function small() {

      const dom = uiFromTemplate("card-small")
      dom.style.setProperty("--background-columns",info.cols)

      for (let i = 0; i < info.vo.length; ++i) {
        const s = uiFromTemplate("card-element");
        s.classList.add(cardTy)
        const hoff = constant.smallCardWidth * info.ho
        const voff = constant.smallCardHeight * info.vo[i]
        s.style.backgroundPosition =
            (-c * constant.cardWidth  - hoff) + "px " +
            (-r * constant.cardHeight - voff) + "px"

        if (i !== 0) dom.appendChild(document.createElement("br"))
        dom.appendChild(s)
      }

      return dom
    }

    function big(d) {
      const dom = uiFromTemplate("card-big")
      dom.classList.add(cardTy)
      dom.style.setProperty("--background-columns",info.cols)
      dom.style.backgroundPosition =
        (-c * constant.cardWidth)  + "px " +
        (-r * constant.cardHeight) + "px"

      return dom
    }

    function selectorBoxes(dom) {
      const boxes = info.boxes

      const bs = []
      for (let i = 0; i < boxes.length; ++i) {
        const [boxY,boxH] = boxes[i]
        const box = uiFromTemplate("card-big-selector")
        box.style.setProperty("--mk-box-height", boxH)
        box.style.setProperty("--mk-box-top", boxY)
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
  obj.getName     = getName
  obj.drawDeed    = drawDeed

  return obj

}
