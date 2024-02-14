import { uiFromTemplate } from "./common-js/template.ts"
import { constant } from "./constants.ts"

export
type CardsUI = {
  getName: (d: DeedUI) => string,
  drawDeed: (name: string) => DeedUI
}


export 
type DeedUI = {
  name: string
  small: () => HTMLElement,
  big: () => HTMLElement,
  selectors: (container: HTMLElement) => SelectorBoxes
}

export
type SelectorBoxes = {
  get:  (i: number) => HTMLElement,
  hide: (i: number) => void,
  show: (i: number) => void
}

export
function newCards(): CardsUI {

  type Cards<T> =
    { Action: T, AdvancedAction: T, Artifact: T, Spell: T, Wound: T }

  type CardTy = keyof Cards<void>

  type CardDef =
    { cols: number, ho: number, vo: number[]
    , boxes: [number,number][]
    , cards: string []
    }
    

  const dims: Cards<CardDef> =
    { Action:
        { cols: 7, ho: 0.22, vo: [6.3]
        , boxes: [ [0.57, 0.21], [0.72, 0.37] ]
        , cards:
            [ "March"
            , "Tranquility"
            , "Concentration"
            , "Savage_Harvesting"
            , ""
            , "Rejuvenate"
            , "Will_Focus"

            , "Swiftness"
            , "Promise"
            , "Mana_Draw"
            , "Swift_Reflexes"
            , ""
            , "Noble_Manners"
            , "Mana_Pull"

            , "Stamina"
            , "Crystallize"
            , "Determination"
            , "Tirelessness"
            , ""
            , "Crystal_Joy"
            , "Cold_Toughness"

            , "Rage"
            , "Threaten"
            , "Improvisation"
            , "Battle_Versatility"
            , ""
            , "Ruthless_Coercion"
            , "Instinct"
            ]
        }

    , AdvancedAction:
        { cols: 8, ho: 0.22, vo: [6.3]
        , boxes: [ [0.57, 0.21], [0.72, 0.37] ]
        , cards:
            [ "Fire_Bolt"
            , "Ice_Bolt"
            , "Swift_Bolt"
            , "Crushing_Bolt"
            , "Blood_Rage"
            , "Ice_Shield"
            , "Agility"
            , "Refreshing_Walk"

            , "Intimidate"
            , "Frost_Bridge"
            , "Song_of_Wind"
            , "Path_Finding"
            , "Blood_Ritual"
            , "Pure_Magic"
            , "Heroic_Tale"
            , "Regeneration"

            , "Into_the_Heat"
            , "Steady_Tempo"
            , "Diplomacy"
            , "In_Need"
            , "Decompose"
            , "Crystal_Mastery"
            , "Mana_Storm"
            , "Ambush"

            , "Maximal_Effect"
            , "Magic_Talent"
            , "Learning"
            , "Training"
            , "Counterattack"
            , "Ritual_Attack"
            , "Blood_of_Ancients"
            , "Shield_Bash"

            , "Temporal_Portal"
            , "Spell_Forge"
            , "Chivalry"
            , "Peaceful_Moment"
            , "Dodge_and_Weave"
            , "Stout_Resolve"
            , "Force_of_Nature"
            , "Mountain_Lore"
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
            [ "Energy_Flow"
            , "Cure"
            , "Meditation"
            , "Tremor"
            , "Underground_Travel"
            , "Restoration"
            , "Mana Meltdown"
            , "Demolish"
            , "Burning_Shield"
            , "Fireball"
            , "Flame_Wall"
            , "Offering"
            , "Mana_Claim"
            , "Chill"
            , "Mist_Form"
            , "Snowstorm"
            , "Space_Bending"
            , "Mana_Bolt"
            , "Mind_Read"
            , "Expose"
            , "Call_to_Arms"
            , "Charm"
            , "Whirlwind"
            , "Wings_of_Wind"
            ]
        }

    , Wound:
        { cols: 1, ho: 0.22, vo: [11.4]
        , boxes: []
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

  function getDeedType(deed: string) : CardTy {
    for (const ty in Object.keys(dims)) {
      const cards = dims[ty].cards
      for (let i = 0; i < cards.length; ++i) {
        if (cards[i] === deed) return ty as CardTy
      }
    }
    throw new Error("I don't know deed: " + deed)
  }

  function drawDeed(deed: string) : DeedUI {
    const [r,c]   = cardLoc[deed]
    const cardTy  = getDeedType(deed)
    const info    = dims[cardTy]

    function small() {

      const dom = uiFromTemplate("card-small")
      dom.style.setProperty("--background-columns",String(info.cols))

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

    function big() {
      const dom = uiFromTemplate("card-big")
      dom.classList.add(cardTy)
      dom.style.setProperty("--background-columns",String(info.cols))
      dom.style.backgroundPosition =
        (-c * constant.cardWidth)  + "px " +
        (-r * constant.cardHeight) + "px"

      return dom
    }

    function selectorBoxes(dom: HTMLElement): SelectorBoxes {
      const boxes = info.boxes

      const bs: HTMLElement[] = []
      for (let i = 0; i < boxes.length; ++i) {
        const [boxY,boxH] = boxes[i]
        const box = uiFromTemplate("card-big-selector")
        box.style.setProperty("--mk-box-height", String(boxH))
        box.style.setProperty("--mk-box-top", String(boxY))
        bs[i] = box
        dom.appendChild(box)
      }

      return {
        get:  (i: number) => bs[i],
        hide: (i: number) => bs[i].classList.add("hidden"),
        show: (i: number) => bs[i].classList.remove("hidden"),
      }
    }

    return {
      name: deed,
      small: small,
      big: big,
      selectors: selectorBoxes
    }
    
  }

  return {
    getName: (d: DeedUI) => d.name, 
    drawDeed: drawDeed
  }
}
