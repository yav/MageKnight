function newCardSheet(name,width,height) {
  const cardWidth  = 220
  const cardHeight = 308

  const dims =
    { basic:        { width: 7000, heght: 5600, rows: 4, cols: 7, ho: 0.22, vo: [6.3] }
    , advanced:     { width: 8000, heght: 8400, rows: 6, cols: 8, ho: 0.22, vo: [6.3] }
    , units_regular:{ width: 4000, heght: 5600, rows: 4, cols: 4, ho: 0.22, vo: [6.3] }
    , units_elite:  { width: 4000, heght: 5600, rows: 4, cols: 4, ho: 0.22, vo: [6.3] }
    , artifacts:    { width: 5000, heght: 7000, rows: 5, cols: 5, ho: 0.22, vo: [6.0] }
    , spells:       { width: 6000, heght: 5600, rows: 4, cols: 6, ho: 0.24, vo: [0.3,6.5]}
    }

  const url = "img/cards/" + name + ".jpg"

  const info = dims[name]

  function newCard(r,c) {

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

  return function(w) {
    const col = html.div("card-container")
    for (let r = 0; r < info.rows; ++r)
      for (let c = 0; c < info.cols; ++c)
        col.appendChild(newCard(r,c))

    w.appendChild(col)
  }

}
