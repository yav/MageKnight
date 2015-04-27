function drawPlayArea(playArea) {

  var w    = 200
  var h    = 256

  var topDom = $('<div/>')
               .attr('id', 'playArea')
               .css('font-family', 'Almendra')
               .css('background-color', '#303')

  topDom.append(drawMana())

  jQuery.each(playArea.cards, function(ix,card) {
    topDom.append(drawActiveCard(card))
  })

  return topDom

  function deedUrl(deed)    { return '/deed/' + deed.name }
  function imgUrl(x)        { return '/img/' + x + '.png' }
  function cardIconUrl(nm)  { return imgUrl('cards/icons/' + nm) }
  function manaUrl(mana)    { return imgUrl('mana/' + mana) }

  function drawMana() {
    var dom = $('<div/>')
              .css('background-color', 'rgba(0,0,0,0.2)')
    jQuery.each(playArea.mana, function(ix,c) {
      dom.append(drawManaToken(c))
    })
    return dom
  }

  function drawManaToken(c) {
    sh = 'black'
    switch (c) {
      case 'red':   colA = '#300'; colB = '#f00'; break;
      case 'green': colA = '#030'; colB = '#0f0'; break;
      case 'blue':  colA = '#003'; colB = '#0ff'; break;
      case 'white': colA = '#333'; colB = '#fff'; break;
      case 'gold':  colA = '#630'; colB = '#ff0'; break;
      case 'black': colA = '#666'; colB = '#000'; break;
    }

    return $('<div/>')
           .css('width',  '2em')
           .css('height', '2em')
           .css('display', 'inline-block')
           .css('background', 'linear-gradient(' + colB + ',' + colA + ')')
           .css('border-radius', '1em')
           .css('box-shadow', 'inset 0px 0px 2px 2px ' + sh)
           .css('margin','2px')

    return $('<img/>')
           .attr('src', manaUrl(c))
           .css('width',  '2em')
           .css('height', '2em')
           .css('margin', '1px')
  }

  function drawSideCard(name,act) {
    var me = $('<div/>')
             .css('margin', '1em')
             .css('position', 'relative')
             .css('display', 'inline-block')
             .css('width',  h + 'px')   // sideways
             .css('height', w + 'px')

    var pic = $('<div/>')
              .css('position', 'absolute')
              .css('transform-origin', '0 0')
              .css('left', '256px')
              .css('top',  '0px')
              .css('transform', 'rotate(90deg)')
              .css('width',  w + 'px')
              .css('height', h + 'px')
              .css('background-image', 'url("' + deedUrl(name) + '")')
              .css('background-size', w + 'px ' + h + 'px')
              .css('background-repeat', 'no-repeat')
              .css('border-radius', '0.5em')

    var icon = $('<img/>')
               .attr('src', cardIconUrl(act))
               .css('margin', '2px')
               .css('width',  '32px')
               .css('height', '40px')
               .css('position', 'absolute')
               .css('left', (h - 32)/2 + 'px')
               .css('top',  (w - 40)/2 + 'px')

    var lab = $('<div/>')
              .css('background-color', 'rgba(0,0,0,0.8)')
              .css('color', 'white')
              .css('position',' absolute')
              .css('width',  h + 'px')
              .css('height', w + 'px')
              .css('left', '0')
              .css('top',  '0')
              .css('border-radius', '0.5em')

    me.append(pic).append(lab.append(icon))
    return me

  }

  function drawActiveCard(card) {
    var name = card.card

    var me = $('<div/>')
             .css('margin', '1em')
             .css('position', 'relative')
             .css('display', 'inline-block')
             .css('width',  w + 'px')
             .css('height', h + 'px')
             .css('background-image', 'url("' + deedUrl(name) + '")')
             .css('background-size', w + 'px ' + h + 'px')
             .css('background-repeat', 'no-repeat')
             .css('border-radius', '0.5em')

    if (card.useFor !== undefined) {
      return drawSideCard(name,card.useFor)
    } else {
      // XXX: Wound

      var pos, screen_h, screen_top;

      switch (name.type) {
        case 'action':
          screen_h = h / 7;
          pos = card.poweredUp ? (7/12) : (9/11);
          break;
        case 'spell':
          screen_h = h / 2;
          pos = card.poweredUp ? 0 : (1/2)
          break;
        case 'artifact': // XXX
          screen_h = h / 7;
          pos = card.poweredUp ? (7/12) : (9/11);
          break;
      }

      var screen = $('<div/>')
                   .width(w + 'px')
                   .height(screen_h + 'px')
                   .css('position','absolute')
                   .css('left','0px')
                   .css('top', (pos * h) + 'px')
                   .css('background-color', 'rgba(0,0,0,0.75)')
      me.append(screen)
    }

    return me
  }

}
