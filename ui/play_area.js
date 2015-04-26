function drawPlayArea(playArea) {

  var w    = 200
  var h    = 256

  var topDom = $('<div/>')
               .attr('id', 'playArea')
               .css('font-family', 'Almendra')

  jQuery.each(playArea.cards, function(ix,card) {
    topDom.append(drawActiveCard(card))
  })

  return topDom

  function deedUrl(nm)      { return '/deed/' + nm }
  function imgUrl(x)        { return '/img/' + x + '.png' }
  function cardIconUrl(nm)  { return imgUrl('cards/icons/' + nm) }

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
               .css('width','16px')
               .css('height','18px')

    var lab = $('<div/>')
              .css('background-color', 'rgba(0,0,0,0.8)')
              .css('color', 'white')
              .css('position',' absolute')
              .css('width',  h + 'px')
              .css('height', w + 'px')
              .css('left', '0')
              .css('top',  '0')
              .css('text-align', 'center')
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
      var pos = card.poweredUp ? (7/12) : (9/11);
      var screen = $('<div/>')
                   .width(w + 'px')
                   .height((h / 7) + 'px')
                   .css('position','absolute')
                   .css('left','0px')
                   .css('top', (pos * h) + 'px')
                   .css('background-color', 'rgba(0,0,0,0.75)')
      me.append(screen)
    }

    return me
  }

}
