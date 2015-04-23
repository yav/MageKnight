function drawPlayArea(playArea) {

  var topDom = $('<div/>')
               .attr('id', 'playArea')

  jQuery.each(playArea.cards, function(ix,card) {
    topDom.append(drawActiveCard(card))
  })

  return topDom

  function deedUrl(nm) { return '/deed/' + nm }

  function drawActiveCard(card) {
    var name = card.card
    var w    = 200
    var h    = 256

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
      me.css('transform', 'rotate(90deg)').text(card.useFor)
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
