function drawPlayerCards(player) {
  var topDom = $('<div/>')
               .attr('id', 'playerCards')
               .css('font-family', 'Almendra')

  jQuery.each(player.cards, drawCard)

  return topDom


  function drawCard(ix,card) {
    var card = $('<div/>')
               .css('background-image', 'url("/deed/' + card + '")')
               .css('background-size', '200px 256px')
               .css('background-repeat', 'no-repeat')
               .css('display','inline-block')
               .css('border-radius', '10px')
               .css('width', '200px')
               .css('height','256px')
               .css('cursor', 'pointer')
               .css('margin', '1em')
               .click(function() {
                    return false
                })

      topDom.append(card)
  }

}
