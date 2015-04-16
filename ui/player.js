
function drawPlayerCards(player) {

  var topDom = $('<div/>')

  jQuery.each(player.units, function(ix,mbUnit) {
    topDom.append(drawUnitSlot(ix,mbUnit))
  })

  return topDom

  function drawUnitSlot(ix,mbUnit) {
    if (mbUnit === null)
      return $('<div/>')
             .css('border', '2px solid black')
             .css('border-radius', '1em')
             .css('width', '220px')
             .css('height','256px')
             .css('margin', '1em')
             .css('background-color', '#303')



    var dom = $('<div/>')
              .css('background-color', '#303')
              .css('padding', '1em')
              .css('display', 'inline-block')

    var card = $('<div/>')
               .css('background-image', 'url("/unit/' + mbUnit.unit + '")')
               .css('background-size', '200px 256px')
               .css('width', '200px')
               .css('height','256px')
               .css('margin', '1em')

    for (var i = 0; i < mbUnit.wounds; ++i) {
      card.append(drawWound())
    }

    return dom.append(card)
              .append(addWoundButton())

    function addWoundButton() {
      return  $('<img/>')
             .attr('src','/img/cards/wound.png')
             .css('width',  '24px')
             .css('height', '32px')
             .css('cursor', 'pointer')
             .click(function() {
                jQuery.post('/woundUnit', { unit: ix }, function(p1) {
                  topDom.replaceWith(drawPlayerCards(p1))
                })
             })
    }

    function drawWound() {
      return $('<img/>')
             .attr('src','/img/cards/wound.png')
             .css('width',  '64px')
             .css('height', '96px')
             .css('margin', '0.2em')
             .css('cursor', 'pointer')
             .click(function() {
                jQuery.post('/healUnit', { unit: ix }, function(p1) {
                  topDom.replaceWith(drawPlayerCards(p1))
                })
             })
    }




  }

}


