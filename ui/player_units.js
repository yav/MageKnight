
function drawPlayerUnits(player) {

  var topDom = $('<div/>')
               .attr('id', 'playerUnits')
               .css('font-family', 'Almendra')

  jQuery.each(player.units, function(ix,mbUnit) {
    topDom.append(drawUnitSlot(ix,mbUnit))
  })

  return topDom

  function drawUnitSlot(ix,mbUnit) {
    var slot = $('<div/>')
               .css('border', '2px solid black')
               .css('border-radius', '1em')
               .css('min-width', '200px')
               .css('min-height','256px')
               .css('background-color', '#303')
               .css('margin', '1em')
               .css('padding', '20px')
               .css('display', 'inline-block')


    if (mbUnit === null) return slot

    var card = $('<div/>')
               .css('position', 'relative')
               .css('background-image', 'url("/unit/' + mbUnit.unit + '")')
               .css('background-size', '200px 256px')
               .css('background-repeat', 'no-repeat')
               .css('border-radius', '10px')
               .css('width', '160px')
               .css('height','256px')
               .css('cursor', 'pointer')
               .css('padding-left','40px')
               .click(function() {
                  jQuery.post('/unitToggleReady', { unit: ix },
                    function(p) { topDom.replaceWith(drawPlayerUnits(p)) })
               })

    if (!mbUnit.ready)
      card.append(drawExhausted())

    card.append(disbandButton())

    if (mbUnit.wounds < 2)
      card.append(addWoundButton())

    for (var i = 0; i < mbUnit.wounds; ++i) {
      card.append(drawWound())
    }

    return slot.append(card)

    function disbandButton() {
      return $('<div/>')
             .html('&#x274c;')
             .css('position', 'absolute')
             .css('right',  '5px')
             .css('bottom', '8px')
             .css('color', '#c00')
             .css('padding','2px')
             .css('font-size', '20px')
             .css('border-radius', '5px')
             .css('pointer', 'cursor')
             .css('margin-left', '5px')
             .click(function () {
                jQuery.post('/disbandUnit', { unit: ix },
                  function (p) { topDom.replaceWith(drawPlayerUnits(p)) })
                return false
             })
    }

    function addWoundButton() {
      return  $('<img/>')
             .attr('src','/img/cards/wound.png')
             .css('position', 'absolute')
             .css('left',   '5px')
             .css('bottom', '8px')
             .css('width',  '24px')
             .css('border-radius','3px')
             .css('cursor', 'pointer')
             .css('float','right')
             .click(function() {
                jQuery.post('/woundUnit', { unit: ix }, function(p1) {
                  topDom.replaceWith(drawPlayerUnits(p1))
                })
                return false
             })

   }

    function drawWound() {
      return $('<div/>')
             .css('background-image', 'url("/img/cards/wound.png")')
             .css('background-size', '64px 96px')
             .css('width', '64px')
             .css('height', '96px')
             .css('border-radius', '5px')
             .css('float','left')
             .css('background-repeat', 'no-repeat')
             .css('margin-top',   '0.2em')
             .css('margin-right', '0.2em')
             .css('cursor', 'pointer')
             .click(function() {
                jQuery.post('/healUnit', { unit: ix }, function(p1) {
                  topDom.replaceWith(drawPlayerUnits(p1))
                })
                return false
             })
    }

    function drawExhausted() {
      return $('<div/>')
             .css('color','white')
             .css('background-color', 'rgba(0,0,0,0.7)')
             .css('position', 'absolute')
             .css('left','0px')
             .css('top','150px')
             .css('width','200px')
             .css('height','100px')
    }

  }

}


