
function drawPlayerCards(player) {

  var topDom = $('<div/>')
              .css('font-family', 'Almendra')
              .css('border', '2px solid black')

  jQuery.each(player.units, function(ix,mbUnit) {
    topDom.append(drawUnitSlot(ix,mbUnit))
  })

  topDom.append(newSlotButton())

  return topDom

  function newSlotButton() {
    return $('<div/>')
           .text('+')
           .css('font-size', '40px')
           .css('display', 'inline-block')
           .css('cursor', 'pointer')
           .css('background-color', '#303')
           .css('color', '#c93')
           .click(function() {
              jQuery.post('/addUnitSlot', {}, function(p) {
                topDom.replaceWith(drawPlayerCards(p))
              })
           })
  }

  function drawUnitSlot(ix,mbUnit) {
    if (mbUnit === null)
      return $('<div/>')
             .css('border', '2px solid black')
             .css('border-radius', '1em')
             .css('width', '220px')
             .css('height','256px')
             .css('background-color', '#303')
             .css('margin', '1em')
             .css('display', 'inline-block')

    var card = $('<div/>')
               .css('position', 'relative')
               .css('display', 'inline-block')
               .css('border', '2px solid red')
               .css('background-image', 'url("/unit/' + mbUnit.unit + '")')
               .css('background-size', '200px 256px')
               .css('background-repeat', 'no-repeat')
               .css('width', '180px')
               .css('height','256px')
               .css('padding-left','20px')
               .css('cursor', 'pointer')
               .click(function() {
                  jQuery.post('/unitToggleReady', { unit: ix },
                    function(p) { topDom.replaceWith(drawPlayerCards(p)) })
               })

    if (!mbUnit.ready)
      card.append(drawExhausted())

    card.append(disbandButton())

    if (mbUnit.wounds < 2)
      card.append(addWoundButton())

    for (var i = 0; i < mbUnit.wounds; ++i) {
      card.append(drawWound())
    }

    return card

    function disbandButton() {
      return $('<div/>')
             .html('&#x274c;')
             .css('position', 'absolute')
             .css('right',  '5px')
             .css('bottom', '8px')
             .css('display', 'inline-block')
             .css('color', '#c00')
             .css('padding','2px')
             .css('font-size', '20px')
             .css('border-radius', '5px')
             .css('pointer', 'cursor')
             .css('margin-left', '5px')
             .click(function () {
                jQuery.post('/disbandUnit', { unit: ix },
                  function (p) { topDom.replaceWith(drawPlayerCards(p)) })
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
             .css('height', '32px')
             .css('cursor', 'pointer')
             .css('float','right')
             .click(function() {
                jQuery.post('/woundUnit', { unit: ix }, function(p1) {
                  topDom.replaceWith(drawPlayerCards(p1))
                })
                return false
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


