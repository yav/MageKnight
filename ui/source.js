function drawSource(manas, time) {

//  time = 'day'

  var bg = time === 'night' ? 'linear-gradient(to right, #936, #000)'
                            : 'linear-gradient(to right, #0cf,#036)'


  var source = $('<div/>')
               .attr('id', 'source')
               .css('font-family', 'Almendra')
               .css('background-image', bg)
               .css('border', '1px solid black')
               .css('margin', '5px')
               .css('line-height', '0')



  jQuery.each(manas, function(ix,mana) {
    source.append(drawDie(mana))
  })

  source.append(refreshButton())

  return source


  function imgUrl(x)        { return '/img/' + x + '.png' }
  function manaUrl(mana)    { return imgUrl('mana/' + mana) }

  function refreshButton() {
    return diePic('refresh')
           .click(function() { jQuery.post('/refillSource', {}, redrawGame) })

  }


  function drawDie(d) {
    return diePic(d)
           .click(function () {
              jQuery.post('/useDie', { color: d }, redrawGame)
           })
  }

  function diePic(d) {
    return $('<img/>')
           .attr('src', manaUrl(d))
           .css('display', 'inline-block')
           .css('margin', '2px')
           .css('width',  '24px')
           .css('height', '24px')
           .css('cursor', 'pointer')
  }
}

