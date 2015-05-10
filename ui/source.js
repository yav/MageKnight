function drawSource(manas) {

  var source = $('<div/>')
               .attr('id', 'source')
               .css('font-family', 'Almendra')
               .css('width', Math.ceil(manas.length / 2) * 30)


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
           .css('margin', '3px')
           .css('width',  '24px')
           .css('height', '24px')
           .css('cursor', 'pointer')
  }
}

