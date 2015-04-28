function drawSource(manas) {

  var source = $('<div/>')
               .attr('id', 'source')
               .css('font-family', 'Almendra')
               .css('background-color', '#303')

  jQuery.each(manas, function(ix,mana) {
    source.append(drawDie(mana))
  })

  source.append(refreshButton())

  return source


  function imgUrl(x)        { return '/img/' + x + '.png' }
  function manaUrl(mana)    { return imgUrl('mana/' + mana) }

  function refreshButton() {
    var icon = $('<div/>')
               .html('&#x21BA;')
               .css('color', 'white')
               .css('font-size', '20px')
               .css('text-align', 'center')
               .css('position', 'absolute')
               .css('top','0')
               .css('left','2px')

    return $('<div/>')
           .append(icon)
           .css('display', 'inline-block')
           .css('position', 'relative')
           .css('cursor', 'pointer')
           .css('height', '24px')
           .css('width', '24px')
           .click(function() { jQuery.post('/refillSource', {}, redrawGame) })

  }


  function drawDie(d) {
    var me = $('<img/>')
             .attr('src', manaUrl(d))
             .css('display', 'inline-block')
             .css('margin', '2px')
             .css('width', '24px')
             .css('height', '24px')
             .css('cursor', 'pointer')
             .click(function () {
                jQuery.post('/useDie', { color: d }, redrawGame)
              })


    return me
  }

}

