function drawSource(manas) {
  var source = $('<div/>')
  jQuery.each(manas, function(ix,mana) {
    var img = $('<img/>')
              .attr('src', manaUrl(mana))
              .css('display', 'inline-block')
              .css('box-shadow', '2px 2px 5px #ccc')
              .css('margin', '2px')
              .width('20px')
    source.append(img)
  })
  return source


  function imgUrl(x)        { return '/img/' + x + '.png' }
  function manaUrl(mana)    { return imgUrl('mana/' + mana) }
}

