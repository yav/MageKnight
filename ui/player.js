function drawPlayer(player) {

  // XXX: units
  // XXX: cards
  // XXX: crystals

  var img = $('<img/>')
            .attr('src', charUrl(player.name, 'art'))
            .css('width','5em')
            .css('height','5em')

  var rep = drawReputation()
  var fam = drawFame()
  var l   = drawLevel()

  var stats = $('<table/>')
              .css('background-color', '#303')
              .css('border', '2px solid black')
              .append($('<tr/>')
                      .append($('<td/>').attr('rowspan','2').append(img))
                      .append($('<td/>').attr('rowspan','2').append(l))
                      .append($('<td/>').append(rep)))
              .append($('<tr/>').append($('<td/>').append(fam)))

  return stats;

  function imgUrl(x)        { return '/img/' + x + '.png'; }
  function levelUrl(which)  { return imgUrl('level/' + which); }
  function charUrl(ch,ty)   { return imgUrl('characters/' + ch + '/' + ty); }

  function drawLevel() {
    var lvl = player.fameInfo.level
    var nm  = ((lvl % 2) === 0) ? ((lvl - 1) + '_' + lvl)
                                : (lvl + '_' + (lvl + 1))
    return $('<img/>')
           .attr('src', levelUrl(nm))
           .css('height', '5em')
           .css('width', '5em')
  }

  function drawFame() {

    var finfo     = player.fameInfo
    var cur       = finfo.fame
    var lvlStart  = finfo.start
    var lvlEnd    = finfo.end
    var lvl       = finfo.level + 1
    var lvlReward = ((lvl % 2) === 0) ? 'up_skill' : 'up_unit'


    var me = $('<div/>')
             .css('position', 'relative')
             .css('font-family', 'Almendra')
             .css('text-align', 'center')
             .css('color', 'white')
             .css('background-color', 'black')
             .text(cur + ' / ' + lvlEnd);

    var done = $('<div/>')
               .css('position', 'absolute')
               .css('left', '0')
               .css('top', '0')
               .css('background-color', 'rgba(255,0,255,0.5)')
               .css('width', 100 * (1 + cur - lvlStart)
                           / (lvlEnd - lvlStart + 1) + '%')
               .css('height', '100%');

    var award = $('<img/>')
                .attr('src', levelUrl(lvlReward))
                .css('position', 'absolute')
                .css('right', '0');

    if (lvlReward === 'up_skill')
      award.css('top','-1ex').css('height', '5ex').css('width', '1.5em');
    else
      award.css('top','-0.2em')
           .css('height', '1.5em').css('width', '1.5em');

    return me.append([done,award]);
  }


  function drawReputation() {
    player.reputation = 0;

    var inf = [ 'x', '-5', '-3', '-2', '-1', '-1', '0', '0', '0'
              , '+1', '+1', '+2', '+2', '+3', '+5' ];

    var me = $('<div/>')
             .css('font-family', 'Almendra')
             .css('text-align', 'center')
             .css('width', 2 * inf.length + 'em')
             .css('background-image', 'linear-gradient(to right,#900,#ff9,#fff)')
             .css('border', '1px solid black');

    jQuery.each(inf, function(ix,i) {
      var d = $('<div/>')
              .css('display','inline-block')
              .css('margin', '0.5em')
              .text(i);
      if (ix - 7 === player.reputation)
          d.css('padding', '0.5em')
           .css('background-color', 'rgba(0,0,0,0.3)')
           .css('border-radius', '1em');

      me.append(d);
    });

    return me
  }


}


