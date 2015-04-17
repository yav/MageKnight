function drawPlayerStats(player) {

  var img = $('<img/>')
            .attr('src', charUrl(player.name, 'art'))
            .css('width','5em')
            .css('height','5em')

  var rep = drawReputation()
  var fam = drawFame()
  var l   = drawLevel()
  var cs  = drawCrystals(player.crystals)
  var ds  = drawDeedDeck()


  var stats = $('<table/>')
              .attr('id','playerStats')
              .css('font-family', 'Almendra')
              .css('background-color', '#303')
              .css('border', '2px solid black')
              .append($('<tr/>')
                      .append($('<td/>').attr('rowspan','2').append(img))
                      .append($('<td/>').attr('rowspan','2').append(l))
                      .append($('<td/>').append(rep))
                      .append($('<td/>').attr('rowspan','2').append(cs))
                      .append($('<td/>').attr('rowspan','2').append(ds))
                      )
              .append($('<tr/>').append($('<td/>').append(fam)))


  return stats

  function imgUrl(x)        { return '/img/' + x + '.png' }
  function levelUrl(which)  { return imgUrl('level/' + which) }
  function charUrl(ch,ty)   { return imgUrl('characters/' + ch + '/' + ty) }
  function crystalUrl(c,n)  { return imgUrl('mana/crystal/' + c + '/' + n) }

  function drawLevel() {
    var lvl      = player.fameInfo.level
    if (lvl > 10) lvl = 10
    var sndLevel = (lvl % 2) === 0

    var nm = sndLevel ? ((lvl - 1) + '_' + lvl)
                      : (lvl + '_' + (lvl + 1))


    var pos = [ '2.1em', '1em'
              , '2.1em', '1em'
              , '2.3em', '1.1em'
              , '2.3em', '1.1em'
              , '2.1em', '1em'
              ]

    return $('<div/>')
            .css('position', 'relative')
            .append( [ $('<img/>')
                         .attr('src', levelUrl(nm))
                         .css('height', '5em')
                         .css('width', '5em')
                      , $('<div/>')
                         .css('background-color', 'rgba(0,0,0,0.7)')
                         .css('border-radius', '5px')
                         .css('position', 'absolute')
                         .css('top', '3.9em')
                         .css('left', pos[lvl-1])
                         .css('width',  '1.5em')
                         .css('height', '0.8em')
                      ]
                     )
  }

  function drawFame() {
    var finfo     = player.fameInfo
    var cur       = finfo.fame
    var lvlStart  = finfo.start
    var lvlEnd    = finfo.end
    var lvl       = finfo.level + 1
    var lvlReward = ((lvl % 2) === 1) ? 'up_skill' : 'up_unit'

    var fameUpdateAmt     = 0
    var fameUpdateAmtCell =
      $('<td/>')
      .attr('rowspan', '2')
      .css('color','white')
      .css('cursor', 'pointer')

    fameUpdateAmtCell
      .hover( function() { fameUpdateAmtCell.css('background-color', '#c96') }
            , function() { fameUpdateAmtCell
                            .css('background-color', 'transparent') })
      .click(function () {
        jQuery.post('/updateFame'
                   , { amount: Math.abs(fameUpdateAmt)
                     , increase: fameUpdateAmt >= 0
                     }
                   , function(p) {
                       stats.replaceWith(drawPlayerStats(p))
                       fameUpdateAmt = 0
                     })
      })



    var meter = $('<div/>')
                .css('text-align', 'center')
                .css('position',   'relative')
                .css('color', 'white')
                .css('background-color', '#606')
                .text(cur + ' / ' + lvlEnd)
                .append( $('<div/>')
                         .css('position', 'absolute')
                         .css('left', '0')
                         .css('top', '0')
                         .css('background-color', 'rgba(255,255,255,0.5)')
                         .css('width', 100 * (1 + cur - lvlStart)
                                     / (lvlEnd - lvlStart + 1) + '%')
                         .css('height', '100%') )

    var award = $('<img/>')
                .attr('src', levelUrl(lvlReward))
                .css('position', 'absolute')

    if (lvlReward === 'up_skill')
      award.css('top','-1ex').css('height', '5ex').css('width', '1.5em')
    else {
      award.css('top','-0.2em')
           .css('height', '1.5em').css('width', '1.5em')
           .css('cursor', 'pointer')
           .click( function() {
              console.log('award clicked')
              jQuery.post('/addUnitSlot', {}, function (p) {
                $('#playerUnits').replaceWith(drawPlayerUnits(p))
              })
           })
    }


    return $('<table/>')
            .css('width', '100%')
            .css('border-collapse', 'collapse')
            .append($('<tr/>')
                    .append($('<td/>')
                            .attr('rowspan', '2')
                            .css('position', 'relative')
                            .css('width', '6%').append(award))
                    .append($('<td/>')
                            .attr('rowspan', '2')
                            .css('width', '90%')
                            .append(meter)
                           )
                    .append(fameUpdateAmtCell)
                    .append(fameButton('&#x25b4;', 1))
                   )
            .append($('<tr/>')
                    .append(fameButton('&#x25be;', -1)))

    function fameButton(x,m) {
      var dom = $('<td/>')
             .css('line-height', '10px')
             .css('width', '3%')
             .css('color', 'rgba(255,255,255,0.7)')
             .css('cursor', 'pointer')
             .css('text-align', 'center')
             .html(x);
      dom.mouseenter ( function () { dom.css('background-color', '#c96') })
         .mouseleave ( function () { dom.css('background-color', 'transparent') })
         .click(function () {
            fameUpdateAmt += m
            fameUpdateAmtCell.text(fameUpdateAmt >= 0 ? ('+' + fameUpdateAmt)
                                                      : fameUpdateAmt)
          })
      return dom
    }



  }



  function drawReputation() {

    var inf = [ 'x', '-5', '-3', '-2', '-1', '-1', '0', '0', '0'
              , '+1', '+1', '+2', '+2', '+3', '+5' ];

    var me = $('<div/>')
             .css('text-align', 'center')
             .css('width', 2 * inf.length + 'em')
             .css('background-image','linear-gradient(to right,#900,#ff9,#fff)')
             .css('border', '1px solid black');

    jQuery.each(inf, function(ix,i) {
      var d = $('<div/>')
              .css('display','inline-block')
              .css('margin', '0.5em')
              .text(i)
              .css('cursor', 'pointer')
              .click(function () {
                jQuery.post('/setReputation', { reputation: ix }
                   , function(p) { stats.replaceWith(drawPlayerStats(p)) })
              })
      if (ix - 7 === player.reputation)
          d.css('padding', '0.5em')
           .css('background-color', 'rgba(0,0,0,0.3)')
           .css('border-radius', '1em')

      me.append(d);
    });

    return me
  }

  function drawCrystals(cs) {
    var newRow = $('<tr/>')
    var curRow = $('<tr/>')

    jQuery.each(['red','green','blue','white'], function(ix,c) {
      var cryNew = $('<td/>').css('min-height', '2em')
                             .css('height', '2em')
                             .css('padding', '0')
                             .css('line-height', '0')
      var cryAmt = $('<td/>').css('min-height', '2em')
                             .css('height', '2em')
                             .css('padding', '0')
                             .css('line-height', '0')
      var n = cs[c]
      if (n === undefined || n < 3) cryNew.append(drawCrystal(c,0))
      if (n !== undefined) cryAmt.append(drawCrystal(c,n))
      newRow.append(cryNew)
      curRow.append(cryAmt)
    })

    return $('<table/>')
           .css('background-color', 'rgba(0,0,0,0.3)')
           .append(newRow)
           .append(curRow)
  }



  function drawCrystal(c,n) {
    var dom = $('<div/>')

    return $('<img/>')
           .attr('src', crystalUrl(c,n))
           .css('height', '2em')
           .css('width', '2em')
           .css('cursor', 'pointer')
           .click(function() {
              var m = n === 0 ? '/addCrystal' : '/removeCrystal'
              jQuery.post(m, { color: c }
                   , function(p) { stats.replaceWith(drawPlayerStats(p))})
           })
    return dom
  }


  function drawDeedDeck () {
    return $('<div/>')
           .css('background-image', 'url("/img/cards/back.png")')
           .css('background-size', '64px 96px')
           .css('width',  '64px')
           .css('height', '96px')
           .css('background-repeat', 'no-repeat')
           .css('border-radius', '10px')
           .css('cursor','pointer')
           .click(function() {
              jQuery.post('/drawCard', {}, function(p) {
                $('#playerCards').replaceWith(drawPlayerCards(p))
              })
           })
  }



}



