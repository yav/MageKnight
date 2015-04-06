function newOffers(offers, el, v) {

  var useElite = el;
  var vis = v;

  var topDom = $('<div/>')
               .css('font-family', 'Almendra')
               .css('background-color', '#303')
               .css('padding', '1em')

  var labels = { 'advancedActions': 'Advanced Actions'
               , 'spells':          'Spells'
               , 'units':           'Units'
               , 'monasteries':     'Monasteries'
               }

  topDom.append (drawRefresh());

  jQuery.each(offers, function(offer, cards) {
    topDom.append(drawOffer(offer,cards))
  })

  // Draw artifact
  return topDom

  function deedUrl(name) { return '/deed/' + name }
  function unitUrl(name) { return '/unit/' + name }

  function drawRefresh() {
    var dom = $('<table/>')
              .css('background-color', 'rgba(255,255,255,0.5)')
              .css('border-collapse', 'collapse')
              .css('border', '2px solid black')
    var row = $('<row/>')
    dom.append(row)

    row.append( $('<td/>')
                .text('Refresh')
                .css('vertical-align', 'middle')
                .css('cursor', 'pointer')
                .click(function() {
                  jQuery.post('/refreshOffers', { elite: false },
                    function(o1) {
                      topDom.replaceWith(newOffers(o1, useElite, vis))
                    })
                })
    )

    function url() {
      return '/img/units/' + (useElite ? 'elite' : 'regular') + '/back.png'
    }

    var unitTypeButton =
      $('<img/>')
      .css('width', '32')
      .css('width', '48px')
      .css('cursor', 'pointer')
      .attr('src', url())
      .click(function () {
        useElite = !useElite
        unitTypeButton.attr('src', url())
      })

    row.append($('<td/>').css('line-height', '0').append(unitTypeButton))
    return dom
  }


  function drawOffer(name,cards) {
    var dom = $('<div/>')
              .css('margin-top',      '1em')
              .css('background-color', 'rgba(255,255,255,0.8)')
              .css('border-bottom-left-radius', '1em')
              .css('border-bottom-right-radius', '1em')
              .css('border', '2px solid black')

    if (vis[name] === undefined) vis[name] = true;

    dom.append($('<div/>')
               .text(labels[name])
               .css('color', 'black')
               .css('padding-left', '1em')
               .css('background-color', '#c93')
               .css('cursor', 'pointer')
               .click(function() {
                  cs = dom.find('img');
                  vis[name] = !vis[name];
                  if (vis[name]) cs.slideDown(); else cs.slideUp()
                })
               )

    jQuery.each(cards, function(ix,card) { dom.append(drawCard(ix,card)) })

    if (!vis[name]) dom.find('img').hide();

    return dom

    function drawCard(ix, card) {
      var dom = $('<img/>')
                .attr('src', name === 'units' ? unitUrl(card) : deedUrl(card))
                .css('width',  '200px')
                .css('height', '256')
                .css('margin', '1em')
                .css('box-shadow', '5px 2px 5px 2px #333')
                .click(function () {
                  var f = function() {}
                  jQuery.post('/takeOffered', { offer: name, card: ix }
                             , function(newOs) {
                                it = newOffers(newOs, useElite, vis)
                                f = function() { topDom.replaceWith(it) }
                             })
                  dom.fadeOut('slow', function() { f(); });

                })
      return dom
    }

  }

}
