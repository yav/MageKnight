function drawPlayerCards(player) {

  function deedUrl(deed)    { return '/deed/' + deed.name }
  function imgUrl(x)        { return '/img/' + x + '.png' }
  function cardIconUrl(nm)  { return imgUrl('cards/icons/' + nm) }

  var topDom = $('<div/>')
             .attr('id', 'playerCards')
             .css('padding', '0')
             .css('margin',  '0')
             .css('position', 'relative')

  jQuery.each(player.cards, function(ix,cardName) {

    var c = deedCardImage(cardName)

    var playMenu = $('<div/>')
                   .css('text-align', 'center')
                   .css('background-color', 'rgba(0,0,0,0.5)')
                   .css('border-top-left-radius', '5px')
                   .css('border-top-right-radius', '5px')
                   .css('color', 'white')

    var sidewaysMenu =
        $('<div/>')
        .css('text-align','right')
        .css('background-color', 'rgba(0,0,0,0.5)')
        .css('border-top-right-radius', '5px')
        .css('border-top-left-radius', '5px')
        .css('color', 'white')
        .hide();

    if (cardName.type === 'wound') {
      playMenu.append(
        $('<img/>')
        .attr('src', cardIconUrl('heal'))
        .css('cursor', 'pointer')
        .css('margin', '2px')
        .css('width','16px')
        .css('height','18px')
        .click(function(ev) {
          jQuery.post('/healPlayerWound', {}, redrawGame)
          return false
        }))
    } else {
      playMenu.append(
       textButton('&#x21b7;', function(ev) {
            playMenu.hide()
            sidewaysMenu.show()
            c.css('transform', 'rotate(90deg)')
            c.selectCard()
            return false
          })
          .css('position', 'absolute')
          .css('right', '0')
        )
        .append (
          textButton('&#x27a1;', function(ev) {
            jQuery.post('/playCard', { card: ix }, redrawGame)
            return false;
          })
        )
    }

    sidewaysMenu.append(
      textButton('&#x21b6;', function(ev) {
        sidewaysMenu.hide()
        playMenu.show()
        c.css('transform', 'rotate(0deg)')
        return false
      }))

    sideButton('move')
    sideButton('influence')
    sideButton('attack')
    sideButton('block')

    c.append(playMenu)
     .append(sidewaysMenu)

    topDom.append(c)

    function sideButton(name) {
       var sideways = 0;
       var btn = $('<img/>')
                 .attr('src', cardIconUrl(name))
                 .css('cursor', 'pointer')
                 .css('margin', '2px')
                 .css('width','16px')
                 .css('height','18px')
                 .css('transform', 'rotate(-90deg)')
                 .click(function(ev) {
                   jQuery.post('/playCardFor', { card: ix, action: name },
                                redrawGame)
                   return false
                 })
       sidewaysMenu.append(btn)
    }

    function textButton(label, click) {
      return $('<div/>')
             .html(label)
             .css('margin-right', '16px')
             .css('cursor', 'pointer')
             .css('font-family', 'Almendra')
             .css('height', '18px')
             .css('display','inline-block')
             .click(click);
    }
  })

  return topDom



  function deedCardImage(name) {

    // var sizes = [ 160, 264 ];
    var sizes = [ 200 ];
    var curW  = 0;


    var me = $('<div/>')
             .css('position', 'relative')
             .css('display', 'inline-block')
             .css('background-image', 'url("' + deedUrl(name) + '")')
             .css('margin', '5px')
             .css('border-radius', '5px')

    function setW() {
      var w = sizes[curW];
      var h = 1.5 * w;
      me.width(w).height(h);
      me.css('background-size', w + 'px ' + h + 'px')
        .css('width', w + 'px')
        .css('height', h + 'px')
    }

    function focus() {
      me.css('z-index', '2')
      me.siblings().css('z-index', '1');
    }

    me.click(function(ev) {
      if (me.css('z-index') == '1') {
        focus();
      } else {
       curW = (curW + 1) % sizes.length;
       setW();
      }
     return false;
    });

    me.selectCard = function() { focus(); }

    setW();
    return me;
  }

}
