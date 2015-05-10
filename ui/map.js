function drawLand(land, p, lastSafe) {
  var topDom = $('<div/>')
               .attr('id','land')
               .css('position', 'relative')
               .css('font-family', 'Almendra')

  var map  = drawMap(land.map, p, lastSafe)

  var next = $('<div/>')
             .css('left', '5px')
             .css('top', '5px')
             .css('position', 'absolute')
             .css('top', '0px')
             .css('left', '0px')
             .css('z-index','1')
             .css('background-color', 'rgba(0,0,0,0.5)')

  jQuery.each(land.nextTiles, function(ix,ty) {
    var t = $('<img/>')
            .width('32px')
            .height('32px')
            .css('display', 'inline-block')
            .attr('src', tileUrl(ty,'back'))
    next.append(t)
  })

  topDom.append(next,map.dom)

  return { dom: topDom, focus: map.focus }



function imgUrl(x)        { return '/img/' + x + '.png' }
function tileUrl(ty,n )   { return imgUrl('maps/' + ty + '/' + n) }
function charUrl(ch,ty)   { return imgUrl('characters/' + ch + '/' + ty) }
function enemyUrl(ty,ch)  { return imgUrl('enemies/' + ty + '/' + ch) }
function ruinsUrl(name)   { return imgUrl('ruins/' + name) }
function timeUrl(when)    { return imgUrl('time/' + when) }


function boardToken(wi,url) {
  return $('<img/>')
         .css('position', 'absolute')
         .css('width', wi + 'px')
         .css('height', wi + 'px')
         .attr('src', url)
}

function characterToken(m,ch) {
  return boardToken(m.charWidth, charUrl(ch,'figure')).css('z-index', '3')
}

function shieldToken(m,ch) {
  return boardToken(m.shieldWidth, charUrl(ch,'shield'))
}

function enemyToken(m,ty,ch) {
  return boardToken(m.enemyWidth, enemyUrl(ty,ch)).css('z-index', '2')
}

function ruinsToken(m,name) { return boardToken(m.enemyWidth, ruinsUrl(name)) }



// Compute various dimensions, for the given maximum width and height
// (in hex-grid coordinates)
function prepareMap(pMaxX, pMaxY, pTileWidth) {

  var tileWidth    = pTileWidth
  var hexWidth     = tileWidth / 3
  var enemyWidth   = 3 * hexWidth / 4
  var charWidth    = hexWidth / 1.5
  var shieldWidth  = hexWidth / 3

  var centerToSide = hexWidth / 2
  var hexSide      = centerToSide / Math.sin(Math.PI/3)
  var hexHeight    = 2 * hexSide

  var maxX         = pMaxX
  var maxY         = pMaxY
  var x_off        = maxY * centerToSide
  var y_off        = maxX * 3 * hexSide  + maxY * 4.5 * hexSide

  return { tileWidth    : tileWidth
         , hexWidth     : hexWidth

         , enemyWidth   : enemyWidth
         , charWidth    : charWidth
         , shieldWidth  : shieldWidth

         , centerToSide : centerToSide
         , hexSide      : hexSide
         , hexHeight    : hexHeight

         , maxX         : maxX
         , maxY         : maxY
         , x_off        : x_off
         , y_off        : y_off
         }
}


// Convert from tile-grid coordinates, to screen coordinates.
function tilePos(m,x,y) {
  return { left: m.x_off + x * 2 * m.hexWidth - y * m.centerToSide
         , top:  m.y_off - x * 3 * m.hexSide  - y * 4.5 * m.hexSide
         }
}

// Compute the location of an individual hex in a tile.
function hexLocation(m, x, y, dir) {
  var coords = tilePos(m,x,y)
  var dX, dY
  switch(dir) {
    case 'NW': dX = 1; dY = 0; break
    case 'NE': dX = 3; dY = 0; break
    case 'W' : dX = 0; dY = 1; break
    case 'C' : dX = 2; dY = 1; break
    case 'E' : dX = 4; dY = 1; break
    case 'SW': dX = 1; dY = 2; break
    case 'SE': dX = 3; dY = 2; break
  }
  return { top:  coords.top  + dY * 1.5 * m.hexSide
         , left: coords.left + dX * m.centerToSide
         }
}

// Place some items on the given hex.
//   - `lower` indicates that the items should be be placed at the
//      bottom of the hex (e.g., shields)
//   - the `items` are a collection of DOM elements.
function positionItems(m, items, x, y, dir, lower) {
  var c = hexLocation(m,x,y,dir)
  var hexCenter = c.left + m.hexWidth / 2

  var totWidth = 0
  jQuery.each(items,function(ix,item) { totWidth += item.width() })

  var maxW = 1.5 * m.hexWidth
  var scaling = 1
  if (totWidth > maxW) { scaling = maxW / totWidth; totWidth = maxW }

  var left = hexCenter - totWidth / 2

  jQuery.each(items,function(ix,item) {
    var h = lower ? (3/4) : (1/2)
    item.css('left', left)
        .css('top',  c.top + h * (m.hexHeight - item.height()))
    left += item.width() * scaling
  })

}

// Draw a tile on the map.
//   - a `tile` is of type: { x, y: tile-grid coordinates
//                          , tile: { name, type }
//                          , content: [ content ]
//                          }
//
//  XXX: Currnetly, this code assumes that we are drawing a "wedge" type map.
//  The assumption ahs to do with drawing the "ocean" around the map's edges.
function drawTile(m, t) {

  var x      = t.x
  var y      = t.y
  var coords = tilePos(m,x,y)
  var left   = coords.left
  var top    = coords.top

  var img = $('<img/>').css('position','absolute')
                       .width(m.tileWidth)
                       .height(m.tileWidth * 0.96)
                       .css('left', left)
                       .css('top',top)

  var imgs = [ img ]

  img.attr('src',tileUrl(t.tile.tile.type,t.tile.tile.name))

  jQuery.each(t.tile.content, function(ix,c) {
    var deco = drawTileContent(m,x,y,c.location,c.content)
    jQuery.each(deco, function(ix,d) { imgs.push(d); })
  })

  jQuery.each(['NW','NE','E','SE','SW','W','C'], function(ix,dir) {
    imgs.push(drawHexShadow(m,x,y,dir))
  })

  return imgs
}


// Draw the various things on a hex in tile.
function drawTileContent(m,x,y,dir,c) {
  var items = []

  if (c.ruins !== null) {
    items.push(ruinsToken(m,c.ruins))
  }

  jQuery.each (c.players, function(ix,player) {
    items.push (characterToken(m,player))
  })

  jQuery.each (c.enemies, function(ix,enemy) {
    items.push (enemyToken(m, enemy.type, enemy.name))
  })

  positionItems(m, items, x, y, dir, false)

  var shields = []
  jQuery.each (c.shields, function(ix,player) {
    var s = shieldToken(m,player)
    shields.push(s)
    items.push(s)
  })

  positionItems(m, shields, x, y, dir, true)

  return items
}


function drawHexShadow(m,x,y,dir) {
  var l = hexLocation(m, x, y, dir)
  var poly = 'polygon(50% 0, 100% 25%, 100% 75%, 50% 100%, 0 75%, 0 25%)'
  var img = $('<div/>')
          .css('width',  m.hexWidth + 'px')
          .css('height', m.hexHeight + 'px')
          .css('position', 'absolute')
          .css('z-index', '4')
          .css('left', l.left)
          .css('top', l.top)
          .css('background-color', 'rgba(255,255,255,0)')
          .css('-webkit-clip-path', poly)
          .css('clip-path', poly)


  img.click(function() {

    jQuery.post('/move',  { tile_x: x, tile_y: y, hex: dir }, redrawGame)
    return

    // Ask for help about a location
    jQuery.post('/mapHelpUrl',  { tile_x: x, tile_y: y, hex: dir },
      function(info) {
        if (info.helpUrl === null) return

        var dom = $('<img/>')
                  .attr('src', info.helpUrl)
                  .css('position', 'absolute')
                  .css('right',    '1em')
                  .css('bottom',   '1em')
                  .css('z-index',  '10')
        dom.click(function () { dom.remove() })
        topDom.append(dom)
      })
    return

    // Render a menu for what to do
    jQuery.post('/click',  { tile_x: x, tile_y: y, hex: dir },
      function(r) {
        switch (r.tag) {
          case 'menu':
            var menu = $('<div/>')
                       .css('background-color', 'white')
                       .css('border', '2px solid black')
                       .css('position', 'absolute')
                       .css('z-index', '5')
                       .css('left', l.left)
                       .css('top', l.top)

            jQuery.each(r.items, function(ix,lab) {
              var item = $('<div/>')
                         .text(lab)
                         .css('padding', '0.5em')
              item.hover( function () { item.css('background-color', '#ccc') }
                        , function () { item.css('background-color', '#fff') }
                        )

                   .click(function() {
                           console.log('I choose ' + ix)
                           menu.remove()
                         })
              menu.append(item)
            })
            img.parent().append(menu)
            break
        }
      })
  })
  .hover( function() { img.css('background-color', 'rgba(255,255,255,0.5)') }
        , function() { img.css('background-color', 'rgba(255,255,255,0)') }
        )

  return img
}

function drawMap(map, p, lastSafe) {

  var div = $('<div/>')
            .css('position', 'relative')
            .css('overflow', 'auto')
            .css('background-color', 'black')

  var maxX = 0;
  var maxY = 0;

  jQuery.each(map, function(ix,tile) {
    if (tile.x > maxX) maxX = tile.x;
    if (tile.y > maxY) maxY = tile.y;
  });

  var m = prepareMap(maxX,maxY,200);
  div.css('height', $(window).height() - 30);

  jQuery.each(map, function(ix,tile) {
    div.append(drawTile(m,tile));
  });

  if (lastSafe !== null) {
    var x = lastSafe.x;
    var y = lastSafe.y;
    var dir = lastSafe.hex;
    var deco = characterToken(m, p)
               .css('opacity', '0.75');
    positionItems(m, [deco], x, y, dir, false);
    div.append(deco);
  }

  return { dom: div, focus: function(x,y) { focusMap(m,div,x,y); } };
}


function focusMap(m,map,x,y) {
  var c = tilePos(m,x,y);
  var w = map.width();
  var h = map.height();
  map.scrollTop(c.top   - (h - m.tileWidth) / 2)
     .scrollLeft(c.left - (w - m.tileWidth) / 2);
}

}
