function Interactions() {

  var is =
    { magicGlade: function(opts) {
        var box = $('<div/>')
                  .css('font-family', 'Almendra')
                  .css('background-color', 'white')
                  .css('border', '2xp solid black')

      }
    }

  return is

  /* Private */

  function multipleChoice(question, answers) {
    var box = $('<div/>')
              .css('font-family', 'Almendra')
              .css('background-color', 'white')
              .css('border', '2xp solid black')
              .css('position', 'fixed')
              .css('z-index', '5')

    var quest = $('<div/>').text(question)
    var ans   = $('<ol/>')

    jQuery.each(answers, function(ix,answer) {
      var item = $('<li/>')
                 .text(answer)
                 .css('cursor', 'pointer')
      ans.append(item)
    })

    return box.append([quest,ans])
  }


}
