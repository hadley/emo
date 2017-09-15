$(function(){

  var emoji_clicked = function(){
    Shiny.onInputChange( "selected_aliases", $(this).data("emoji") ) ;
  } ;

  var emoji_refresh = function(data){

    var emojis = data.emojis ;
    var aliases = data.aliases ;

    $emojis = $("#emojis") ;
    $emojis.empty() ;

    for( i=0; i<emojis.length; i++){
      var $link = $("<a class='action-button emoji-link' href='#' data-emoji="+aliases[i]+" id='"+emojis[i]+"'>" + emojis[i] + "</a>" ) ;
      $link.click(emoji_clicked) ;
      $emojis.append( $link ) ;
    }

  } ;

  Shiny.addCustomMessageHandler( "emoji_refresh", emoji_refresh );

}) ;
