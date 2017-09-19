$(function(){

  var emoji_clicked = function(){
    Shiny.onInputChange( "selected_emoji", $(this).attr("id") ) ;
  } ;

  var emoji_dblclicked = function(){
    Shiny.onInputChange( "emoji_dblclicked", $(this).attr("id") ) ;
  } ;

  var emoji_refresh = function(data){

    var emojis = data.emojis ;
    var aliases = data.aliases ;

    $emojis = $("#emojis") ;
    $emojis.empty() ;

    for( i=0; i<emojis.length; i++){
      var $link = $("<a class='action-button emoji-link' href='#' id='"+emojis[i]+"'>" + emojis[i] + "</a>" ) ;
      $link.click(emoji_clicked) ;
      $link.dblclick(emoji_dblclicked) ;
      $emojis.append( $link ) ;
    }

  } ;

  Shiny.addCustomMessageHandler( "emoji_refresh", emoji_refresh );

}) ;
