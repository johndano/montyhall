#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'  The user selects a door number.
#'  
#' @description
#'  `select_door()` generates a random number choice between 1 to 3.
#'  
#' @details
#'  The random choice is what the user picks as the door they believe contains
#'  the car and the host will always show a goat door that is not related to 
#'  the door choice the user picked.
#'  
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a random number between 1 to 3 which the user
#'  picks for the door choice.
#' 
#' @examples
#'   select_door()
#'   
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'  The host opens up a door containing a goat after the user picks a door number.
#'  
#' @description
#'  `open_goat_door(game, a.pick)` selects a door number, containing a goat, out
#'  of the remaining door numbers available after the user chose a door number.
#'  
#' @details
#'  After the user picks a door number, the host will choose one of the
#'  remaining doors only if the door contains a goat behind it. For example, if 
#'  the user chooses a door with a goat behind it, the host only has one door 
#'  option to open with a goat behind it. If the user chooses a door with a car 
#'  behind it, the host has two options left and can open either door.
#'  
#' @param ... The function takes in the arguments game and a.pick. Game refers 
#' to any object/variable which stores the values generated from create_game(). 
#' The object/variable, a.pick, refers to any object/variable that stores the 
#' value from select_door().
#'  
#' @return 
#'  The function returns a door number containing a goat behind it after it 
#'  referencing the remaining doors, the user's choice, and the position of the 
#'  doors with their corresponding value of goat or car.
#'  
#' @examples
#'   game <- create_game()
#'   a.pick <- select_door()
#'   open_goat_door(game, a.pick)
#'   
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'  The chance to change or stick with choice.
#' 
#' @description
#'  `change_door(stay, opened.door, a.pick)`The chance to change choice to the 
#'  remaining door available after revealing a goat door or sticking with the 
#'  original choice.
#'  
#' @details
#'  The function allows the user to have the option to change their choice to 
#'  the last door choice or stick with their choice after the host has revealed 
#'  a goat door.
#'  
#' @param ... the function takes in parameters: stay (logical value), 
#' opened.door (integer value), and a.pick (integer value).
#' 
#' @return 
#'  It outputs an integer value of the final pick if the user decided to stick 
#'  with their original pick or switch picks.
#'  
#' @examples
#'   When the user stays -> change_door(stay = T, opened.door, a.pick)
#'   When the user switches -> change_door(stay = F, opened.door, a.pick)
#' 
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#'  Checks to see if the user won the game.
#'  
#' @description
#'  `determine_winner(final.pick,game)` returns a "WIN" or "LOSE" depending on 
#'  the final choice.
#'  
#' @details
#'  The function validates if the user's final choice is equal to the door number
#'  which contains the car behind it.
#'  
#' @param ... The function takes in the parameters: final.pick (integer value) 
#' and game (character values).
#'  
#' @return 
#'  If the final choice corresponds to the position of the car door in game, the
#'  function will return "WIN" indicating the user won. Otherwise, the function
#'  will return "LOSE" if the final choice is not the car door.
#'  
#' @examples
#'   game <- create_game()
#'   final.pick <- change_door(stay = T, opened.door, a.pick)
#'   determine_winner(final.pick,game)
#'   
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'  Plays the Monty Hall game.
#'  
#' @description
#'  `play_game()` returns a data frame containing either "WIN" or "LOSE".
#'  
#' @details
#'  The function takes in all the previous functions generated to functionally
#'  play the Monty Hall game. It will simulate each function and output a data 
#'  frame.
#'  
#' @param ... no arguments are used by the function.
#' 
#' @return 
#'  Returns a data frame since it tests the strategy of staying and switching.
#' 
#' @examples
#'   outcome <- play_game()
#'   
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'  Plays the Monty Hall game in x amount of times.
#'  
#' @description
#'  `play_n_games(n)` returns a data frame, after x amount of iterations, of the
#'  probability of winning and losing if user decides to switch or stay.
#'  
#' @details
#'  The function iterates for x amount of times, depending on the integer input 
#'  (n), and will output a data frame into a table indicating the wins and loses
#'  for staying and switching.
#'  
#' @param ... the function takes in the variable/object, n. n represents an 
#' integer value which determines how many times the function will iterate the
#' Monty Hall game.
#' 
#' @return 
#'  It will return a data frame, after x amount of iterations, containing the 
#'  probability of winning and losing if a user decides to switch or stay.
#'  
#' @examples
#'   play_n_games(n=10)
#' 
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
