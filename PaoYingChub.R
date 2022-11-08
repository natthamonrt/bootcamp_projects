# pao ying chub
# unlimited times to play
# when player type key to stop, show all results (win, loss and tie)
print("Hello! Welcome to Pao Ying Chub Game!")

i <- 0
result_list <- list(player = c(),
                computer = c(),
                result = c())
result_df <- NULL

items <- function(){
  item_id <- c(1,2,3)
  item_list <- c("Rock","Paper","Scissors")
  item_df <- data.frame(item_id,item_list)
  return(item_df)
}

while (i<1) {
  df1 <- items()
  cat("What are you gonna choose?",
      "[1] Rock [2] Paper [3] Scissors [99] Quit",sep = "\n")
  
  player <- as.numeric(readLines("stdin",n = 1))
  if (player %in% c(1,2,3)) {
    computer <- sample(1:3,size = 1,replace = TRUE)
    print( paste("You choose :", df1$item_list[player]) )
    print( paste("I choose :",df1$item_list[computer]) )
    result_list$player <- append(result_list$player , df1$item_list[player])
    result_list$computer <- append(result_list$computer , df1$item_list[computer])
    
      if (player == computer){
        result_list$result <- append(result_list$result,"Tie")
        print(">> It's a TIE ! <<")
      }else if((player == 1 & computer == 3) | (player == 2 & computer == 1) | (player == 3 & computer == 2)) {
        result_list$result <- append(result_list$result,"Win")
        print(">> How lucky!, You WIN !! <<")
      }else if((player == 3 & computer == 1) | (player == 1 & computer == 2) | (player == 2 & computer == 3)) {
        result_list$result <- append(result_list$result,"Lose")
        print(">> Sorry but you LOSE ;p <<")
      }
    result_df <- data.frame(result_list$player,result_list$computer,result_list$result)
    colnames(result_df) <- c("Player","Computer","Result")
  }else if(player == 99) {
      if( is.null(result_df) ){
        print("Your total score is Zero.")
      } else {
        print(result_df)
        print( paste("Win :", nrow(result_df[result_df$Result == "Win", ])) )
        print( paste("Lose :",nrow(result_df[result_df$Result == "Lose", ])) )
        print( paste("Tie :", nrow(result_df[result_df$Result == "Tie", ])) )
        }
      i <- i+1
      cat("===================================",
          "Hope to play with you again!",
          "===================================",sep = "\n")
  }else{
      print("You entered incorrect number, please try again.")
  }
} 
