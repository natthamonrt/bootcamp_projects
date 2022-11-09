link : https://replit.com/@naththmnratnsur/Batch06ChatbotPizza?v=1

# readLines("stdin",n=1) 
# stdin = starndard input from console, n=1 to get one input
# chatbot rule-based

# welcome message
print("Hola! Welcome to Golden Boy Pizza")
print("How should we call you? ")
user_name <- readLines("stdin",n=1)
print( paste("Great seeing you!",user_name) )

# create dataframe of menus
cat("------------------------------------------------------------------",
    "Here is our best pizza!",
    "------------------------------------------------------------------",sep="\n")
pizza_menu <- c(
  "Cheese(S)","Cheese(L)",
  "Pepperoni(S)","Pepperoni(L)",
  "Sausage(S)","Sausage(L)",
  "Combo(S)","Combo(L)",
  "Pesto Veggie(S)","Pesto Veggie(L)")
pizza_components <- c(
  "cheese & sauce","cheese & sauce",
  "pepperoni & sauce","pepperoni & sauce",
  "sausage & sauce","sausage & sauce",
  "Pepperoni, Sausage, Cheese","Pepperoni, Sausage, Cheese",
  "Onion, Mushroom, Black Olive","Onion, Mushroom, Black Olive"
)
pizza_price <- c(12.00,22.50,13.50,25.50,13.50,25.50,15.25,28.75,15.25,28.75)

pizza <- data.frame(pizza_menu,pizza_components,pizza_price)
print(pizza)
cat("------------------------------------------------------------------",
   "------------------------------------------------------------------",sep="\n")
          

basket <- NULL

m<-0
  while(m<1){
    cat("Would you like to order (more)pizza?","[1] yes","[2] no",sep="\n")
    more_pizza <- as.numeric(readLines("stdin",n=1))
    if(more_pizza == 1){
      
          # take orders
          i<-0
          get_order <- list(id=c(),
                  menu=c(),
                  more_detail=c(),
                  quantity=c(),
                  price=c(),
                  total_price=c())          
          while (i<1){
                  cat("Which one would you like to order today?",
                      (as.character(c("[1] Cheese(S)","[2] Cheese(L)",
                                      "[3] Pepperoni(S)","[4] Pepperoni(L)",
                                      "[5] Sausage(S)","[6] Sausage(L)",
                                      "[7] Combo(S)","[8] Combo(L)",
                                      "[9] Pesto Veggie(S)","[10] Pesto Veggie(L)"))),sep="\n")
                  get_order$id <- as.numeric(readLines("stdin",n=1))
                  if (get_order$id %in% 1:10){
                    i<-i+1
                    print( paste(sample(c("That Sounds Great to choose","Nice!! you choose"),1,replace=TRUE),pizza$pizza_menu[get_order$id] ) )
                    get_order$menu <-as.character(pizza$pizza_menu[get_order$id])
                    get_order$price <- as.numeric(pizza$pizza_price[get_order$id])
                    }else{
                    print("Please type the number again.")
                  }
          }
                    
          j<-0
          while (j<1){
                  cat("Would you like to have additional details? e.g.'No onion','More crispy'",
                      (as.character(c("[1] yes","[2] no"))),sep="\n")
                  get_order$more_detail <- as.numeric(readLines("stdin",n=1))
                  if (get_order$more_detail ==2){
                    j<-j+1
                    get_order$more_detail <- "---no more details---"
                  }else{
                    print("Enter your additional details here.")
                    get_order$more_detail <- as.character(readLines("stdin",n=1))
                    j<-j+1
                  }
          }
                    
          k<-0
          while (k<1){
                  print("How many pizza would you like?")
                  get_order$quantity <- as.numeric(readLines("stdin",n=1))
                  if (is.numeric(get_order$quantity) == TRUE){
                    k<-k+1
                    get_order$total_price <-get_order$quantity * pizza$pizza_price[get_order$id]
                  }else{
                    print("Please try the number of pizza that you want again.")}
          }
          order_df <- data.frame(get_order)
          basket <- rbind(basket,order_df)
          print("Let me repeat your orders.")
          print(basket)
    
    }else if(more_pizza==2 && is.null(basket) == TRUE){
          m<-m+1
    }else if(more_pizza==2 && !is.null(basket) == TRUE){ 
          m<-m+1
          print( paste("Your total payments are ",sum(basket$total_price),"dollars"))
          cat("Your pizza are on processing and ready in 15-20 minutes.",
             "Thank you so much.",
             "HAVE A NICE DAY!!",sep="\n")
    }else{
      print("You entered incorrected number, please try again.")
    }
    }
