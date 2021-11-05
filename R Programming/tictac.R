#tic tac toe game

#assign user symbol
user_choice <- function() {
  if (interactive()) {
    con <- stdin()
  } else {
    con <- "stdin"
  }
  cat("x or o ?: ")
  p_choice <- readLines(con = con, n = 1)
  while ((p_choice != "x") & (p_choice != "o")) {
    cat(paste("You have chosen: ", p_choice, ".!! Wrong Choice. Please, choose again.\n"))
    cat("x or o ?: ")
    p_choice <- readLines(con = con, n = 1)
  }
  cat(paste("You have chosen: ", p_choice, "\n"))
  return(p_choice)
}


#function to update the board
fill_df <- function(row, column, df, player) {
 df[row, column] <-  player
 return(df)
}


get_sum <- function(x, player) {
  if (any(is.na(x))) {
    x <- x == player
    return(sum(x, na.rm = TRUE))
  }else{
    return(0)
  }
}

# function to get the fill position for computer
fill_position <- function(tic_tac, player) {
  r <- apply(tic_tac, 1, get_sum, player)
  c <- apply(tic_tac, 2, get_sum, player)
  if (2 %in% r) {
    row_index <- which(r == 2)
    val <- which(is.na(tic_tac[row_index, ]))
    return(c(row_index, val))
  }else if (2 %in% c) {
    col_index <- which(c == 2)
    val <- which(is.na(tic_tac[, col_index]))
    return(c(val, col_index))
  }else{
    return(0)
  }
}

#function to place offensive computer moves
computer_move <- function(tic_tac, p_choice, c_choice) {
  diag1 <- diag(tic_tac)
  diag2 <- diag(t(apply(tic_tac, 2, rev)))

  tictac_logic <- tic_tac == c_choice
  df <- replace(tictac_logic, is.na(tictac_logic), FALSE)

  diag1_ <- diag(df)
  diag2_ <- diag(t(apply(df, 2, rev)))

  pos <- fill_position(tic_tac, c_choice)

  if (length(pos) == 2) {
    tic_tac[pos[1], pos[2]] <- c_choice
    return(tic_tac)
  }else if ((sum(diag1_) == 2) & (length(which(is.na(diag1))) == 1)) {
    val <- which(is.na(diag1))
    tic_tac[val, val] <- c_choice
    return(tic_tac)
  }else if ((sum(diag2_) == 2) & (length(which(is.na(diag2))) == 1)) {
    val <- which(is.na(diag2))
    tic_tac_t <- t(apply(tic_tac, 2, rev))
    tic_tac_t[val, val] <- c_choice
    return(apply(t(tic_tac_t), 2, rev))
  }else{
    return(computer_defense_move(tic_tac, p_choice, c_choice))
  }
}

#function to place defensive computer moves
computer_defense_move <- function(tic_tac, p_choice, c_choice) {
  values <- c(tic_tac[, 1], tic_tac[, 2], tic_tac[, 3])
  diag1 <- diag(tic_tac)
  diag2 <- diag(t(apply(tic_tac, 2, rev)))

  tictac_logic <- tic_tac == p_choice
  df <- replace(tictac_logic, is.na(tictac_logic), FALSE)

  diag1_ <- diag(df)
  diag2_ <- diag(t(apply(df, 2, rev)))

  pos <- fill_position(tic_tac, p_choice)

  if (length(pos) == 2) {
    tic_tac[pos[1], pos[2]] <- c_choice
    return(tic_tac)
  }else if (sum(diag1_) == 2 & length(which(is.na(diag1))) == 1) {
    val <- which(is.na(diag1))
    tic_tac[val, val] <- c_choice
    return(tic_tac)
  }else if (sum(diag2_) == 2 & length(which(is.na(diag2))) == 1) {
    val <- which(is.na(diag2))
    tic_tac_t <- t(apply(tic_tac, 2, rev))
    tic_tac_t[val, val] <- c_choice
    return(apply(t(tic_tac_t), 2, rev))
  }else{
    val <- which(is.na(values))
    if (length(val) == 1) {
      values[val] <- c_choice
    }else{
      values[sample(val, 1)] <- c_choice
    }
    return(matrix(values, nrow = 3, ncol = 3))
  }
}


#function to check the game status
check_status <- function(df) {
  if (sum(is.na(df)) == 0) {
    cat("\n********* This game is a DRAW *********\n\n")
    quit(save = "yes", status = 0)
  }
}

#function to check winner
check_winner <- function(tic_tac, player) {
  tictac_logic <- tic_tac == player
  df <- replace(tictac_logic, is.na(tictac_logic), FALSE)
  diag1 <- diag(df)
  diag2 <- diag(t(apply(df, 2, rev)))

  if (3 %in% colSums(df) | 3 %in% rowSums(df) | sum(diag1) == 3 | sum(diag2) == 3) {
    return(TRUE)
  }else{
    return(FALSE)
  }
}

# function to take user input for row and column
input_val <- function() {
  if (interactive()) {
    con <- stdin()
  } else {
    con <- "stdin"
  }

  row <- 0
  col <- 0
  while ((row %in% c(1, 2, 3) & col %in% c(1, 2, 3)) == FALSE) {
    cat("What row ?: ")
    suppressWarnings(row <- as.numeric(readLines(con = con, n = 1)))

    cat("What column ?: ")
    suppressWarnings(col <- as.numeric(readLines(con = con, n = 1)))

    if ((row %in% c(1, 2, 3) & col %in% c(1, 2, 3)) == FALSE) {
      cat("\n!!! Invalid Selection !!! Choose again.. \n\n")
    }
  }
  return(c(row, col))
}

get_input <- function() {
  if (interactive()) {
    con <- stdin()
  } else {
    con <- "stdin"
  }

  entry <- input_val()
  row <- entry[1]
  column <- entry[2]

  cat("Is your input final (y/n): ")
  check <- readLines(con = con, n = 1)
  while (check != "y") {
    if (check != "y" & check != "n") {
      cat("invalid entry. Try again !!\n")
      cat("Is your input final (y/n): ")
      check <- readLines(con = con, n = 1)
    }else{
      entry <- input_val()
      row <- entry[1]
      column <- entry[2]
      cat("Is your input final (y/n): ")
      check <- readLines(con = con, n = 1)
    }
  }
  return(c(row, column))
}

# main function to run the game
run <- function() {
  # assigning the user and computer letter symbols
  p_choice <- user_choice()
  if (p_choice == "x") {
    c_choice <- "o"
  }else{
    c_choice <- "x"
  }

  #creating matrix board
  tic_tac <- matrix(NA, ncol = 3, nrow = 3, byrow = TRUE)
  cat("------------------------ CURRENT BOARD ----------------\n")
  print(tic_tac)
  cat("\n---------------------------------------------------\n")
  count_val <- c(3, 5, 7, 9)

  if (c_choice == "x") {
    cat("\nYou chose 'o', so computer goes first\n\n")
    count <- 1
    round <- 1
    cat("######################## Round 1 ################\n")
    cat("\nComputer move registered\n\n")
    Sys.sleep(1)
    tic_tac <- fill_df(sample(c(1:3), 1), sample(c(1:3), 1), tic_tac, c_choice)

    print(tic_tac)
    cat("\n---------------------------------------------------\n")
  }else{
    cat("\nYou chose 'x', so you go first\n\n")
    count <- 0
    round <- 1
    cat("######################## Round 1 ################\n")
  }


  winner <- FALSE
  while (winner == FALSE & sum(is.na(tic_tac))) {
    count <- count + 1
    if (count %in% count_val) {
      round <- round + 1
      cat(paste("\n########################  Round", round, "################\n"))
    }

    cat("\n\n****** Your turn *******\n\n")
    entry <- get_input()
    row <- entry[1]
    column <- entry[2]

    while (is.na(tic_tac[row, column]) == FALSE) {
      cat("\n!!!Move already placed in that position. Please choose again..!!!\n\n")
      print(tic_tac)
      cat("\n")
      entry <- get_input()
      row <- entry[1]
      column <- entry[2]
    }

    cat("\nMove placed\n\n")
    tic_tac <- fill_df(row, column, tic_tac, p_choice)

    print(tic_tac)

    winner <- check_winner(tic_tac, p_choice)
    if (winner == TRUE) {
      cat(paste("\n\n****** Player: ", p_choice, " is winner.*******\n\n"))
      quit(save = "yes", status = 0)
    }
    check_status(tic_tac)
    count <- count + 1
    if (count %in% count_val) {
      round <- round + 1
      cat(paste("\n########################  Round", round, "################\n"))
    }
    tic_tac <- computer_move(tic_tac, p_choice, c_choice)
    cat("\nComputer move registered\n")
    Sys.sleep(1)

    cat("\n-------------------- CURRENT BOARD ------------------\n")
    print(tic_tac)
    cat("\n---------------------------------------------------\n")
    winner <- check_winner(tic_tac, c_choice)
    if (winner == TRUE) {
      cat(paste("\n***** Player: ", c_choice,  "is winner.*****\n\n"))
      quit(save = "yes", status = 0)
    }
    check_status(tic_tac)
  }
}

run()
