
#' Fixes home data
#' This is a non-exported helper function
#'
#' @param home_data_df
#'
#' @return tibble

fix_home_data <- function(home_data_df){
  # Remove assessment
  home_data_df$assessment <- NULL

  # Remove files
  home_data_df$files <- NULL

  # Fix coordinates
  coordinates <- dplyr::bind_rows(home_data_df$coordinates)
  names(coordinates) <- paste0("coordinates_", names(coordinates))
  home_data_df$coordinates <- NULL
  home_data_df <- cbind(home_data_df, coordinates)

  # Fix estimate
  estimate <- home_data_df$estimate
  names(estimate) <- paste0("estimate_", names(estimate))
  home_data_df$estimate <- NULL
  home_data_df <- cbind(home_data_df, estimate)

  # Fix expenses
  expenses <- home_data_df$expenses
  home_data_df$expenses <- NULL
  ownership_expenses <- expenses$ownership_expenses
  expenses$ownership_expenses <- NULL
  expenses <- cbind(expenses, ownership_expenses)
  names(expenses) <- paste0("expenses_", names(expenses))
  home_data_df <- cbind(home_data_df, expenses)

  # Remove home info
  home_data_df$home_info <- NULL

  # Fix market info
  market_info <- home_data_df$market_info
  home_data_df$market_info <- NULL
  names(market_info) <- paste0("market_info_", names(market_info))
  home_data_df <- cbind(home_data_df, market_info)

  # Fix suggest
  suggest <- home_data_df$suggest
  suggest$contexts <-  unlist(suggest$context)
  suggest$address1 <- purrr::map_chr(suggest$input, function(x) x[1])
  suggest$address2 <- purrr::map_chr(suggest$input, function(x) x[2])
  suggest$input <- NULL
  home_data_df$suggest <- NULL
  names(suggest) <- paste0("suggest_", names(suggest))
  home_data_df <- cbind(home_data_df, suggest)

  # Take first sales
  # sales <- home_data_df$sales
  # sales <- purrr::map_df(sales, function(x){
  #   x <-  x[1,]
  #   if(ncol(x) == 0) x <- tibble::tibble(date = NA_character_, price = NA_integer_, type = NA_character_)
  #   return(x)
  # })
  # names(sales) <- paste0("sales_", names(sales))
  # home_data_df <- cbind(home_data_df, sales)
  home_data_df$sales <- NULL


  # Return the home data
  return(home_data_df)
}


#' Turns a list of home data in to one big data frame
#' This is a non-exported helper function
#'
#' @param home_data_list
#'
#' @return tibble
home_data_list_to_df <- function(home_data_list){

  fixed_data <- purrr::map_df(home_data_list, fix_home_data)

  return(fixed_data)

}
