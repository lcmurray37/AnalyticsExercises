# remove non-alphanumeric characters for later string manipulation
remove_non_alphanumeric <- function(x) {
  stringr::str_remove_all(x, "[^[:alnum:]]")
}

# create list of usaspending award ID api calls
format_contract_numbers <- function(df) {
  
  list(
    contract_num_formatted = dplyr::case_when(
      is.na(df$deliveryorder) & !is.na(df$contractnumber) ~
        paste0("CONT_AWD_", df$contractnumber, "_9700_-NONE-_-NONE-"),
      
      !is.na(df$deliveryorder) & !is.na(df$contractnumber) ~
        paste0("CONT_AWD_", df$deliveryorder, "_9700_", df$contractnumber, "_9700"),
      
      TRUE ~
        paste0("CONT_AWD_", df$deliveryorder, "_9700_-NONE-_-NONE-")
    ),
    
    contract_num_do_formatted = dplyr::if_else(
      !is.na(df$deliveryorder) & !is.na(df$contractnumber),
      paste0("CONT_AWD_", df$deliveryorder, "_9700_", df$contractnumber, "_9700"),
      NA_character_
    )
  )
}

# create raw list of usaspending award IDs
raw_id <- function(df) {
  dplyr::coalesce(df$contractnumber, df$deliveryorder)
}

# splice list into `segments` due to server error with larger lists
split_segments <- function(x, batch_size = 50) {
  split(x, ceiling(seq_along(x) / batch_size))
}

# perform transactions API request from USASpending
transactions_request <- function(ids) {
  
  req <- httr2::request(
    "https://api.usaspending.gov/api/v2/transactions/"
  )
  
  out <- purrr::map(
    ids,
    \(id) {
      req |>
        httr2::req_body_json(
          list(
            award_id = id,
            page = 1
          )
        ) |>
        httr2::req_perform() |>
        httr2::resp_body_json()
    },
    .progress = "Transactions"
  )
  
  names(out) <- ids
  out
}

# perform summary API request from USASpending
summary_request <- function(contract) {
  
  httr2::request(
    "https://api.usaspending.gov/api/v2/search/transaction_spending_summary/"
  ) |>
    httr2::req_body_json(
      list(
        filters = list(
          keywords = list(contract)
        )
      )
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  
}

# function to perform API requests with a delay and error handling
safe_api_call <- function(input_list, req_func, delay_time = 1) {
  
  # Wrap your API request function with safely
  safe_transactions_request <-
    purrr::possibly(
      transactions_request,
      otherwise = list()
    )
  
  # Perform API requests on the list of vectors with a delay between requests
  results <- map(input_list, function(vector) {
    result <- safe_transactions_request(vector)
    Sys.sleep(delay_time)  # Pause execution for the specified delay time
    return(result)
  })
  
  return(results)
}

# recursive function to traverse the nested list structure and extract "results" lists
extract_results <- function(x) {
  dplyr::bind_rows(
    purrr::map(
      x$con,
      ~ dplyr::bind_rows(.x$results)
    )
  )
}