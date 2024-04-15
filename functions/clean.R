# remove non-alphanumeric characters for later string manipulation
remove_non_alphanumeric <- function(string) {
  stringr::str_replace_all(string, "[^A-Za-z0-9]", "")
}

# create list of usaspending award ID api calls
format_contract_numbers <- function(df) {
  
  contract_num_formatted <- vector("character")
  contract_num_do_formatted <- vector("character")
  
  for (i in 1:nrow(df)) {
    if (is.na(df$deliveryorder[i]) & !is.na(df$contractnumber[i])) {
      contract_num_formatted[i] <- paste0("CONT_AWD_", df$contractnumber[i], "_9700_-NONE-_-NONE-")
    } else if (!is.na(df$deliveryorder[i]) & !is.na(df$contractnumber[i])) {
      contract_num_do_formatted[i] <- paste0("CONT_AWD_", df$deliveryorder[i], "_9700_", df$contractnumber[i], "_9700")
    } else {
      contract_num_formatted[i] <- paste0("CONT_AWD_", df$deliveryorder[i], "_9700_-NONE-_-NONE-")
    }
  }
  
  return(list(contract_num_formatted = contract_num_formatted, 
              contract_num_do_formatted = contract_num_do_formatted))
}

# splice list into tenths due to server error with larger lists
splice_into_tenths <- function(lst) {
  # Calculate the length of the list and round to the nearest integer
  len <- round(length(lst))
  
  # Calculate the index positions for each tenth
  divisions <- seq(0, 1, by = 0.1)
  indexes <- ceiling(len * divisions)
  
  # Create tenths using index positions
  tenths <- map2(indexes[-length(indexes)], indexes[-1], ~ lst[.x:.y])
  
  # Return tenths as a list of lists
  setNames(tenths, paste0("tenth", 1:length(tenths)))
}

# perform transactions API request from USASpending
transactions_request <- function(query_string_list) {
  
  # create connection
  req <- request("https://api.usaspending.gov/api/v2/transactions/")
  
  # initialize list
  usa_list <- list()
  
  for(i in seq_along(query_string_list)){
  
    # define input based on a single list value
    contract_num_input <- query_string_list[i]
    
    # perform the request
    resp <- req %>% 
      req_body_json(
      list(
        award_id = contract_num_input,
        limit = 1,
        page = 1
      )
    ) %>% 
    req_perform()
  
  # store in a list
  usa_list[[contract_num_input]] <- resp %>% 
    resp_body_json()
  }
  return(usa_list)
}

# perform summary API request from USASpending
summary_request <- function(contract_num_input) {
  
  # create connection
  req <- request("https://api.usaspending.gov/api/v2/search/transaction_spending_summary/")
  
  # initialize list
  sum_list <- list()
  
  # create the request body using req_body_json
  resp <- req %>% 
    req_body_json(
    list(
      filters = list(
        keywords = list(contract_num_input)
      )
    )
  ) %>% 
  req_perform()
  
  # store in a list
  sum_list[[contract_num_input]] <- resp %>% 
    resp_body_json()

}

# function to perform API requests with a delay and error handling
safe_api_call <- function(input_list, req_func, delay_time = 1) {
  
  # Wrap your API request function with safely
  safe_transactions_request <- safely(req_func)
  
  # Perform API requests on the list of vectors with a delay between requests
  results <- map(input_list, function(vector) {
    result <- safe_transactions_request(vector)
    Sys.sleep(delay_time)  # Pause execution for the specified delay time
    return(result)
  })
  
  return(results)
}

# recursive function to traverse the nested list structure and extract "results" lists
extract_results <- function(nested_list) {
  
  # initalize empty results list
  results_list <- list()
  
  # traverse the nested structure
  traverse_nested <- function(nested_list) {
    if (is.list(nested_list)) {
      for (element in nested_list) {
        if ("results" %in% names(element)) {
          # if "results" list is found, add it to the results_list
          results_list <<- c(results_list, element$results)
        } else {
          # recursively traverse the nested structure
          traverse_nested(element)
        }
      }
    }
  }
  
  # start traversing the nested structure
  traverse_nested(nested_list)
  
  # convert each "results" list to a data frame and bind them together
  results_df <- bind_rows(results_list)
  
  return(results_df)
}