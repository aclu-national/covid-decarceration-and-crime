custom_errors <- function(errors, data = NULL, custom_msg = NULL){
  if (is.null(attr(data, "assertr_errors"))) 
    attr(data, "assertr_errors") <- list()
  attr(data, "assertr_errors") <- append(attr(data, "assertr_errors"), 
                                         errors)
  if(!is.null(custom_msg)){
    warning_line(glue::glue(custom_msg))
  }
  error_stop(errors, data, warn = TRUE)
  
  return(data)
}


# Makes assertions within each crime dataset to ensure the data is as expected, depending on whether it
# came at the incident, offense or month level. 
verify_crime_dataset <- function(.data, type = c("incident_level","offense_level","month_level"), id_warning = NULL){
  
  res <- .data
  type <- match.arg(type)
  
  id_errors <- function(errors, data = NULL){
    custom_errors(errors = errors ,data = data,custom_msg = 
                    paste("{nrow(errors[[1]]$error_df)} non-unique ID in dataset",id_warning, sep = "\n")
    )
  }
  
  if(type == "incident_level"){
    res <- res %>%
      verify(assertr::has_all_names('id','occur_date','year','month','location'), error_fun = error_stop) %>%
      assert(is_uniq, id, error_fun = id_errors) 
  } else if(type == "offense_level") {
    res <- res %>%
      verify(assertr::has_all_names('id','occur_date','year','month','location'), error_fun = error_stop) 
  } else {
    # monthly level validations
    res <- res %>%
      verify(assertr::has_all_names('month','n','location'), error_fun = error_stop) 
  }
  
  return(res)
}
