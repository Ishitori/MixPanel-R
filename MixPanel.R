mixpanel <- function (key, secret) {
  library(RCurl)
  library(rjson)
  library(digest)
  library(stringr)
  library(httr)	
  
  # helper function to join named vectors using sep
  join <- function(vector, sep = "")
  {
    #in case of creating a hashed_sig the parameters should be alphabetically ordered
    ordered_vector <- vector[order(ifelse(names(vector) == '', NA, names(vector)))]
    res <- lapply(seq_along(vector), function(x) { ifelse(names(vector)[x] != '', paste(names(vector)[x], vector[x], sep = "="), vector[x]) })
    paste(unlist(res), sep = '', collapse = sep)
  }
  
  # main function that execute a custom method with provided args
  # linePerObject specifies should we expect JSONL as a result or not
  executeQuery <- function(method, args, linePerObject = F)
  {
    # Set the expiry time for the API link as 15 minutes
    expire <- as.integer(as.numeric(as.POSIXlt(Sys.time()))) + 900 
    argsFull <- append(append(c("api_key" = key, "expire" = as.character(expire)), args), c(secret))
    args_sig <- join(argsFull)
    hashed_sig <- digest(args_sig, algo="md5", serialize = FALSE)

    argsNoSecret <- append(c("api_key" = key, "expire" = as.character(expire), "sig" = hashed_sig), args)
    args_url <- join(argsNoSecret, sep="&")
    url <- paste("https://data.mixpanel.com/api/2.0/", method, "/?", args_url, sep="", collapse=NULL)
    content <- content(GET(url), as = 'text')
    
    if (linePerObject)
    {
      items <- unlist(str_split(str_trim(content), "\n")[[1]])
      # Mixpanel returns \n in the end of file, so the last time is an empty string
      dataset <- array(list(), length(items) - 1)
      
      for (i in seq(1, length(items) - 1))
      {
        dataset[i] <- list(fromJSON(items[i]))
      }
      
      dataset
    }
    else
    {
      dataset <- fromJSON(content)
    }
  }
  
  #public API function - return the list of raw events from - to date
  exportEvents <- function(from, to)
  {
    args = c("from_date" = format(from, "%Y-%m-%d"), "to_date" = format(to, "%Y-%m-%d"))
    executeQuery(method = "export", args, linePerObject = T)
  }

  #add more public functions here
  list(
    exportEvents = exportEvents
  )
}