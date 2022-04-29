# internal function to add check digit to id
.add_check_digit <- function(id, strict = TRUE, verbose = TRUE){
  split <- strsplit(as.character(id), "")[[1]]
  if(strict) stopifnot(length(split) == 7)
  dig <- check_digit(id)
  split[3] <- dig
  x <- as.numeric(paste0(split, collapse=""))
  if(verbose && x != id)
    message(sprintf(
      "ID '%s' changed to '%s'",
      id, x))
  x
}

# vectorised function to add check digit to vector of ids
add_check_digit <- function(ids, strict = FALSE, verbose = TRUE){
  sapply(ids, .add_check_digit, 
         strict = strict,
         verbose = verbose)
}

# function to return check digit of an id
check_digit <- function(id) {
  split <- strsplit(as.character(id), "")[[1]]
  if(length(split) >= 7) split <- split[-3]
  check2 <- sapply(1:length(split), function(x){
    y <- as.numeric(split[x])
    if(! x %% 2) y <- y*2
    sapply(y, function(z){
      i <- strsplit(as.character(z), "")[[1]]
      sum(as.numeric(i))
    })
  })
  check <- sum(check2) %% 10
  if(check == 0) return(check)
  10 - check
}