# just a way to remove files as we go to avoid cluttering the environment
remove_ls <- function(pattern = NULL){
  remove(list = unlist(ls(name = globalenv(), pattern = pattern)),envir = globalenv())
  return(invisible())
}

# just a way to avoid excessive parsing messages
read_csv3 <- function(file){
  read_csv(file, progress = FALSE, col_types = cols())
}