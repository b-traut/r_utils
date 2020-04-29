# Load all excel files and sheets which all data will be binded vertically.

extract_excel_book_names <- function(directory, is_recursive = TRUE) {
  
  #' @description Create the vector of excel book path existing under the directory
  #' @param directory The directory path under which excel files will be loaded.
  #' @param is_recursive Whether to search recursively. Default is TRUE
  #' @return The vector of full path of excel books existing under the directory
  
  base::stopifnot(
    directory %>% base::sprintf("Directory %s is not found", .),
    exprs = {base::dir.exists(directory)}
  )
  
  book_paths <- directory %>%
    base::list.files(
      pattern = ".xls(b|m|x)?$",
      full.names = TRUE,
      recursive = is_recursive)
  
  return(book_paths)
  
}


.load_all_sheets_on_book <- function(book, ...) {
  
  sheets <- book %>% purrr::map(~ .x %>% readxl::excel_sheets())
  df_book <- sheets %>%
    purrr::map_dfr(
      readxl::read_excel(book, sheet = .x, col_types = "text", ...) %>%
        dplyr::mutate(
          book_name = base::basename(book),
          sheet_name = .x,
          row_id = dplyr::row_number()
        ) %>%
        tidyr::pivot_longer(
          -c(book_name, sheet_name, row_id)
          names_to = "label",
          values_to = "value"
        )
    )
  return(df_book)
}


load_all_sheets_on_books <- function(books, ...) {
  df_books <- books %>%
    purrr::map_dfr(
      ~ .x %>% .load_all_sheets_on_book(...)
    )
  return(df_books)
}
