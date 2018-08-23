library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)
library(minerva)
library(foreach)
library(tibble)

# Function to extract (numeric|integer) columns 
extract_cols <- function(x) {return(is.numeric(x) | is.integer(x))}


get_mic_df <- function(df, is_flat = TRUE){
  
  #' @description 
  #' Function to get information around MIC with data frame format
  #' minerva::mine returns a list of tables, which is hard to handle.
  #' Without the argument 'is_flat', you can get the return of the method with a list of data frame.
  #' With the argument 'is_flat', you can get with a plain data frame format.
  #' Note that when compute mic, records which has NA value will be droped.
  #' 
  #' @param df : Data frame object. This is input to compute mic.
  #' @param is_flat : Boolean, whether you want to get the return with plain data frame format.
  
  df_selected <- df %>% select_if(extract_cols)
  
  # Confirm existance of integer or numeric columns.
  if (ncol(df_selected) == 0L){
    stop("Input data frame doesn't have any integer or numeric column.")
  }
  
  cols <- df_selected %>% names()
  
  # Compute mic and transform to list of data frame
  mics <- df_selected %>% 
    mine(use = "complete.obs") %>% 
    map(~ as.tibble(.x) %>% 
          mutate(col = cols) %>% 
          select(dim(.x)[2] + 1, 1:(dim(.x)[2])))
  
  if(is_flat){
    # Transform mic result to flat data frame
    mics_tidy <- mics %>% 
      map(gather, key = "col2", value = "val", -col)
    
    mics_df <- foreach(
      i = 1:length(names(mics_tidy)),
      .combine = bind_rows
    ) %do% {
      
      mics_tidy[i] %>% 
        as.data.frame() %>%
        set_colnames(c("col", "col2", "val")) %>% 
        mutate(idx = names(mics_tidy)[i])
      
    } %>% 
      select(idx, col, col2, val)
    
    mics <- mics_df
    
  }
  
  return(mics)
}


get_group_mic_df <- function(df, group_keys, min_value){
  
  #' @description 
  #' Function to compute indexes around mic for each hierarchy.
  #' minerva::mine doesn't compute for each group.
  #' Note that if a group has few records, its group will be ignored.
  #' You can set threshold to drop with the parameter, min_value.
  #' 
  #' @param df : Data frame object which is input of mic.
  #' @param group_keys : Vector of characters, which is key of hierarchy
  #' @param min_value : Minimum number of records in each group.
  
  df_group <- df %>% select(group_keys)
  df_selected <- df %>% select_if(extract_cols)
  
  # Confirm existance of integer or numeric columns. 
  if (ncol(df_selected) == 0L){
    stop("Input data_frame doesn't have any integer or numeric column.")
  }
  
  # Drop NA and count records for each hierarchy.
  df_tmp <-df_group %>% 
    cbind(df_selected) %>% 
    drop_na(... = colnames(df_selected)) %>% 
    group_by_(.dots = group_keys) %>% 
    filter(n() >= min_value)
  
  if(nrow(df_tmp) == 0L){
    stop("No records for input of mic")
  } 
  
  df_tmp %<>% nest()
  group_var <- df_tmp %>% select(group_keys)
  
  cols <- df_selected %>% colnames()
  
  mics <- df_tmp$data %>%
    map(
      ~ as.data.frame(.) %>%
        mine(use = "complete.obs")
    )
  
  idx <- names(mics[[1]])
  
  res_df <- NULL
  for (i in 1:nrow(group_var)) {
    
    tmp <- mics %>%
      pluck(i) %>% 
      map(
        ~ as.tibble(.x) %>%
          mutate(col = cols) %>%
          select(dim(.x)[2] + 1, 1:(dim(.x)[2])) %>%
          gather(key = 'col2', value = 'val', -col)) 
    
    mics_tidy_i <- foreach(j = 1:length(idx), .combine = rbind) %do% {
      
      tmp[j] %>% 
        as.data.frame() %>%
        set_colnames(c("col", "col2", "val")) %>% 
        mutate(idx = idx[j])
      
    }
    
    key_i <- foreach(k = 1:nrow(mics_tidy_i), .combine = rbind) %do% {
      
      group_var %>% slice(i) 
      
    }
    
    res <- cbind(key_i, mics_tidy_i)
    res_df %<>% rbind(res)
    
  }
  
  df_idx <- res_df %>% select(idx)
  df_col_val <- res_df %>% select(col, col2, val)
  df_gr <- res_df %>% select(-idx, -col, -col2, -val)
  
  res_df_ordered <- df_idx %>% 
    bind_cols(df_gr) %>% 
    bind_cols(df_col_val)
  
  return(res_df_ordered)
  
}


get_mic <- function(df, group_keys = NA, min_value = 3L, is_flat = TRUE){
  
  #' @description
  #' Utility function to call get_mic_df and get_group_mic_df
  #' When you set group_keys, get_group_mic_df will be executed.
  #' Without group_keys, get_mic_df will be executed.

  if(is.na(group_keys)){
    out <- get_mic_df(df, is_flat)
  } else {
    out <- get_group_mic_df(df, group_keys, min_value)
  }
  
  return(out)
  
}
