scrape_yahoo <- function(stock, sheet) {
  if (sheet == "IS") {
    url_piece <- "/financials?p="
  }
  
  df <- "https://finance.yahoo.com/quote/" %>%
    str_c(stock, url_piece, stock) %>%
    html_session() %>%
    html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table') %>%
    html_table(fill = TRUE) %>%
    .[[1]]
  
  df
}

wrangle_fin <- function(df) {
  colnames(df) <- paste(df[1,])
  
  df %<>%
    rename(subcategory = 1) %>%
    # take the subheaders and put them into their own column
    mutate(category = if_else(.[[1]] == .[[2]] | str_detect(.[[2]], "/"),
                              subcategory,
                              NA_character_)) %>%
    fill(category) %>%
    # remove subheaders row
    filter(category != subcategory) %>%
    rowid_to_column("group")
  
  df
}

clean_fin <- function(df) {
  df %<>%
    mutate_at(3:6, str_replace, "^-$", "0") %>%
    mutate_at(3:6, parse_number)
  
  df
}

tidy_fin <- function(df) {
  df <- df %>%
    gather(key = "date", value = "value", 3:6)
  
  df
}