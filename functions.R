# Goal of this was taking the solution from stack overflow and 
# making it into the same format as tidyquant used to have
# here's the code I modified:
# https://stackoverflow.com/questions/49452906/getfinancials-quantmod-and-tq-get-tidy-quant-not-working/49466286#49466286
library(tidyverse)
library(rvest)

scrape_yahoo <- function(stock, sheet) {
  if (sheet == "IS") {
    url_piece <- "/financials?p="
  }
  if (sheet == "BS") {
    url_piece <- "/balance-sheet?p="
  }
  if (sheet == "CF") {
    url_piece <- "/cash-flow?p="
  }
  
  df <- "https://finance.yahoo.com/quote/" %>%
    str_c(stock, url_piece, stock) %>%
    html_session() %>%
    html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table') %>%
    html_table(fill = TRUE) %>%
    .[[1]] %>%
    as_tibble()
  
  df
}

wrangle_yahoo_fin <- function(df) {
  colnames(df) <- paste(df[1,])
  
  df %<>%
    rename(category = 1) %>%
    # take the subheaders and put them into their own column
    mutate(subheader = if_else(.[[1]] == .[[2]] | str_detect(.[[2]], "/"),
                              category,
                              NA_character_)) %>%
    fill(subheader) %>%
    # remove subheaders row
    filter(subheader != category) %>%
    rowid_to_column("group")
  
  df
}

clean_yahoo_fin <- function(df) {
  df %<>%
    mutate_at(3:6, str_replace, "^-$", "0") %>%
    mutate_at(3:6, parse_number) %>%
    # cash flow sheet doesn't follow the same pattern
    # as the others and needs to be adjusted slightly
    mutate(subheader = if_else(subheader == "Period Ending",
                              category,
                              subheader))
  
  df
}

tidy_yahoo_fin <- function(df) {
  df <- df %>%
    gather(key = "date", value = "value", 3:6)
  
  df
}

get_yahoo_fin <- function(stock, sheet) {
  stock %>%
    scrape_yahoo(sheet) %>%
    wrangle_yahoo_fin() %>%
    clean_yahoo_fin() %>%
    tidy_yahoo_fin()
}

scrape_wsj <- function(stock, sheet, report_time = "annual") {
  wsj_url1 <- "https://quotes.wsj.com/"
  wsj_url3 <- "/financials/"
  if (sheet == "IS") {
    url <- paste0(wsj_url1, stock, wsj_url3, report_time, "/", "income-statement")
  }
  
  df <- url %>%
    read_html() %>%
    html_nodes("table") %>%
    .[[2]] %>%
    html_table(fill = TRUE)
  
  df
}

clean_wsj <- function(df) {
  df %<>%
    na_if("") %>%
    na_if("-") %>%
    remove_empty(c("rows", "cols")) %>%
    rename("category" = 1) %>%
    filter(!str_detect(category, "Growth$"),
           !str_detect(category, "Margin$")) %>%
    mutate_at(.vars = 2:6, str_replace, "\\(", "-") %>%
    mutate_at(.vars = 2:6, str_replace, "\\)", "") %>%
    mutate_at(.vars = 2:6, parse_number) %>%
    distinct() %>%
    gather(key = "year", value = "amount", -category) %>%
    replace_na(list(amount = 0)) %>%
    spread(category, amount) %>%
    mutate(year = ymd(paste0(year, "12", "31"))) %>%
    clean_names()
  
  df
}

get_wsj_fin <- function(stock, sheet, report_time = "annual") {
  scrape_wsj(stock, sheet, report_time) %>%
    clean_wsj()
}

is_get_basics <- function(df) {
  df %>%
    select(-cost_of_goods_sold_cogs_incl_d_a, -gross_income, -sg_a_expense, -ebit,
           -interest_expense, -income_tax, -net_income, -consolidated_net_income,
           -net_income_after_extraordinaries, -net_income_available_to_common,
           -eps_basic, -eps_diluted, -ebitda, -pretax_income)
}