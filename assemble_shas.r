# function definition: assemble_shas
# takes raw html file and converts to data frame
# masechet details are in last row with rowid = 0
assemble_shas <- function(x) {

	library(dplyr)
	library(rvest)
	library(stringr)
	library(tibble)
	
    raw_text <- read_html(x)
    masechet <- raw_text %>% html_nodes("h2") %>%
        head(1) %>% html_text() %>%
        str_remove("מסכת") %>%
        str_remove("פרק א") %>% str_trim
    
    # generate node list -- dapim & mishna/gemara breaks
    if (masechet == "ברכות") {
        dapim <- raw_text %>% html_nodes("p") %>% tail(-3)
    } else {
        dapim <- raw_text %>% html_nodes("p") %>% tail(-1)
    }
    names(dapim) <- "dapim"
    
    # list all the nodes
    # breaklist is a tibble of each daf/mishna/gemara break
    breaklist <- dapim %>% xml_children() %>%
        html_text() %>%
        enframe(value = "Daf", name = NULL) %>%
        filter(str_length(Daf) >= 10) %>%
        rowid_to_column()
    
    # extract text for each item
    # remove opening daf reference from each segment
    talmud_text <- dapim %>% xml_text() %>%
        str_replace("^\\w+\\s\\w+,\\w\\s\\w+\\s", "") %>%
        enframe(value = "Talmud", name = NULL) %>%
        rowid_to_column()
    
    
    # structure
    pages <- breaklist$Daf %>% str_extract("דף \\w+\\b") %>% 
        unique() %>% length()
    
    # this works to create database.
    all_text  <-  full_join(breaklist, talmud_text, by = "rowid")
    all_text <- all_text %>% add_row(rowid = 0, Talmud = masechet, Daf = pages)
    return(all_text)
}
