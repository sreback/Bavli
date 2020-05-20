# function definition: assemble_shas
# takes raw html file and converts to data frame
# masechet details are in last row with rowid = 0

assemble_shas <- function(x) {
    raw_text <- read_html(x)
    masechet <- raw_text %>% html_nodes("h2") %>%
        head(1) %>% html_text() %>%
        str_remove("\u5de\u5e1\u5db\u5ea") %>%
        str_remove("\u5e4\u5e8\u5e7\u20\u5d0") %>% str_trim()
    message(masechet)
    
    # generate node list -- dapim & mishna/gemara breaks
    if (masechet == ("\u5d1\u5e8\u5db\u5d5\u5ea")) {
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
    pages <- breaklist$Daf %>% str_extract(("\u5d3\u5e3 \\w+\\b")) %>% 
        unique() %>% length()
    message(pages)
    
    # this works to create database.
    all_text  <-  full_join(breaklist, talmud_text, by = "rowid")
    all_text <- all_text %>% add_row(rowid = 0, Talmud = masechet, Daf = as.character(pages))
    return(all_text)
}
