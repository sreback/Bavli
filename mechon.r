## TRYING TO READ DOWNLOADED MECHON MAMRE FILES 

# bavli link: https://www.mechon-mamre.org/b/l/l0.htm

library(stringr)
library(dplyr)
library(rvest)
library(tibble)

# download original file with GetHTMLFile.r


################### CREATE THE BAVLI FILE ##############

setwd("f:/RProjects/Projects/Bavli/Data")
source_file <- choose.files(multi=FALSE)


proxy <- curl::ie_get_proxy_for_url("https://raw.githubusercontent.com/")
if (!is.null(proxy)) {
    Sys.setenv(https_proxy=proxy)
}

source_file <- "https://raw.githubusercontent.com/sreback/Bavli/master/Data/avodazara.htm"

# rvest: reads in Ivrit
# function definition: assemble_shas
# takes raw html file and converts to data frame
# masechet details are in last row with rowid = 0
assemble_shas <- function(x) {
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

# function definition: tell_me_about
# takes a processed bavli data frame and returns key information
# about the masechet (no error checking)

tell_me_about <- function (x) {
    # name
    masechet_name <- as.character(x[nrow(x), 3])
    # words  (string of word characters)
    words <- x$Talmud %>% str_count("\\w+") %>% sum()
    # pages
    pages <- as.numeric(x[nrow(x), 2])
    # # words/page
    word_density <- round(words/pages,0)
    y <- tibble::enframe(c(masechet = masechet_name, dapim = pages, world_length = words))
    return(y)
    
    #     sprintf("%s Dapim: %i \\nWords: %i \\nWords/daf %.2f",
    #             as.character(masechet_name),
    #             pages,
    #             words,
    #             word_density)
}    

full_text <- assemble_shas(source_file)
tell_me_about(full_text)

full_text %>% tail(3)

########### SEARCHING AMRI INSHI  #############

## still want to return multiple appearance in same section
## still want to find across page boundaries

amri <- 'אמרי אינשי'
enddaf <- 'אמרי$'
testsrch <- 'אמר$'
standing <- "קאימנא קמיה"

srch <- standing

# Total occurrences:
full_text$Talmud %>% str_count(standing) %>% sum()

# this works for one occurrence in each section
(results <- subset(full_text, str_detect(Talmud, srch)) %>% 
        mutate(Text = str_extract(Talmud, 
                                  paste(srch,"(\\w+|[[:punct:]]?[[:space:]]){3,9}"))) %>% 
        select(-Talmud))

# test to see if there are any boundary "amar"s
# if found, extract rowids, calculate next rowids and select only those 
# rows from main list
if(sum(str_detect(full_text$Talmud, testsrch)) ==0) {
    sprintf("Not found") 
} else {
    found_count <- full_text$Talmud %>% str_count(testsrch) %>% sum() 
    found_rows <- full_text %>% filter(str_detect(Talmud, testsrch)) %>% 
        select(rowid) 
    next_rows <-  found_rows + 1  
    needed_rows <- next_rows %>% full_join(found_rows) %>% arrange(rowid)
    selected_rows <- needed_rows %>% left_join(full_text)
}




########### GENERIC SEARCHES #############
# any text searches

srch <- "שלום"
srch <- "עקיבא"
srch <- "הרבה עשו כן"
srch <- "קאימנא קמיה"

# Total occurrences:
full_text$Talmud %>% str_count(srch) %>% sum()

# this works for one occurrence in each section
(results <- subset(full_text, str_detect(Talmud, srch)) %>% 
        mutate(Text = str_extract(Talmud, 
                                  paste(srch,"(\\w+|[[:punct:]]?[[:space:]]){3,9}"))) %>% 
        select(-Talmud))



