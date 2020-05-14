# function definition: tell_me_about
# takes a processed bavli data frame and returns key information
# about the masechet (no error checking)

tell_me_about <- function (x) {

	library(dplyr)
	library(stringr)

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
