
clean_itemtext <- function(qop_data) {
    iteminfo <- qop_data[["spm"]]
    itemstem <- qop_data[["spmtekst"]]
}

prep_rater_data <- function(qop_data) {
    essay_rating <- qop_data[["sensur"]]
    essay_rating$diff <- abs(essay_rating$kar - essay_rating$kar2)
    return(essay_rating)
}