prep_rater_data <- function(qop_data) {
    essay_rating <- qop_data[["sensur"]]
    essay_rating$diff <- abs(essay_rating$kar - essay_rating$kar2)
    return(essay_rating)
}

prep_alttext <- function(qop_data){
    item_alttext <- qop_data[["spm"]]

    item_alttext <- item_alttext[
        item_alttext$type %in% c("MC", "MR"), 
        c("spmid", "altnr", "alttekst")]

    item_alttext$alt_letters <- letters[item_alttext$altnr + 1]
    item_alttext <- split(item_alttext, item_alttext$spmid)

    item_alttext <- lapply(item_alttext, 
        function(df) df[, c("alt_letters", "alttekst")])

    paste_alt_func <- function(df) {
        txt1 <- apply(df, 1, paste, collapse = ": ")
        txt2 <- paste(txt1, collapse = "\\\\ \n")
        return(txt2)
    }

    item_alttext <- lapply(item_alttext, paste_alt_func)

    return(item_alttext)
}

prep_iteminfo <- function(qop_data){
    for_iteminfo <- qop_data[["spm"]]
    for_iteminfo <- for_iteminfo[,
        c("spmid",
        "oppgave",
        "kortform",
        "blokk",
        "spmnr", 
        "fag",
        "type"
    )]

    for_iteminfo <- unique(for_iteminfo)
    for_iteminfo$itemlabel <- paste0(
        for_iteminfo$kortform, 
        " - Del: ", for_iteminfo$blokk,
        " - Spm: ", for_iteminfo$spmnr,
        " - Fag: ", for_iteminfo$fag,
        " - ID: ", for_iteminfo$spmid,
        " - Format: ", for_iteminfo$type

        )

    for_iteminfo <- split(for_iteminfo$itemlabel, for_iteminfo$spmid)
    return(for_iteminfo)
}

prep_itemabrev <- function(qop_data){
    for_iteminfo <- qop_data[["spm"]]
    for_iteminfo <- for_iteminfo[,
        c("spmid",
        "kortform",
        "blokk",
        "spmnr"
    )]

    for_iteminfo <- unique(for_iteminfo)
    for_iteminfo$itemlabel <- for_iteminfo$kortform
    for_iteminfo$itemlabel <- gsub("\\D", "", for_iteminfo$itemlabel)
    for_iteminfo$itemlabel <- paste0(
        substr(for_iteminfo$kortform, 1, 3), 
        for_iteminfo$itemlabel, "-D",
        for_iteminfo$blokk, "-S",
        for_iteminfo$spmnr
    )

    for_iteminfo <- split(for_iteminfo$itemlabel, for_iteminfo$spmid)
    return(for_iteminfo)
}
