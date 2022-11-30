
tabfunc_mc_mr <- function(
    mc_tab,
    item_id = "spmid",
    candidate_level_label = "score_level_label",
    resp_choices = "alt_letters",
    answer_key_df = NULL,
    answer_key_letters = "alt_letters",
    answer_key_correct = "korrekt"
    ) {
        tab_title <- as.character(unique(mc_tab[, item_id]))

        answer_alt <- answer_key_df[, answer_key_letters]
        answer_key <- answer_key_df[, answer_key_correct]

        alternatives_ordered <- sort(unique(answer_alt))
        alternatives_print <- sort(unique(answer_alt))

        alternatives_print[answer_key] <-
            paste0(alternatives_print[answer_key], "*")

        mc_tab[, resp_choices] <- ordered(mc_tab[, resp_choices],
            levels = alternatives_ordered,
            labels = alternatives_print)

        textab <- table(mc_tab[, resp_choices], 
            mc_tab[, candidate_level_label])

        return(textab)
}

