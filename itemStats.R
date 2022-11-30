calc_ctt_item <- function(item_df, 
    sumscore_var = "sum_score", 
    itemscore_var = "item_score", 
    item_score_max = 1){

    item_calc <- item_df[, c(sumscore_var, itemscore_var)]
    item_calc$item_scaled <- item_df[, itemscore_var] / item_score_max

    diff <- mean(item_calc$item_scaled, na.rm = TRUE)*100
    diff <- round(diff, 1)

    disc_spear <- tryCatch(
        cor( 
            item_calc$item_scaled, 
            item_df[, sumscore_var], 
            method = "spearman"),
            warning = function(...) return(0)
    )


    disc_spear <- round(disc_spear, 2)

    disc_pears <- tryCatch(
        cor( 
            item_calc$item_scaled, 
            item_df[, sumscore_var], 
            method = "pearson"),
            warning = function(...) return(0)
    )

    disc_pears <- round(disc_pears, 2)

    out <- data.frame(
        diff = diff, 
        disc_pears = disc_pears, 
        disc_spear = disc_spear)

    return(out)
}

kappa_calc <- function(kappa_df, score_range = 0:6){
    rater1 <- ordered(kappa_df$score1, levels = score_range) 
    rater2 <- ordered(kappa_df$score2, levels = score_range) 

    kappa_out <- psych::cohen.kappa(table(rater1, rater2))

    out <- data.frame(
        kappa = round(kappa_out$kappa, 2), 
        wkappa = round(kappa_out$weighted.kappa, 2)
    )
    return(out)
}