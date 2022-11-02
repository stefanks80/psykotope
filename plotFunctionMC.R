plot_mc_mr <- function(
    mc_data,
    candidate_id = "kandnr",
    candidate_level = "score_level",
    candidate_level_label = "score_level_label",
    resp_choices = "alt_letters",
    answer_key_df = NULL,
    answer_key_letters = "alt_letters",
    answer_key_correct = "korrekt",
    item_id = "spmid",
    distr_palette = c(
        "#F0E442", "#56B4E9", "#E69F00",
        "#0072B2", "#D55E00", "#CC79A7",
        "#999999"),
    corr_palette = c(
        "#238B45", "#41AB5D", "#74C476",
        "#A1D99B", "#C7E9C0"),
    out_dir = NULL
    ) {
        plot_title <- unique(mc_data[, item_id])
        plot_filename <- paste0(out_dir, "/item-", plot_title, ".pdf")

        answer_alt <- answer_key_df[, answer_key_letters]
        answer_key <- answer_key_df[, answer_key_correct]

        alternatives_ordered <- sort(unique(answer_alt))
        alternatives_correct <- alternatives_ordered[answer_key]
        alternatives_wrong <- alternatives_ordered[!answer_key]

        n_correct <- length(alternatives_correct)
        n_wrong <- length(alternatives_wrong)

        mc_data$response_fact <- ordered(
            mc_data[, resp_choices],
            levels = rev(c(
                alternatives_correct,
                alternatives_wrong, "#")), 
            labels = rev(c(
                paste0("*", alternatives_correct),
                alternatives_wrong, "#"))
                )

        # Make grouping factor
        mc_data$candidate_groups <- factor(mc_data[, candidate_level_label])

        # Determine colour palette for graph
        correct_color <- rev(corr_palette[1:n_correct])
        wrong_color <- distr_palette[1:n_wrong]
        missing_color <- "#ffffff"
        alternatives_palette <- c(correct_color, wrong_color, missing_color)
        alternatives_palette <- rev(alternatives_palette)

        # Make plot
        ggplot_mc <- ggplot2::ggplot(mc_data, aes(x = candidate_groups,
                fill = response_fact)
                ) + 
            geom_bar(position=position_fill(), width=.5) +
            scale_fill_manual(
                drop = FALSE, 
                guide = guide_legend(title = NULL, reverse = TRUE, nrow = 2),
                values = alternatives_palette
                ) +
            scale_x_discrete("Prestasjonsgruppe") +
            scale_y_continuous("Andel valgte alternativer",
                labels = scales::percent) +
            theme16 

        pdf(plot_filename, width=5, height=6)
            print(ggplot_mc)
        graphics.off()
}

