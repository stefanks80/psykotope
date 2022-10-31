plot_mc_mr <- function(
    mc_data,
    candidate_id = "kandnr",
    candidate_level = "score_level",
    candidate_level_label = "score_level_label",
    resp_choices = "alt_letters",
    item_alternatives = NULL,
    item_id = "spmid",
    distr_palette = c(
        "#56B4E9", "#E69F00", "#F0E442",
        "#0072B2", "#D55E00", "#CC79A7",
        "#999999"),
    corr_palette = c(
        "#238B45", "#41AB5D", "#74C476",
        "#A1D99B", "#C7E9C0"),
    out_dir = NULL
    ) {
        plot_title <- unique(mc_data[, item_id])
        plot_filename <- paste0(out_dir, "/item", plot_title, ".pdf")

        alternatives_ordered <- sort(unique(mc_data[, resp_choices]))
        alternatives_correct <- grep("\\*", alternatives_ordered, value=TRUE)
        alternatives_notchosen <- grep("\\#", alternatives_ordered, value=TRUE)
        alternatives_wrong <- alternatives_ordered  %in%
            c(alternatives_correct, alternatives_notchosen)

        alternatives_wrong <- alternatives_ordered[!alternatives_wrong]

        n_correct <- length(alternatives_correct)
        n_wrong <- length(alternatives_ordered)-n_correct

        correct_color <- corr_palette[1:n_correct]
        wrong_color <- distr_palette[1:n_wrong]

        alternatives_palette <- rev(c(correct_color, wrong_color))

        mc_data$candidate_groups <- factor(mc_data[, candidate_level_label])
        mc_data$response_fact <- ordered(
            mc_data[, resp_choices],
            levels = rev(c(
                alternatives_correct,
                alternatives_wrong,
                alternatives_notchosen))
                )

        ggplot_mc <- ggplot2::ggplot(mc_data,
            aes(
                x=candidate_groups,
                fill=response_fact)
                ) + 
            geom_bar(position=position_fill(), width=.5) +
            scale_fill_manual(
                drop = FALSE, 
                guide = guide_legend(title = NULL, reverse = TRUE),
                values = alternatives_palette
                ) +
            scale_x_discrete("Prestasjonsnivå") +
            scale_y_continuous("Andel valgte alternativer",
                labels = scales::percent) +
            theme16

        pdf(plot_filename, width=5, height=6)
            print(ggplot_mc)
        graphics.off()
}