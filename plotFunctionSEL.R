plot_sel <- function(
    sel_df,
    out_dir,
    item_id = NULL, 
    candidate_level_label = "score_level_nlabel",
    distr_palette = c(
        "#F0E442", "#56B4E9", "#E69F00",
        "#0072B2", "#D55E00", "#CC79A7",
        "#999999", "#cec767", "#064163",
        "#9b7b36", "#9db6c4", "#642c02",
        "#7c4061", "#292929")) { 

        # Plot Info
        plot_title <- unique(sel_df[, item_id])
        plot_filename_tf <- paste0(out_dir, "/item-", plot_title, "tf.pdf")
        plot_filename_alt <- paste0(out_dir, "/item-", plot_title, "alt.pdf")

        # Prepare data for plotting
        sel_true <- sel_df[sel_df$korrekt == "true", ]
        sel_true$alt_label <- paste0(
        sel_true$alttekst, "\n(Korrekt: ", sel_true$opsjtekst, ")")

        for_label <- unique(sel_true[, c("altnr", "alt_label")])

        sel_false <- sel_df[sel_df$korrekt == "false", ]
        sel_false <- merge(sel_false, for_label, all.x = TRUE)

        uni_opt <- length(unique(sel_false$opsjnr))
        alternatives_palette <- distr_palette[1:uni_opt]
        
        # Make grouping factor
        sel_df$candidate_groups <- factor(sel_df[, candidate_level_label])

        # Plot 1
        tf_plot <- ggplot2::ggplot(sel_df,
            aes(x = candidate_groups, fill = as.factor(korrekt))) +
            geom_bar(position = position_fill(), width = .5) +
            scale_fill_manual(
                drop = FALSE,
                guide = guide_legend(title = NULL, reverse = TRUE, nrow = 1),
                values = c("false" = "#E69F00", "true" = "#238B45"),
                labels = c("galt", "riktig")
                ) +
            scale_x_discrete("Prestasjonsgruppe") +
            scale_y_continuous("Andel riktig og gale svar",
                labels = scales::percent) +
            theme16

        pdf(paste0(plot_filename_tf), width = 5, height = 6)
            print(tf_plot)
        graphics.off()

        # Determine Dimensions for Plot 2
        ncol_plot <- 4
        plot2_width <- 18
        plot2_height <- 6
        for_pdf_props <- length(unique(sel_true$alt_label))
        if (for_pdf_props < ncol_plot) plot2_width <- 12
        if (for_pdf_props == ncol_plot) plot2_height <- 6
        if (for_pdf_props > ncol_plot) plot2_height <- 12
        if (for_pdf_props > 2 * ncol_plot) plot2_height <- 20

        # Determin legend for many options
        nrow_legend <- 1
        for_legend_row <- unique(sel_df$opsjtekst)
        for_legend_row <- length(for_legend_row)
        if (for_legend_row > 7) nrow_legend <- 2

        # Plot 2

        if(nrow(sel_false) > 0) {
            false_resp_plot <- ggplot(sel_false,
                aes(x = score_level_label, fill = as.factor(opsjtekst))) +
                    geom_bar(position = position_fill(), width = .5) +
                    scale_fill_manual(
                        drop = FALSE,
                        guide = guide_legend(title = NULL,
                            reverse = TRUE,
                            nrow = nrow_legend),
                        values = alternatives_palette
                        ) +
                    facet_wrap(~alt_label, ncol = ncol_plot) +
                    scale_y_continuous("Andel valgte gale alternativer",
                        labels = scales::percent) +
                    theme16 

            pdf(paste0(plot_filename_alt),
                width = plot2_width,
                height = plot2_height)
            print(false_resp_plot)
            graphics.off()

        }
}
