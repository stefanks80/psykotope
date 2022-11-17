plot_ess <- function(
    ess_item, 
    max_score = 6, 
    itemid = "spmid", 
    out_dir = NULL) {
        plot_title <- unique(ess_item[, itemid])
        plot_filename <- paste0(out_dir, "/item-", plot_title, ".pdf")

        ### Discrimination graph (Corr with total score) #######
        discr_ess_plot <- ggplot(ess_item,
            aes(x = score_level_label)) + 
                stat_summary(aes(y = item_score/max_score),
                        fun = "mean",
                        geom = "bar",
                        width = .5) +
                scale_x_discrete("Prestasjonsgruppe") +
                scale_y_continuous("Andel mulige poeng",
                        labels = scales::percent) +
                theme16

        pdf(plot_filename, width = 5, height = 5.5)
            print(discr_ess_plot)
        graphics.off()

        ### Rater descrepancies ################################
        sens_diff_plot <- ggplot(ess_item, aes(x = sens_diff)) +
            geom_bar(aes(y = (..count..)/sum(..count..))) +
            scale_x_continuous("Differanse", breaks = -6:6) +
            scale_fill_brewer() +
            coord_cartesian(
                xlim = c(-7, 7),
                ylim = c(0, 1)) +
            scale_y_continuous("Andel", 
                labels = scales::percent, 
                breaks = seq(0, 1, by=0.1)) +
            theme16

        plot_filename <- paste0(out_dir, "/sensdiff-", plot_title, ".pdf")
        pdf(plot_filename, width = 5, height = 5.5)
            print(sens_diff_plot)
        graphics.off()

        ### Score distribution ################################
        ford_ess_plot <- ggplot(ess_item, aes(x = item_score)) +
            geom_bar(aes(y = (..count..)/sum(..count..))) +
            scale_x_continuous("Skaar per kandidat", breaks = 0:6) +
            scale_fill_brewer() +
            coord_cartesian(
                xlim = c(0, 6),
                ylim = c(0, 1)) +
            scale_y_continuous("Andel", 
                labels = scales::percent, 
                breaks = seq(0, 1, by=0.1)) +
            theme16

        plot_filename <- paste0(out_dir, "/essfordel-", plot_title, ".pdf")
        pdf(plot_filename, width = 5, height = 5.5)
            print(ford_ess_plot)
        graphics.off()

        ### Rater alignment ################################
        align_data <- ess_item
        align_data <- align_data[, c("kandnr", "score1", "score2", "sens_diff")]

        align_long <- reshape(align_data, 
            direction = "long", 
            idvar = c("kandnr", "sens_diff"), 
            varying = c("score1", "score2"), 
            v.names = c("score"))

        align_long <- align_long[order(align_long$kandnr), ]

        align_long$score_jitter <- jitter(align_long$score)
        align_long$sens_diff <- factor(abs(align_long$sens_diff))
        npal <- length(levels(align_long$sens_diff))

        pal <- RColorBrewer::brewer.pal(6, "Set1")
        pal <- rev(pal)

        samsvar_plot <- ggplot(align_long, aes(
                x = time, 
                y = score_jitter, 
                group = kandnr,
                colour = sens_diff)) +
            geom_point(size = 3) + 
            geom_line(size = 1.1) +
            scale_x_continuous(name = "", 
                breaks = c(1, 2), 
                labels = c("Sensor 1", "Sensor 2")) +
            scale_y_continuous(name = "Skaar per kandidat", breaks = 0:6) +
            scale_colour_manual(values = pal) +
            theme16 + 
            theme(legend.position = "none")

        plot_filename <- paste0(out_dir, "/samsvar-", plot_title, ".pdf")
        pdf(plot_filename, width = 5, height = 5.5)
            print(samsvar_plot)
        graphics.off()
}