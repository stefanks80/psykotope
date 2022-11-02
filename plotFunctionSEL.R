
out_dir <- exam_path

for(i in 1:length(sel_data_split)){

sel_df <- sel_data_split[[i]]

plot_title <- unique(sel_df[, item_id])
plot_filename_tf <- paste0(out_dir, "/item-", plot_title, "tf.pdf")
plot_filename_alt <- paste0(out_dir, "/item-", plot_title, "alt.pdf")

distr_palette = c(
        "#F0E442", "#56B4E9", "#E69F00",
        "#0072B2", "#D55E00", "#CC79A7",
        "#999999")

sel_true <- sel_df[sel_df$korrekt == "true", ]

sel_true$alt_label <- paste0(
    sel_true$alttekst, "\n(Korrekt: ", sel_true$opsjtekst, ")")

for_label <- unique(sel_true[, c("altnr", "alt_label")])

sel_false <- sel_df[sel_df$korrekt == "false", ]
sel_false <- merge(sel_false, for_label, all.x = TRUE)

uni_opt <- length(unique(sel_false$opsjnr))
alternatives_palette <- distr_palette[1:uni_opt]


# Plot 1
tf_plot <- ggplot(sel_df,
    aes(x = score_level_label, fill = as.factor(korrekt))) +
    geom_bar(position = position_fill(), width = .5) +
    scale_fill_manual(
        drop = FALSE,
        guide = guide_legend(title = NULL, reverse = TRUE, nrow = 1),
        values = c("#E69F00", "#238B45"),
        labels = c("galt", "riktig")
        ) +
    scale_x_discrete("Prestasjonsgruppe") +
    scale_y_continuous("Andel riktig og gale svar",
        labels = scales::percent) +
    theme16

pdf(paste0(plot_filename_tf), width = 5, height = 6)
    print(tf_plot)
graphics.off()

# Plot 2
false_resp_plot <- ggplot(sel_false,
    aes(x = score_level_label, fill = as.factor(opsjtekst))) +
        geom_bar(position = position_fill(), width = .5) +
        scale_fill_manual(
            drop = FALSE,
            guide = guide_legend(title = NULL, reverse = TRUE, nrow = 1),
            values = alternatives_palette
            ) +
        facet_wrap(~alt_label, ncol = 4) +
        scale_y_continuous("Andel valgte gale alternativer",
            labels = scales::percent) +
        theme16

pdf(paste0(plot_filename_alt), width = 18, height = 8)
    print(false_resp_plot)
graphics.off()
}