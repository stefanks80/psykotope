generate_indexcard_mc <- function(
    itemid = NULL, # id in the corresponding dataset
    fig_path = NULL, # Where to find figures
    tex_path = NULL, # Where to write output
    itemstem_text = NULL, # List with each element beeing the itemstem
    item_options_text = NULL, # List with each element beeing the item options
    itemtab = NULL, # List with each element beeing the item response tab
    item_label = NULL, # List with each element beeing item info
    item_abrev = NULL, # List with each element beeing item info
    cttstats = NULL, # List with each element beeing the item stats
    owndoc = TRUE,
    tex_command = "pdflatex.exe") { 
        # TeX file path
        filcard_tex <- paste0(tex_path, "/indexcard", itemid, ".tex")

        # Item text and options
        printtext <- itemstem_text[[itemid]]
        print_alttext <- item_options_text[[itemid]]

        # Distractor plot
        plotpath <- paste0(fig_path, "/item-", itemid, ".pdf")

        # Print TeX Table
        textab <- itemtab[[itemid]]

        tex_tab <- print(xtable(textab, align ="lcccc"),
            booktabs = TRUE,
            table.placement = "H",
            include.rownames = TRUE,
            foating = TRUE,
            size = "footnotesize",
            hline.after = c(-1, 0, nrow(textab)))

        # Basic CTT stats
        print_stat <- cttstats[[itemid]]
        print_stat <- paste0(
            "Gjennomsittsskår: ",
            print_stat$diff,
            "\\\\ Korrelasjon (Pearson) med eksamensskår: ",
            print_stat$disc_pears,
            "\\\\ Korrelasjon (Spearman) med eksamensskår: ",
            print_stat$disc_pears
            )

        # Generate TeX File
        sink(filcard_tex)
            if (owndoc == TRUE) cat(texHeader1, "\n")
            if (owndoc == TRUE) cat(texHeader2, "\n")
            if (owndoc == TRUE) cat("\\thispagestyle{empty}", "\n")
            cat("\\section{", item_abrev[[itemid]], "} ")
            cat("\\label{sec:", itemid, "} \n\r")
            cat("\\leftframe{")
            cat("\\scriptsize\\textbf{",
                item_label[[itemid]],
                "} \\vspace{1em} \\small \\\\")
            cat(printtext, "\\\\ --- \\\\ \n")
            cat(print_alttext, "\n")
            cat("}\n\r \\normalsize \n\r")
            cat("\\subsubsection*{Svarmønster}\n\r")
            cat("\\begin{minipage}[t]{1\\textwidth}")
            cat("\\centering\n\r")
            cat("\\begin{minipage}[c]{.4\\textwidth}\n\r")
            cat('\\includegraphics[width=\\textwidth]{"',
                plotpath, '"}',
                sep = "")
            cat("\\end{minipage} \\hspace{2em}")
            cat("\\begin{minipage}[c]{.5\\textwidth}")
            cat("\n\r", tex_tab, "")
            cat("\\end{minipage} \n\r")
            cat("\\end{minipage}")
            cat("\\subsubsection*{Spørsmålstatistikk}\n\r")
            cat(print_stat, "\n\r")
            if(owndoc == TRUE) cat("\\end{document}")
        sink()

        if (owndoc == TRUE) {
            origwd <- getwd()
            setwd(tex_path)
            sys_com <- paste0('"', tex_command, '" ', ' "', filcard_tex, '"')
            system(sys_com)
            setwd(origwd)
        }
}