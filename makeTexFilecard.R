        print(xtable(textab, align="lcccc"), 
            booktabs=TRUE, 
            table.placement = "H", 
            include.rownames = TRUE, 
            foating = FALSE, 
            hline.after= c(-1, 0, nrow(textab)))