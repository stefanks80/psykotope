prepare_onpremise_sel <- function(qop_data) {
    data_spm <- qop_data[["spm"]]
    data_spm <- data_spm[data_spm$type %in% "SEL", ]
    var_sel <- c("spmid", "altnr", "korrekt", "opsjnr", "opsjtekst", "alttekst")
    data_spm <- data_spm[, var_sel]

    head(data_alt)
    data_svar <- qop_data[["svar"]]
    data_svar <- data_svar[data_svar$type %in% "SEL", ]
    var_sel <- c("kandnr", "spmid", "altnr", "opsjnr")
    data_svar <- data_svar[, var_sel]
    data_sel <- merge(data_svar, data_spm, all.x = TRUE)
    data_sel <- unique(data_sel)
    return(data_sel)
}