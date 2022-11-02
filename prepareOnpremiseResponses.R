add_responses <- function(qop_data) {
    data_spm <- qop_data[["spm"]]
    data_spm <- data_spm[, c("spmid", "altnr", "korrekt", "opsjnr")]

    data_svar <- qop_data[["svar"]]

    data_svar_list <- split(data_svar, data_svar$kandnr)

    add_responses <- mapply( # merge possible responses to given responses
        function(candidate_responses, possible_resp) {
            merge(candidate_responses, possible_resp, all = TRUE)},
            data_svar_list,
            MoreArgs = list(data_spm), SIMPLIFY = FALSE)

    add_responses <- mapply(function(res_data, kand_ids) {
        res_data$kandnr <- kand_ids
        return(res_data)},
        add_responses, names(add_responses), SIMPLIFY = FALSE)

    add_responses <- do.call("rbind", add_responses)
    add_responses <- add_responses[,
        c("spmid", "altnr", "kandnr", "kar", "type", "korrekt")]

    add_responses$response <- ifelse(is.na(add_responses$kar), 0, 1)
    add_responses$korrekt <- as.numeric(as.logical(add_responses$korrekt))

    add_responses$for_split <- paste0(add_responses$spmid,
      "-", add_responses$kandnr)

    add_responses <- split(add_responses, add_responses$for_split)

    fill_function <- function(resp) { 
        resp$kar <- unique(na.omit(resp$kar))
        resp$type <- unique(na.omit(resp$type))
        return(resp)
    }

    add_responses <- lapply(add_responses, fill_function)
    add_responses <- do.call("rbind", add_responses)

    row.names(add_responses) <- NULL
    add_responses$for_split <- NULL

    add_responses[, "alt_letters"] <- "#"
    add_responses[add_responses$altnr >= 0, "alt_letters"] <-
        letters[add_responses[add_responses$altnr >= 0, "altnr"] + 1]

    non_response_to_correct <- 
        (add_responses$korrekt %in% 1) & (add_responses$response %in% 0)

    add_responses[non_response_to_correct, "alt_letters"] <- 
        paste0("#", add_responses[non_response_to_correct, "alt_letters"])

    add_responses$sel_mr <- ifelse(
        (add_responses$response %in% 0 &  add_responses$korrekt %in% 1) |
        (add_responses$response %in% 1),
         1, 0)

    add_responses <- add_responses[
        ((add_responses$sel_mr %in% 1) & (add_responses$type %in% "MR")) |
        ((add_responses$response %in% 1) & (!add_responses$type %in% c("MR"))),
        ]

    add_responses$sel_mr <- NULL

    add_responses <- add_responses[, c(
      "spmid", "altnr", "kandnr", "type",
      "korrekt", "response", "alt_letters")]

    correct_answers <- (add_responses$korrekt %in% 1) & 
      (add_responses$response %in% 1)

    add_responses[correct_answers, ]$alt_letters <- 
      paste0("*", add_responses[correct_answers, ]$alt_letters)

    add_responses <- add_responses[!add_responses$type %in% "ESSAY", ]
    add_responses <- unique(add_responses)
    return(add_responses)
}