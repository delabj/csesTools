
#' Reduce the factor levels of employment status
#' @param data a data frame from CSES
#' @param var_name the name of the column with employment status
#' @export
reduce_employment_status <- function(data, var_name){
    var_name <- ggplot2::enquo(var_name)

    data %>%
        dplyr::mutate(
            !!var_name :=  forcats::fct_other(
                !!var_name,
                keep = c(
                    "Full Time (32+ hours)",
                    "Part Time (< 15 hours)",
                    "Part Time (15 - 32 hours)"
                    )
                )
            ) %>%
        dplyr::mutate(
            !!var_name := forcats::fct_collapse(
                !!var_name,
                "Full Time" = c(
                    "Full Time (32+ hours)"
                    ),
                "Part Time" = c(
                    "Part Time (< 15 hours)",
                    "Part Time (15 - 32 hours)"
                    ),
                "Other" = c(
                    "Other"
                    )
                )
        )
    }
