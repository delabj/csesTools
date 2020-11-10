#' Calculate Labor Force Status
#'
#' @param data A data frame
#' @param var_name Name of column with employment status
#' @param new_name Name of new column
#'
#' @return a tbl with a new column for labor force
#'
#' @export
calc_labor_force_status <- function(data, var_name, new_name){
    var_name <- ggplot2::enquo(var_name)

    data %>%
        dplyr::mutate(!!new_name :=  dplyr::case_when(
            !!var_name %in% c(1,2,3,4,5)  ~ "In",
            !!var_name %in% c(6,7,8,9,10) ~ "Not In" ,
            TRUE ~ NA_character_
        ))
}


#' Calculate Voter Status
#'
#' @description If a voter voted in any of the elections (lower house, upper house, head of state)
#' the respondent is classified as `Voted`, otherwise `Did Not Vote`
#'
#' @param data a data frame
#' @param new_name name of the new column
#' @param
calc_voted <- function(
    data,
    new_name="voted",
    early_module_ballot_1,
    early_module_ballot_2,
    current_pres_1st,
    current_pres_2nd,
    current_lh
    ){

    data %>%
        mutate(!!voted := case_when(
            {{early_module_ballot_1}} == 1 ~ "Voted",
            {{early_module_ballot_2}} == 1 ~ "Voted",
            {{current_pres_1st}}      == 1 ~ "Voted",
            {{current_pres_2nd}}      == 1 ~ "Voted",
            {{current_lh}}            == 1 ~ "Voted",
            TRUE ~ "Did Not Vote"

        ))

}
