#' clean household_size
#' @param data a data frame from CSES
#' @param var_name the name of the column with household_size
#' @param NA_types Should CSES varying NA types be preserved? Defaults to FALSE
#'
#' @export
clean_household_size <- function(data, var_name, NA_types = FALSE){

    if(!(NA_types %in% c(TRUE, FALSE))){
        warning("NA_types should be one of TRUE or FALSE")
        warning("Defaulting to FALSE")
    }

    if(NA_types==TRUE){
        data %>%
            dplyr::mutate({{var_name}} :=  case_when(
                {{var_name}} <= 90 ~ as.character({{var_name}}),
                {{var_name}} == 97 ~ "VOLUNTEERED: REFUSED",
                {{var_name}} == 97 ~ "VOLUNTEERED: DON'T KNOW",
                TRUE ~ NA_character_
            ))
    }else{
        data %>%
            dplyr::mutate({{var_name}} :=  case_when(
                {{var_name}} <= 90 ~ as.character({{var_name}}),
                TRUE ~ NA_character_
            ))%>%
            dplyr::mutate({{var_name}} := as.numeric({{var_name}}))
    }


}

#' Clean Left Right Scale
#'
#' This function should work for self identified left right, expert identified left right,
#' and respondent classification of all parties across all modules.
#'
#' @param data a data frame
#' @param var_name name of column with left right scale
#'
#' @export
clean_left_right_scale <- function(data, var_name){
    var_name <- ggplot2::enquo(var_name)


    data %>%
        dplyr::mutate(
            {{var_name}} :=  dplyr::case_when(
                ({{var_name}} <=10 & {{var_name}} >=0) ~ {{var_name}},
                TRUE ~ NA_real_
            )
        )
}








