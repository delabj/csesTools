#' clean household_size
#' @param data a data frame from CSES
#' @param var_name the name of the column with household_size
#'
#' @export
clean_household_size <- function(data, var_name){
    var_name <- ggplot2::enquo(var_name)
    data %>%
        dplyr::mutate({{var_name}} :=  case_when(
            {{var_name}} <= 90 ~ as.character({{var_name}}),
            {{var_name}} == 97 ~ "VOLUNTEERED: REFUSED",
            {{var_name}} == 97 ~ "VOLUNTEERED: DON'T KNOW",
            TRUE ~ NA_character_
        ))
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




calc_dominant_party_family <- function(
    data,
    party_A_vote_share,
    party_B_vote_share,
    party_C_vote_share,
    party_D_vote_share,
    party_E_vote_share,
    party_F_vote_share,
    party_A_family,
    party_B_family,
    party_C_family,
    party_D_family,
    party_E_family,
    party_F_family
    ){
    data %>%
        mutate(idol_dom_party = case_when(
            # Party A pct share > than all others
            {{party_A_vote_share}} > {{party_B_vote_share}} &
            {{party_A_vote_share}} > {{party_C_vote_share}} &
            {{party_A_vote_share}} > {{party_D_vote_share}} &
            {{party_A_vote_share}} > {{party_E_vote_share}} &
            {{party_A_vote_share}} > {{party_F_vote_share}} ~ {{party_A_family}},
            # Party B pct share > than all others
            {{party_B_vote_share}} > {{party_A_vote_share}} &
            {{party_B_vote_share}} > {{party_C_vote_share}} &
            {{party_B_vote_share}} > {{party_D_vote_share}} &
            {{party_B_vote_share}} > {{party_E_vote_share}} &
            {{party_B_vote_share}} > {{party_F_vote_share}} ~ {{party_B_family}},
            # Party C pct share > than all others
            {{party_C_vote_share}} > {{party_B_vote_share}} &
            {{party_C_vote_share}} > {{party_A_vote_share}} &
            {{party_C_vote_share}} > {{party_D_vote_share}} &
            {{party_C_vote_share}} > {{party_E_vote_share}} &
            {{party_C_vote_share}} > {{party_F_vote_share}} ~ {{party_C_family}},
            # Party D pct share > than all others
            {{party_D_vote_share}} > {{party_B_vote_share}} &
            {{party_D_vote_share}} > {{party_C_vote_share}} &
            {{party_D_vote_share}} > {{party_A_vote_share}} &
            {{party_D_vote_share}} > {{party_E_vote_share}} &
            {{party_D_vote_share}} > {{party_F_vote_share}} ~ {{party_D_family}},
            # Party E pct share > than all others
            {{party_E_vote_share}} > {{party_B_vote_share}} &
            {{party_E_vote_share}} > {{party_C_vote_share}} &
            {{party_E_vote_share}} > {{party_D_vote_share}} &
            {{party_E_vote_share}} > {{party_A_vote_share}} &
            {{party_E_vote_share}} > {{party_F_vote_share}} ~ {{party_E_family}},
            # Party F pct share > than all others
            {{party_F_vote_share}} > {{party_B_vote_share}} &
            {{party_F_vote_share}} > {{party_C_vote_share}} &
            {{party_F_vote_share}} > {{party_D_vote_share}} &
            {{party_F_vote_share}} > {{party_E_vote_share}} &
            {{party_F_vote_share}} > {{party_A_vote_share}} ~ {{party_F_family}},
            TRUE ~ NA_character_
        ))

}




