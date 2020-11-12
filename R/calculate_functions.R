#' Calculate Labor Force Status
#'
#' @param data A data frame
#' @param var_name Name of column with employment status
#' @param new_name Name of new column
#' @param values_in Vector of values considered in the labor force
#' @param values_out Vector of values considered out of the labor force
#'
#' @return a tbl with a new column for labor force
#'
#' @export
calc_labor_force_status <- function(data, var_name, new_name,
                                    values_in = c(1,2,3,4,5),
                                    values_out = c(6,7,8,9,10,11,12)){
    var_name <- ggplot2::enquo(var_name)

    data %>%
        dplyr::mutate(!!new_name :=  dplyr::case_when(
            !!var_name %in% values_in  ~ "In",
            !!var_name %in% values_out ~ "Not In" ,
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


#' Calculate Dominante Party Family
#'
#' @param data a data frame
#'
#' @export
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
