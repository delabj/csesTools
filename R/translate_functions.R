#' Translate election type encoding
#'
#' @param data a data frame from CSES
#' @param var_name the name of the column with election type
#'
#' @return a tbl
#'
#' @export
translate_election_type <- function(data, var_name){

    data %>%
        dplyr::mutate({{var_name}} := dplyr::case_when(
            {{var_name}} == 10 ~ "PARLIAMENTARY/LEGISLATIVE",
            {{var_name}} == 12 ~ "PARLIAMENTARY/LEGISLATIVE AND PRESIDENTIAL",
            {{var_name}} == 13 ~ "PARLIAMENTARY/LEGISLATIVE AND PRIME MINISTER",
            {{var_name}} == 20 ~ "PRESIDENTIAL",
            {{var_name}} == 30 ~ "HEAD OF GOVERNMENT",
            TRUE ~ NA_character_
        ))
}


#' Translate Respondent Gender
#' @param data a data frame from CSES
#' @param var_name the name of the column with gender encoding
#'
#' @return a tbl
#'
#' @export
translate_responder_gender <- function(data, var_name){

    data %>%
        dplyr::mutate({{var_name}} :=  dplyr::case_when(
            {{var_name}} == 1 ~ "Male",
            {{var_name}} == 2 ~ "Female",
            {{var_name}} == 3 ~ "Other",
            {{var_name}} == 7 ~ "VOLUNTEERED: REFUSED",
            {{var_name}} == 8 ~ "VOLUNTEERED: DON'T KNOW",
            TRUE ~ NA_character_
        )
        )
}

#' Translate education status
#' @description I've made some opinionated recoding of module 1,2,&3's coding to bring them in line with 4/5
#'
#' @param data a data frame from CSES
#' @param var_name the name of the column with gender encoding
#' @param module_var name of column with module information
#' @export
translate_education <- function(data, var_name, module_var){

    module_var <- ggplot2::enquo(module_var)
    data %>%
        dplyr::mutate({{var_name}} :=  dplyr::case_when(
            {{var_name}} == 1 & !!module_var %in% c(1,2,3) ~ "None",
            {{var_name}} == 2 & !!module_var %in% c(1,2,3) ~ "EARLY CHILDHOOD EDUCATION",
            {{var_name}} == 3 & !!module_var %in% c(1,2,3) ~ "PRIMARY",
            {{var_name}} == 4 & !!module_var %in% c(1,2,3) ~ "LOWER SECONDARY",
            {{var_name}} == 5 & !!module_var %in% c(1,2,3) ~ "UPPER SECONDARY",
            {{var_name}} == 6 & !!module_var %in% c(1,2,3) ~ "POST-SECONDARY NON-TERTIARY",
            {{var_name}} == 7 & !!module_var %in% c(1,2,3) ~ "SHORT-CYCLE TERTIARY",
            {{var_name}} == 8 & !!module_var %in% c(1,2,3) ~ "BACHELOR OR EQUIVALENT",
            {{var_name}} == 9 & !!module_var %in% c(1,2,3) ~ "Other",
            #### Scales changed on modules 4 and 5
            {{var_name}} == 1 & !!module_var %in% c(4,5)   ~ "EARLY CHILDHOOD EDUCATION",
            {{var_name}} == 2 & !!module_var %in% c(4,5)   ~ "PRIMARY",
            {{var_name}} == 3 & !!module_var %in% c(4,5)   ~ "LOWER SECONDARY",
            {{var_name}} == 4 & !!module_var %in% c(4,5)   ~ "UPPER SECONDARY",
            {{var_name}} == 5 & !!module_var %in% c(4,5)   ~ "POST-SECONDARY NON-TERTIARY",
            {{var_name}} == 6 & !!module_var %in% c(4,5)   ~ "SHORT-CYCLE TERTIARY",
            {{var_name}} == 7 & !!module_var %in% c(4,5)   ~ "BACHELOR OR EQUIVALENT",
            {{var_name}} == 8 & !!module_var %in% c(4,5)   ~ "MASTER OR EQUIVALENT",
            {{var_name}} == 9 & !!module_var %in% c(4,5)   ~ "DOCTORAL OR EQUIVALENT",
            {{var_name}} == 96 ~ "NONE",
            {{var_name}} == 97 ~ "VOLUNTEERED: REFUSED",
            {{var_name}} == 98 ~ "VOLUNTEERED: DON'T KNOW",
            TRUE ~ NA_character_
        )
        )
}

#' Translate employment status
#' @param data a data frame from CSES
#' @param var_name the name of the column with employment status
#' @export
translate_employment_status <- function(data, var_name ){


    data %>%
        dplyr::mutate({{var_name}} :=  dplyr::case_when(
            {{var_name}} == 1  ~ "Full Time (32+ hours)",
            {{var_name}} == 2  ~ "Part Time (15 - 32 hours)",
            {{var_name}} == 3  ~ "Part Time (< 15 hours)",
            {{var_name}} == 4  ~ "Helping Family",
            {{var_name}} == 5  ~ "Unemployed",
            {{var_name}} == 6  ~ "Student",
            {{var_name}} == 7  ~ "Retired",
            {{var_name}} == 8  ~ "Housewife",
            {{var_name}} == 9  ~ "Disabled",
            {{var_name}} == 10 ~ "Other",
            {{var_name}} == 11 ~ "Other",
            {{var_name}} == 12 ~ "Other",
            TRUE ~ NA_character_
        )
        )
}


#' Translate Residence Type
#' @param data a data frame from CSES
#' @param var_name the name of the column with Residence Type
#'
#' @export
translate_residence_type <- function(data, var_name){

    data %>%
        dplyr::mutate({{var_name}} :=  dplyr::case_when(
            {{var_name}} == 1 ~ "Rural",
            {{var_name}} == 2 ~ "Small or Mid-sized Town",
            {{var_name}} == 3 ~ "Suburbs",
            {{var_name}} == 4 ~ "Large Town or City",
            TRUE ~ NA_character_
        ))
}

#' Translate Satisfaction with democracy
#' @param data a data frame from CSES
#' @param var_name the name of the column with Satisfaction with democracy
#'
#' @export
translate_satisfaction <- function(data, var_name, module_var){

    module_var <- ggplot2::enquo(module_var)

    data %>%
        dplyr::mutate({{var_name}} :=  dplyr::case_when(
            #### Module 1 options (a few countries used a 5 pt scale rather than 4)
            ({{var_name}} == 1 & !!module_var == 1) ~ "VERY SATISFIED",
            ({{var_name}} == 2 & !!module_var == 1) ~ "FAIRLY SATISFIED",
            #({{var_name}} == 3 & !!module_var == 1) ~ "Neither satisfied nor unsatisfied",
            ({{var_name}} == 4 & !!module_var == 1) ~ "NOT VERY SATISFIED",
            ({{var_name}} == 5 & !!module_var == 1) ~ "NOT AT ALL SATISFIED",
            ({{var_name}} == 8 & !!module_var == 1) ~ "VOLUNTEERED: DON'T KNOW",
            #### Module 2 options (ensured a 4 pt scale)
            ({{var_name}} == 1 & !!module_var == 2) ~ "VERY SATISFIED",
            ({{var_name}} == 2 & !!module_var == 2) ~ "FAIRLY SATISFIED",
            ({{var_name}} == 3 & !!module_var == 2) ~ "NOT VERY SATISFIED",
            ({{var_name}} == 4 & !!module_var == 2) ~ "NOT AT ALL SATISFIED",
            #({{var_name}} == 6 & !!module_var == 2) ~ "Neither satisfied nor dissatisfied",
            ({{var_name}} == 7 & !!module_var == 2) ~ "VOLUNTEERED: REFUSED",
            ({{var_name}} == 8 & !!module_var == 2) ~ "VOLUNTEERED: DON'T KNOW",
            #### Module 3-5 options (ensured a 4 pt scale)
            {{var_name}} == 1 ~ "VERY SATISFIED",
            {{var_name}} == 2 ~ "FAIRLY SATISFIED",
            {{var_name}} == 4 ~ "NOT VERY SATISFIED",
            {{var_name}} == 5 ~ "NOT AT ALL SATISFIED",
            #{{var_name}} == 6 ~ "Neither satisfied nor dissatisfied",
            {{var_name}} == 7 ~ "VOLUNTEERED: REFUSED",
            {{var_name}} == 8 ~ "VOLUNTEERED: DON'T KNOW",
            TRUE ~ NA_character_
        ))
}

#' Translate Belief that Who is in power matters
#'
#' @param data a data frame with CSES columns
#' @param var_name name of column containting raw response to question of if who is in power matters
#'
#' @return a tbl
#'
#' @export
translate_power_matters <- function(data, var_name){



    data %>%
        mutate({{var_name}} := dplyr::case_when(
            {{var_name}} >0 & {{var_name}}<6 ~ {{var_name}},
            TRUE ~ NA
        ))
}


#' Translate marital status
#' @param data a data frame from CSES
#' @param var_name the name of the column with gender encoding
#'
#' @export
translate_marital_status <- function(data, var_name){

    data %>%
        dplyr::mutate({{var_name}} :=  dplyr::case_when(
            {{var_name}} == 1 ~ "MARRIED OR LIVING TOGETHER AS MARRIED",
            {{var_name}} == 2 ~ "WIDOWED",
            {{var_name}} == 3 ~ "DIVORCED OR SEPARATED",
            {{var_name}} == 4 ~ "SINGLE, NEVER MARRIED",
            {{var_name}} == 5 ~ "OTHER",
            {{var_name}} == 7 ~ "VOLUNTEERED: REFUSED",
            {{var_name}} == 8 ~ "VOLUNTEERED: DON'T KNOW",
            TRUE ~ NA_character_
        ))
}


#' Translate Party Ideological Family
#' @param data a data frame from CSES
#' @param var_name the name of the column with party_family
#'
#' @export
translate_party_family <- function(data, var_name){

    data %>%
        dplyr::mutate({{var_name}} := case_when(
            {{var_name}} <= 01 ~ "ECOLOGY PARTIES",
            {{var_name}} == 02 ~ "COMMUNIST PARTIES",
            {{var_name}} == 03 ~ "SOCIALIST PARTIES",
            {{var_name}} == 04 ~ "SOCIAL DEMOCRATIC PARTIES",
            {{var_name}} == 05 ~ "LEFT LIBERAL PARTIES",
            {{var_name}} == 06 ~ "LIBERAL PARTIES",
            {{var_name}} == 07 ~ "RIGHT LIBERAL PARTIES",
            {{var_name}} == 08 ~ "CHRISTIAN DEMOCRATIC PARTIES",
            {{var_name}} == 09 ~ "CONSERVATIVE PARTIES",
            {{var_name}} == 10 ~ "NATIONAL PARTIES",
            {{var_name}} == 11 ~ "AGRARIAN PARTIES",
            {{var_name}} == 12 ~ "ETHNIC PARTIES",
            {{var_name}} == 13 ~ "REGIONAL PARTIES",
            {{var_name}} == 14 ~ "RELIGIOUS PARTIES",
            {{var_name}} == 15 ~ "INDEPENDENT PARTIES",
            {{var_name}} == 16 ~ "OTHER PARTIES",

            TRUE ~ NA_character_



        ))
}


#' Translate Household Income
#' @param data a data frame from CSES
#' @param var_name the name of the column with Household Income
#' @export
translate_household_income <- function(data, var_name){


    data %>%
        dplyr::mutate({{var_name}} := case_when(
            {{var_name}} == 1 ~ "LOWEST HOUSEHOLD INCOME QUINTILE", #"< 22,500 USD",
            {{var_name}} == 1 ~ "SECOND HOUSEHOLD INCOME QUINTILE", #"22,500 -  44,999 USD",
            {{var_name}} == 1 ~ "THIRD HOUSEHOLD INCOME QUINTILE",  # "45,000 -  74,999 USD",
            {{var_name}} == 1 ~ "FOURTH HOUSEHOLD INCOME QUINTILE", # "75,000 - 109,999 USD ",
            {{var_name}} == 1 ~ "HIGHEST HOUSEHOLD INCOME QUINTILE",#"more than 110,000 USD",
            TRUE ~ NA_character_

        ) )
}
