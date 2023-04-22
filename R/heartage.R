#' to calculate the heartage
#'
#' @param age numeric
#' @param hdl numeric
#' @param tc numeric
#' @param sbp numeric
#' @param treat chr
#' @param smoker chr
#' @param DM chr

female <- function(age, hdl, tc, sbp, treat, smoker, DM) {

    sbp_not=ifelse( treat %in% c("Yes", "yes","YES"),NA,sbp)
    sbp_treat=ifelse( treat %in% c("Yes", "yes","YES"),sbp,NA)

    point <- sum(
        dplyr::case_when(
            age > 30 & age <= 34 ~ 0,
            age <= 39 ~ 2,
            age <= 44 ~ 4,
            age <= 49 ~ 5,
            age <= 54 ~ 7,
            age <= 59 ~ 8,
            age <= 64 ~ 9,
            age <= 69 ~ 10,
            age <= 74 ~ 11,
            TRUE ~ 0,
            F~NA
        ),
        dplyr::case_when(
            hdl >= 60 ~ -2,
            hdl >= 50 ~ -1,
            hdl >= 45 ~ 0,
            hdl >= 35 ~ 1,
            TRUE ~ 2,
            F~NA
        ),
        dplyr::case_when(
            tc < 160 ~ 0,
            tc < 200 ~ 1,
            tc < 240 ~ 3,
            tc < 280 ~ 4,
            TRUE ~ 5,
            F~NA
        ),
        dplyr::case_when(
            F~NA,
            sbp_not < 120 ~ -3,
            sbp_treat < 120 ~ -1,
            sbp_not < 130 ~ 0,
            sbp_not < 140 ~ 1,
            sbp_not < 150 | sbp_treat < 130 ~ 2,
            sbp_treat < 140 ~ 3,
            sbp_not < 160 ~ 4,
            sbp_not < 180 | sbp_treat < 150 ~ 5,
            sbp_treat < 160 ~ 6,
            TRUE ~ 7
        ),
        dplyr::case_when(
            smoker %in% c("No", "no","NO")~0,
            smoker %in% c("Yes", "yes","YES")~3,
            F ~ NA),
        dplyr::case_when(
            DM %in% c("No", "no","NO")~0,
            DM %in% c("Yes", "yes","YES")~4,
            F ~ NA)
    )
    return(point)
}

#' to calculate the heartage
#'
#' @param age numeric
#' @param hdl numeric
#' @param tc numeric
#' @param sbp numeric
#' @param treat chr
#' @param smoker chr
#' @param DM chr
#'
male <- function(age, hdl, tc, sbp, treat, smoker, DM) {
    sbp_not=ifelse( treat %in% c("Yes", "yes","YES"),NA,sbp)
    sbp_treat=ifelse( treat %in% c("Yes", "yes","YES"),sbp,NA)

    point <- sum(
        dplyr::case_when(
            age > 30 & age <= 34 ~ 0,
            age <= 39 ~ 2,
            age <= 44 ~ 5,
            age <= 49 ~ 6,
            age <= 54 ~ 8,
            age <= 59 ~ 10,
            age <= 64 ~ 11,
            age <= 69 ~ 12,
            age <= 74 ~ 14,
            TRUE ~ 15
        ),
        dplyr::case_when(
            hdl >= 60 ~ -2,
            hdl >= 50 ~ -1,
            hdl >= 45 ~ 0,
            hdl >= 35 ~ 1,
            TRUE ~ 2
        ),
        dplyr::case_when(
            tc < 160 ~ 0,
            tc < 200 ~ 1,
            tc < 240 ~ 2,
            tc < 280 ~ 3,
            TRUE ~ 4
        ),
        dplyr::case_when(
            sbp_not < 120 ~ -2,
            sbp_not < 130 | sbp_treat < 120 ~ 0,
            sbp_not < 140 ~ 1,
            sbp_not < 160 | sbp_treat < 130 ~ 2,
            sbp_not>=160  | sbp_treat < 140 ~ 3,
            sbp_treat < 160 ~ 4,
            TRUE ~ 5
        ),
        dplyr::case_when(
            smoker %in% c("No", "no","NO")~0,
            smoker %in% c("Yes", "yes","YES")~4,
            F ~ NA),
        dplyr::case_when(
            DM %in% c("No", "no","NO")~0,
            DM %in% c("Yes", "yes","YES")~3,
            F ~ NA)
    )
    return(point)
}

#' to calculate the heartage
#'
#' @param gender chr
#' @param point num
#'
#'
heartage <- function(gender, point) {
    male <- c(30, 32, 34, 36, 38,40,42,45,48,51,54,57,60,64,68,72,76,80)
    female <- c(31, 34, 36, 39,42,45,48,51,55,59,64,68,73,79,80)
    HA <- dplyr::case_when(gender == "male" & point >= 0 & point <= 17 ~ male[point + 1],
                    gender == "female" & point >= 1 & point <= 15 ~ female[point],
                    TRUE ~ NA_real_)
    return(HA)
}


#' to calculate the heartage
#'
#' @param gender chr
#' @param age numeric
#' @param hdl numeric
#' @param tc numeric
#' @param sbp numeric
#' @param treat chr
#' @param smoker chr
#' @param DM chr
#'
#' @export point_heartage
point_heartage <- function(gender, age, hdl, tc, sbp, treat, smoker, DM) {
    point <- ifelse(gender == "male", male(age, hdl, tc, sbp, sbp, smoker, DM),
                    ifelse(gender == "female", female(age, hdl, tc, sbp, sbp, smoker, DM), NA))
    HA <- heartage(gender, point)
    return(list(point,HA))
}
