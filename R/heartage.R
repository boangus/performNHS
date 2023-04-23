#' Calculate the CVD risk score using Framingham model
#'
#' This function calculates the 10-year cardiovascular disease (CVD) risk score
#' using the Framingham model. The model is based on the following parameters:
#'
#'  * Age
#'  * Gender (male/female)
#'  * HDL cholesterol (HDL)
#'  * Total cholesterol (TC)
#'  * Systolic blood pressure (SBP)
#'  * Whether the patient is receiving treatment for high blood pressure (treat)
#'  * Whether the patient is a smoker (smoker)
#'  * Whether the patient has diabetes (DM)
#' @param Gender chr
#' @param Age num
#' @param HDL num
#' @param TC num
#' @param SBP num
#' @param Treat chr
#' @param Smoker chr
#' @param diabetes chr
#' @examples point("female",61,47,180,124,NA,"yes","no")
#' point("maile",61,47,180,124,NA,"yes","no")
#' df <- data.frame(
#'         Gender = c("male", "female", "male","female","male"),
#'         Age = c(55, 60, 65,61,53),
#'         HDL = c(50, 60, 70,47,55),
#'         TC = c(200, 220, 240,180,161),
#'         SBP = c(120, 130, 140,124,125),
#'         Treat = c("yes", "no", "yes","no","yes"),
#'         Smoker = c("yes", "no", "yes","yes","no"),
#'         DM = c("yes", "no", "yes","no","yes")
#'       )
#' df %>% dplyr::mutate(score = point(Gender, Age, HDL, TC, SBP, Treat, Smoker, DM))
#'
#' @return numeric The 10-year CVD risk score in percentage.
#'
#' @export point

point <- function(Gender,Age, HDL, TC, SBP, Treat, Smoker, diabetes){
    female <- function(age, hdl, tc, sbp, treat, smoker, DM) {
        # Create lookup tables
        age_table <- c(0, 2, 4, 5, 7, 8, 9, 10, 11, 12)
        hdl_table <- c(2, 1, 0, -1, -2)
        tc_table <- c(0, 1, 3, 4, 5)
        sbp_not_table <- c(-3, 0, 1, 2, 4, 5)
        sbp_tre_table <- c(-1, 2, 3, 5, 6, 7)
        smoker_table <- c(0, 3)
        DM_table <- c(0, 4)

        # Compute points for each variable
        age_points <- age_table[cut(age, c(30, 35, 40, 45, 50, 55, 60, 65, 70, Inf),right=F)]
        hdl_points <- hdl_table[cut(hdl, c(-Inf, 35, 45, 50, 60, Inf),right=F)]
        tc_points <- tc_table[cut(tc, c(-Inf, 160, 200, 240, 280, Inf),right=F)]
        sbp_points <- ifelse(treat %in% c("Yes", "yes", "YES"),
                             sbp_tre_table[cut(sbp, c(-Inf, 120, 130, 140, 150,160, Inf),right=F)],
                             sbp_not_table[cut(sbp, c(-Inf, 120, 130, 140, 150,160, Inf),right=F)]
                             )
        smoker_points <- smoker_table[ifelse(smoker %in% c("No", "no", "NO"), 1, 2)]
        DM_points <- DM_table[ifelse(DM %in% c("No", "no", "NO"), 1, 2)]

        # Compute total points
        point <- sum(age_points, hdl_points, tc_points, sbp_points, smoker_points, DM_points, na.rm = TRUE)

        return(point)
    }
    male <- function(age, hdl, tc, sbp, treat, smoker, DM) {
        # Create lookup tables
        age_table <- c(0, 2, 5, 6, 8, 10, 11, 12, 14, 15)
        hdl_table <- c(2, 1, 0, -1, -2)
        tc_table <- c(0, 1, 2, 3, 4)
        sbp_not_table <- c(-2, 0, 1, 2, 3)
        sbp_tre_table <- c( 0, 2, 3, 4, 5)
        smoker_table <- c(0, 4)
        DM_table <- c(0, 3)

        # Compute points for each variable
        age_points <- age_table[cut(age, c(30, 35, 40, 45, 50, 55, 60, 65, 70, 75, Inf),right=F)]
        hdl_points <- hdl_table[cut(hdl, c(-Inf, 35, 45, 50, 60, Inf),right=F)]
        tc_points <- tc_table[cut(tc, c(-Inf, 160, 200, 240, 280, Inf),right=F)]
        sbp_points <- ifelse(treat %in% c("Yes", "yes", "YES"),
                             sbp_tre_table[cut(sbp, c(-Inf, 120, 130, 140, 160, Inf),right=F)],
                             sbp_not_table[cut(sbp, c(-Inf, 120, 130, 140, 160, Inf),right=F)]
                             )
        smoker_points <- smoker_table[ifelse(smoker %in% c("No", "no", "NO"), 1, 2)]
        DM_points <- DM_table[ifelse(DM %in% c("No", "no", "NO"), 1, 2)]

        # Compute total points
        point <- sum(age_points, hdl_points, tc_points, sbp_points, smoker_points, DM_points, na.rm = TRUE)

        return(point)
    }
    ifelse(Gender=="female",female(Age, HDL, TC, SBP, Treat, Smoker, diabetes),
        male(Age, HDL, TC, SBP, Treat, Smoker, diabetes))
    }

#' to calculate the heartage
#'
#' @param gender chr
#' @param point num
#'
heartage <- function(gender, point) {
    male <- c(30, 32, 34, 36, 38, 40, 42, 45, 48, 51, 54, 57, 60, 64, 68, 72, 76, 80)
    female <- c(31, 34, 36, 39, 42, 45, 48, 51, 55, 59, 64, 68, 73, 79, 80)

    if (gender == "male") {
        ha <- ifelse(point >= 0 & point <= 17, male[as.integer(point) + 1], 81)
    } else if (gender == "female") {
        ha <- ifelse(point >= 1 & point <= 15, female[as.integer(point)], 81)
    } else {
        ha <- 81
    }

    return(ha)
}

heartrisk <- function(gender, point) {
    male <- c("<1","1.1","1.4","1.6","1.9","2.3","2.8","3.3","3.9","4.7","5.6","6.7","7.9","9.4","11.2","13.2","15.6","18.4","21.6","25.3","39.4",">30")
    female <- c("<1","1.2","1.5","1.7","2","2.4","2.8","3.3","3.9","4.5","5.3","6.3","7.3","8.6","10","11.7","13.7","15.9","18.5","21.5","24.8","28.5",">30")
    risk <- ifelse(gender=="female",female[cut(point, c(-Inf,-1:20, Inf))],
                   male[cut(point, c(-Inf,-2:17, Inf),right = F)])
    return(risk)
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
#' @example point_heartage(df,Gender,Age,HDL,TC,SBP,Treat,Smoker,DM)
#' @export point_heartage

point_heartage <- function(dat,gender, age, hdl, tc, sbp, treat, smoker, DM){
    dat <- dat |> dplyr::rowwise() |>
        dplyr::mutate(point=point(tolower({{gender}}),{{age}}, {{hdl}}, {{tc}}, {{sbp}}, {{treat}}, {{smoker}}, {{DM}}),
                      heartage=heartage(tolower({{gender}}),point),
                      risk=heartrisk(tolower({{gender}}),point),
                      risk_20 = ifelse(tolower({{gender}}) == "male" & point > 14, ">20%",
                                    ifelse(tolower({{gender}}) == "female" & point > 17, ">20%", "<20%")))

    return(dat)
}


