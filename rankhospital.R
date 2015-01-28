#'
#' robertgalderman@gmail.com
#' jan 2015
#'
#' Various functions for working on data from an outcome-of-care-measures dataset
#' provided by US Dept of Health and Human Services. The functions sort the data
#' by mortality rates across 3 outcomes (heart attack, heart failure, and pneumonia)
#' and rank hospitals by their respective mortality rate.
#'


#'
#' Returns the column index (from outcome-of-care-measures.csv) of the 
#' "Hospital 30 day mortality rate from {outcome}" column for the given outcome.
#'
#' @param outcome one of either "heart attack", "heart failure", or "pneumonia"
#'
#' @return the respective column index for the given outcome
#'
getMortalityColumnIndex <- function(outcome) {

    # map outcome parm to respective column index
    colIdx <- list("heart attack"=11, "heart failure"=17, "pneumonia"=23)

    # validate outcome parm
    if (length(colIdx[[outcome]]) == 0) {
        # Note: I would normally include the invalid value in the message, 
        # but the assignment instructions are explicit about the message to use.
        stop("invalid outcome")  
    }

    colIdx[[outcome]]
}


#' 
#' Validate the given state parm by checking if the outcomes.dd data frame
#' contains data for that state.  This is a way to verify the state parameter
#' without making any assumptions about the set of valid states.  The state
#' is valid if there is data for it.
#'
#' @param outcomes.dd the outcomes data frame
#' @param state the state abbreviation (e.g "AZ") to verify
#'
#' @return true if outcomes.dd contains data for the given state 
#'
validateParmState <- function(outcomes.dd, state) {
    length(outcomes.dd[outcomes.dd$State == state,1]) > 0
}


#'
#' Returns the hospital with the lowest mortality rate in the given 
#' state for the given outcome parameter
#'
#' @param state the state abbreviation (e.g. "AZ")
#' @param outcome one of either "heart attack", "heart failure", or "pneumonia"
#'
#' @return the name of the hospital with the lowest mortality rate 
#'
best <- function(state, outcome) {

    # map outcome parm to respective column index
    colIdx <- getMortalityColumnIndex(outcome)

    # read the data
    outcomes.dd <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    # validate state parm
    if (validateParmState(outcomes.dd, state) == FALSE) {
        stop("invalid state")
    }

    # filter on state
    state.outcomes.dd <- outcomes.dd[outcomes.dd$State == state,]

    sorted <- sortByOutcome(state.outcomes.dd, colIdx)

    # return the first element
    sorted[[1,1]]
}


#'
#' Given an outcomes data frame and a column index to sort by, return a sorted data frame
#' with 2 cols: hospital.name and the sorted column
#'
#' @param outcomes.dd a (possibly filtered) data frame read from outcome-of-care-measures.csv
#' @param colIdx the column index to sort by.  Note: the column data will be coerced to numeric 
#' @param na.rm=FALSE optionally remove NA values from the result set
#'
#' @return a data frame with 2 cols, hospital.name and colIdx, sorted by colIdx
#'
sortByOutcome <- function(outcomes.dd, colIdx, na.rm=FALSE) {

    # convert the outcome column (colIdx) to numeric
    mortalityRates <- suppressWarnings( as.numeric(outcomes.dd[[ colIdx ]]) )

    # sort by given outcome column, then by hospital name (col 2)
    sorted <- outcomes.dd[order( mortalityRates, outcomes.dd[[2]] ), 
                                c(2, colIdx) ]

    if (na.rm) {
        f <- !is.na( suppressWarnings( as.numeric( sorted[,2] ) ) )
        sorted[f, ]
    } else {
        sorted
    }

}


#'
#' @param sorted a sorted data frame where col 1 is hospital.name 
#' @param num the desired ranking ("best", "worst", or a numeric ranking)
#'
#' @return the hospital.name (col 1) from sorted.dd at the given rank (num).
#'         or NA if the ranking does not exist.
#'
getHospitalWithRank <- function(sorted, num) {
    # coerce num into an ordered rank value 
    rank <- if (num == "best") 1 else if (num == "worst") nrow(sorted) else as.numeric(num)

    # return the hospital at the given rank
    if (rank > nrow(sorted)) {
        NA
    } else {
        sorted[rank,1]
    }
}



#'
#' Given a state, outcome, and ranking (num), returns the name of the hospital 
#' in that state that has the given ranking in terms of mortality rate for the
#' given outcome.
#'
#' @param state the state abbreviation (e.g. "AZ")
#' @param outcome one of either "heart attack", "heart failure", or "pneumonia"
#' @param num the desired ranking ("best", "worst", or a numeric ranking)
#'
#' @return the name of the hospital with the given ranking 
#'
rankhospital <- function(state, outcome, num = "best") {

    # map outcome parm to respective column index
    colIdx <- getMortalityColumnIndex(outcome)

    # read the data
    outcomes.dd <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    # validate state parm
    if (validateParmState(outcomes.dd, state) == FALSE) {
        stop("invalid state")
    }

    # filter on state
    state.outcomes.dd <- outcomes.dd[outcomes.dd$State == state,]

    # sort the data by the given outcome
    sorted <- sortByOutcome(state.outcomes.dd, colIdx, na.rm=T)

    getHospitalWithRank(sorted, num)

}


#'
#' Given an outcome and a ranking, return a list of hospitals across all states
#' that have the given ranking for the given outcome
#'
#' @param outcome one of either "heart attack", "heart failure", or "pneumonia"
#' @param num the desired ranking ("best", "worst", or a numeric ranking)
#'
#' @return a data frame with 2 cols: hospital and state, where each hospital has
#'         the given ranking for the given outcome in its respective state
#'
rankall <- function(outcome, num = "best") {

    # map outcome parm to respective column index
    colIdx <- getMortalityColumnIndex(outcome)

    # read the data
    outcomes.dd <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    # split by state
    outcomes.by.state <- split(outcomes.dd, outcomes.dd$State)

    # allocate vectors for hospital and state to be put in the data frame later
    hospitals <- vector(length = length(outcomes.by.state))
    states <- vector(length = length(outcomes.by.state))

    # For each state, find the hospital of the given rank
    for (i in seq_along(outcomes.by.state)) {

        # sort the data by the given outcome
        sorted <- sortByOutcome(outcomes.by.state[[i]], colIdx, na.rm=T)

        hospitals[i] <- getHospitalWithRank(sorted, num)
        states[i] <- names(outcomes.by.state)[i]
    }

    # Return a data frame with the hospital names and the (abbreviated) state name
    data.frame(hospital=hospitals, state=states)
}

