best <- function(state, outcome) {
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses="character")
        
        ## Check that state and outcome are valid
        if (!any(df$State == state)) {
                stop("invalid state")   
        }
        
        l_outcomes <- c("heart attack", "heart failure", "pneumonia")
        if (!any(outcome == l_outcomes)) {
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        df[,11] <- suppressWarnings(as.numeric(df[,11]))
        df[,17] <- suppressWarnings(as.numeric(df[,17]))
        df[,23] <- suppressWarnings(as.numeric(df[,23]))
        
        df_state <- df[df$State==state,]
        
        if (outcome == l_outcomes[1]) {
                df_state <- df_state[!is.na(df_state[,11]),]
                min <- min(df_state[,11],na.rm = TRUE)
                hosp <- df_state[df_state[,11]==min,"Hospital.Name"]
        } else if (outcome == l_outcomes[2]) {
                df_state <- df_state[!is.na(df_state[,17]),]
                min <- min(df_state[,17],na.rm = TRUE)
                hosp <- df_state[df_state[,17]==min,"Hospital.Name"]
        } else if (outcome == l_outcomes[3]) {
                df_state <- df_state[!is.na(df_state[,23]),]
                min <- min(df_state[,23],na.rm = TRUE)
                hosp <- df_state[df_state[,23]==min,"Hospital.Name"]
        }
        
        if (length(hosp)>1) {
                hosp <- sort(hosp)
        }
        hosp[1]
}