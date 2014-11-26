rankhospital <- function(state, outcome, num = "best") {
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
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        n_outcomes <- c(11,17,23)
        col <- n_outcomes[match(outcome,l_outcomes)]
        
        df_state <- df[df$State==state,]
        
        df_state[,col] <- suppressWarnings(as.numeric(df_state[,col]))
        df_state <- df_state[!is.na(df_state[,col]),]
        df_state2 <- df_state[order(df_state$Hospital.Name),]
        
        if (num == "best") {
                num <- 1
        } else if (num == "worst") {
                num <- nrow(df_state2)
        } else if (num > nrow(df_state2)) {
                hosp <- "NA"
        }  
        
        
        rank <- rank(df_state2[,col],ties.method = "first")
        hosp <- df_state2[match(num,rank),"Hospital.Name"]
        hosp
}