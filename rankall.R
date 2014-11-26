rankall <- function(outcome, num = "best") {
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses="character")
        
        l_outcomes <- c("heart attack", "heart failure", "pneumonia")
        if (!any(outcome == l_outcomes)) {
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        n_outcomes <- c(11,17,23)
        col <- n_outcomes[match(outcome,l_outcomes)]
        
        states <- sort(unique(df$State))
        hosp <- data.frame()
        hospital <- character()
        
        for (state in states) {
                df_state <- df[df$State==state,]
                
                df_state[,col] <- suppressWarnings(as.numeric(df_state[,col]))
                df_state <- df_state[!is.na(df_state[,col]),]
                df_state2 <- df_state[order(df_state$Hospital.Name),]
                
                if (num == "best") {
                        num2 <- 1
                } else if (num == "worst") {
                        num2 <- nrow(df_state2)
                } else if (num > nrow(df_state2)) {
                        hospital <- c(hospital, "NA")
                        next
                } else {
                        num2 <- num
                } 
                
                rank <- rank(df_state2[,col],ties.method = "first")
                hospital <- c(hospital,df_state2[match(num2,rank),"Hospital.Name"])
        }
        hosp <- data.frame(hospital,states)
        row.names(hosp) <- states
        names(hosp) <- c("hospital","state")
        hosp
}