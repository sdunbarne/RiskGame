A <- 10
D <- 4
## A <- 40
## D <- 40

totalStates <- A * D + (A + D)

stateToIndex <- function(a,d) {
    # given a state, retrun the index for the trans prob matrix
    if (a > 0) {
        ind <- (A+1 - a) + (D-d)*A  
    }  else {
        ind <- A*D + A + d 
    }
    ind
}

indexToState <- function(ind) {
    # given an index for a transient state, return the state as a pair
    if (ind <= A*D + A) {
        j <- A*D - ind
        r <- j %% A
        first <- 1 + r
        second <- 1 + (j-r)/A
    } else {
        first <- 0
        second <- ind - (A*D + A)
    }
    c(first, second)
}

rho111 <- 15/36                          # light gray
rho110 <- 21/36                          # magenta

rho121 <- 55/216                         # brown
rho120 <- 161/216                        # purple

rho211 <- 125/216                        # pink
rho210 <- 91/216                         # black

rho222 <- 295/1296                       # cyan
rho221 <- 420/1296                       # darkgreen
rho220 <- 581/1296                       # yellow

rho311 <- 855/1296                       # orange
rho310 <- 441/1296                       # gray

rho322 <- 2890/7776                      # green
rho321 <- 2611/7776                      # blue
rho320 <- 2275/7776                      # red

QR <- matrix(0, A * D, A * D + A + D)
## R <- matrix(0, A * D, A + D)
I <- diag(A + D)
Z <- matrix(0, A + D, A * D)

att3dice <- seq(from=A, to = 3, by=-1)
def2dice <- seq(from=D, to = 2, by=-1)

# Cases 6, 5, 4
for (d in def2dice) {
    for (a in att3dice) {
        QR[stateToIndex(a,d), stateToIndex(a, d-2)] <- rho322   # green
        QR[stateToIndex(a,d), stateToIndex(a-1, d-1)] <- rho321 # blue
        QR[stateToIndex(a,d), stateToIndex(a-2, d)] <- rho320 # red
    }
    QR[stateToIndex(2, d), stateToIndex(2, d-2)] <- rho222 # cyan
    QR[stateToIndex(2, d), stateToIndex(1, d-1)] <- rho221 # darkgreen
    QR[stateToIndex(2, d), stateToIndex(0, d)] <- rho220 # yellow

    QR[stateToIndex(1, d), stateToIndex(1, d-1)] <- rho121 # purple
    QR[stateToIndex(1, d), stateToIndex(0, d)] <- rho120 # brown
}

# Case 3
for (a in att3dice) {
    QR[stateToIndex(a, 1), stateToIndex(a, 0)] <- rho311 # orange
    QR[stateToIndex(a, 1), stateToIndex(a-1, 1)] <- rho310 # gray
}

# Cases 2 and 1
QR[stateToIndex(2, 1), stateToIndex(2, 0)] <- rho211 # pink
QR[stateToIndex(2, 1), stateToIndex(1, 1)] <- rho210 # black
QR[stateToIndex(1, 1), stateToIndex(1, 0)] <- rho111 # lightgray
QR[stateToIndex(1, 1), stateToIndex(0, 1)] <- rho110 # magenta


M <- rbind(QR, cbind(Z, I))

library("markovchain")

riskChain <- new("markovchain", states = as.character(1:totalStates), byrow = TRUE, 
    transitionMatrix = M, name = "RISK")
absorpProb <- absorptionProbabilities(riskChain)
attackerWins  <- apply(absorpProb[ , 1:A], 1, sum)
attackerWinTable  <- t(matrix(formatC(attackerWins, digits=3, format="f"), A,D))

## NAME: risk.R
## USAGE: within R, at interactive prompt
##        source("risk.R")
## REQUIRED ARGUMENTS: none
## OPTIONS:  None
## DESCRIPTION: Calculate probability of attacker winning in Risk
##   given the number of attacking and defending armies.              
## DIAGNOSTICS: None
## CONFIGURATION AND ENVIRONMENT: R and markovchain package
## DEPENDENCIES: R and markovchain package
## INCOMPATIBILITIES: None known
## PROVENANCE: Steve Dunbar as of Tue 27 Oct 2020 11:19:41 AM CDT
## BUGS AND LIMITATIONS: None known
## FEATURES AND POTENTIAL IMPROVEMENTS: Make the number of armies bigger
##  Add some graphing, 
## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0 as of Tue 27 Oct 2020 11:20:37 AM CDT
## KEYWORDS: Markov chain, win probability, absorption probability


## cat(c("Attacker Win      ", "Attacker Lose", "\n"))
## cat(c(sum(absP[1, 1:A]), sum(absP[1, (A+1):(A+D)]), "\n") )

## # Calculate win-lose probabilities with normal matrix: (I-Q)^{-1} R
## Q = QR[ , 1:(A * D)]
## R = QR[ , (A * D + 1):(A * D + A + D)]   
## IsC <- diag( A * D )
## matrixProbs <- solve(IsC - Q, R)

## attackerWinLoseHist <-  c(absP[1, 1:A], absP[1, (A+1):(A+D)])
## plot(attackerWinLoseHist, type="b")

## # The remainder down below isn't working the way I expect or want it to.....
## library("tidyverse")

## defWins <- apply(absP[ , (A+1):(A+D)], 1, sum)
## cdfAttArmRem <-  t(apply( cbind(absP[ , 1:A], defWins), 1, cumsum))

## cdfAttArmRem >= 0.25 * matrix(1, A*D, A+1)

## locTRUE <- function(x) {match(TRUE, x) }
## Q1 <- (A + 1) - apply(cdfAttArmRem >= 0.25, 1, locTRUE)
## Q2 <- (A + 1) - apply(cdfAttArmRem >= 0.50, 1, locTRUE)
## Q3 <- (A + 1) - apply(cdfAttArmRem >= 0.75, 1, locTRUE)

## attackers <- as.integer(indexToState(1:40)[ 1:40])
## defenders <- as.character(indexToState(1:40)[41:80])
## tileData <- as.data.frame(cbind(attackers, Q1, Q2, Q3))
## tileData <- cbind(tileData, defenders)
## gatherTileData <- gather(tileData, 'Q1', 'Q2', 'Q3', key=quartile, value=remain )

## ggplot(data=gatherTileData, mapping=aes(x=attackers, y=remain, color=quartile))+
##     geom_line() +
##     geom_point() +
##     facet_wrap( ~ defenders, nrow=2) +
##       scale_x_continuous(breaks=seq(0, 10, by=1)) +
##       scale_y_continuous(breaks=seq(0, 10, by=1))


##                                         # Expected wating time:
## expectedWaitTime <- solve(  IsC - Q, matrix(1, A * D, 1))
