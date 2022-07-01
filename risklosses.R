A <- 20
D <- 20

totalStates <- A * D + (A + D)

stateToIndex <- function(a,d) {
    # given a state, return the index for the trans prob matrix
    if (a > 0) {
        ind <- (A+1 - a) + (D-d)*A  
    }  else {
        ind <- A*D + A + d 
    }
    ind
}

indexToAttackers <- function(ind) {
    # for transient state index, return number of Attacking armies
    j <- A*D - ind
    r <- j %% A
    attArmies <- 1 + r
    attArmies
}

indexToDefenders <- function(ind) {
    # given an index for a transient state, return number of Defending armies
    j <- A*D - ind
    r <- j %% A
    defArmies <- 1 + (j-r)/A
    defArmies
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

# Cases 3
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

## Now I calculate the expected number of Attacking armies remaining
## from each state.  This is the inner or dot product of the submatrix
## absorpProb[ , 1:20] with the vector 20:1.  Note that the first column
## of absorpProb[ , 1:20] is 20 Attacker armies remain, the second column  of
## absorpProb[ , 1:20] is 19 Attacking armies remain and so on.
## This explains the use of the descending vector 20:1.
## Furthermore, for example consider row 90, which corresponds to
## state Att = 11 and Def = 16.  Then examination of the product shows
## the first 9 entries of absorpProb[90, 1:20] are 0, positive probabilities
## only start at entry 16, as described in the text, so the only contribution
## to the expected number of attacking armies remaining will be from the
## absorption probabilities of 11, 10, 9, ..., 1 armies remaining.

attArmiesRemain  <- absorpProb[ , 1:20] %*% 20:1

attackersRemain = data.frame(Att=indexToAttackers(1:400),
                             Def=indexToDefenders(1:400),
                             attRem=attArmiesRemain)

defArmiesRemain  <- absorpProb[ , 21:40] %*% 1:20

defendersRemain = data.frame(Att=indexToAttackers(1:400),
                             Def=indexToDefenders(1:400),
                             defRem=defArmiesRemain)

## library("ggplot2")
## ggplot(attackersRemain, aes(Att,Def)) +
##     geom_raster(aes(fill=attRem)) +
##     scale_x_reverse() +
##     xlab("Attackers") +
##     ylab("Defenders") +
##     scale_fill_viridis_c()

## ggplot(defendersRemain, aes(Att,Def)) +
##     geom_raster(aes(fill=defRem)) +
##     scale_x_reverse() +
##     xlab("Attackers") +
##     ylab("Defenders") +
##     scale_fill_viridis_c()

