
library("lpSolve")

# zadanie 68
# Set up cost matrix
costs <- matrix(c(6, 1, 3, 3, 4, 3, 5, 2, 3, 2, 4, 5),
    nrow = 3, byrow = TRUE
)

# Set up constraint signs and right-hand sides
row.signs <- rep("==", 3)
row.rhs <- c(1200, 800, 1200)
col.signs <- rep("==", 4)
col.rhs <- c(700, 700, 1000, 800)

# Solve
(lptrans <- lp.transport(costs, "min", row.signs, row.rhs,
    col.signs, col.rhs,
    compute.sens = 1
))
lptrans$solution
lptrans$objval

# zadanie 75
# a)
# Set up cost matrix
production_cost <- c(220, 200, 210)
transport_cost <- matrix(
    c(
        5, 12, 10, 8, 10, 2, 5, 7, 15, 10, 14,
        3, 8, 13, 7, 16, 9, 3
    ),
    nrow = 3, byrow = TRUE
)

costs <- production_cost + transport_cost
# Set up constraint signs and right-hand sides
row.signs <- rep("==", 3)
row.rhs <- c(5000, 6000, 4000)
col.signs <- rep("==", 6)
col.rhs <- c(4000, 3000, 2000, 1000, 3000, 2000)

# Solve
(lptrans <- lp.transport(costs, "min", row.signs, row.rhs,
    col.signs, col.rhs,
    compute.sens = 1
))
lptrans$solution
lptrans$objval

# b)
production_cost <- c(220, 200, 210)
transport_cost <- matrix(
    c(
        5, 12, 10, 8, 10,
        5, 7, 15, 10, 14, 
        8, 13, 7, 16, 9
    ),
    nrow = 3, byrow = TRUE
)

costs <- production_cost + transport_cost
# add 0 column
costs <- cbind(costs, c(0,0,0))
# Set up constraint signs and right-hand sides
row.signs <- rep("==", 3)
row.rhs <- c(5000, 6000, 4000)
col.signs <- rep("==", 6)
col.rhs <- c(4000, 3000, 2000, 1000, 3000, 2000)

# Solve
(lptrans <- lp.transport(costs, "min", row.signs, row.rhs,
    col.signs, col.rhs,
    compute.sens = 1
))
lptrans$solution
lptrans$objval
