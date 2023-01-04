
library("lpSolve")

# linear programming: small problems
#
# Przyklad 1
#
library("lpSolve")
# defining parameters
obj.fun <- c(5, 3, 4)
constr <- matrix(0.01*c(2.6, 2.1, 2.1, 0.4, 0.9, 0.6, 0.6, 0.2, 0.6, 0.6, 0.2, 0.6, 2, 0, -1), ncol = 3, byrow = TRUE)
constr.dir <- c(">=", ">=", ">=", "<=", "=")
rhs <- c(10.2, 2.4, 2.7, 4, 0)
# solving model
(prod.sol <- lp("min", obj.fun, constr, constr.dir, rhs,
    compute.sens = TRUE
))
# accessing to R output
prod.sol$solution # decision variables values
prod.sol$duals # includes duals of constraints, reduced costs of variables
#

library("lpSolve")
# defining parameters
obj.fun <- c(9.6, 14.4, 10.8, 7.2)
constr <- matrix(c(0.8, 2.4, 0.9, 0.4, 0.6, 0.6, 0.3, 0.3), ncol = 4, byrow = TRUE)
constr.dir <- c(">=", ">=")
rhs <- c(1200, 600)
# solving model
(prod.sol <- lp("min", obj.fun, constr, constr.dir, rhs,
    compute.sens = TRUE
))
# accessing to R output
prod.sol$solution # decision variables values
prod.sol$duals # includes duals of constraints, reduced costs of variables
#
# Przyklad 2
#
# defining parameters
obj.fun <- c(4, 7, 10, 0)
constr <- matrix(c(3, 2, 1, 0, 0, 1, 2, 4), ncol = 4, byrow = TRUE)
constr.dir <- c(">=", ">=")
rhs <- c(660, 500)
# solving model
(prod.sol <- lp("min", obj.fun, constr, constr.dir, rhs,
    compute.sens = TRUE
))
# accessing to R output
prod.sol$solution # decision variables values
prod.sol$duals # includes duals of constraints, reduced costs of variables
#
# sensibility analysis results
prod.sol$duals.from
prod.sol$duals.to
prod.sol$sens.coef.from
prod.sol$sens.coef.to
#
# Przyklad 3
#
# defining parameters
obj.fun <- c(-1.5, 2)
constr <- matrix(c(5, 4, -3, 4, 1, 0), ncol = 2, byrow = TRUE)
constr.dir <- c(">=", ">=", "<=")
rhs <- c(20, 4, 3)
# solving model
(prod.sol <- lp("min", obj.fun, constr, constr.dir, rhs,
    compute.sens = TRUE
))
# accessing to R output
prod.sol$solution # decision variables values
prod.sol$duals # includes duals of constraints, reduced costs of variables
#
# sensibility analysis results
prod.sol$duals.from
prod.sol$duals.to
prod.sol$sens.coef.from
prod.sol$sens.coef.to
#
#
# Przyklad 4
#
# defining parameters
obj.fun <- c(-1.5, 2)
constr <- matrix(c(5, 4, -3, 4, 1, 0), ncol = 2, byrow = TRUE)
constr.dir <- c("<=", ">=", ">=")
rhs <- c(20, 4, 3)
# solving model
(prod.sol <- lp("max", obj.fun, constr, constr.dir, rhs,
    compute.sens = TRUE
))
# accessing to R output
prod.sol$solution # decision variables values
prod.sol$duals # includes duals of constraints, reduced costs of variables
#
# sensibility analysis results
prod.sol$duals.from
prod.sol$duals.to
prod.sol$sens.coef.from
prod.sol$sens.coef.to

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
costs <- cbind(costs, c(0, 0, 0))
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
