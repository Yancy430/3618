# setting up initial values
n <- 10
C <- 10000
r <- 0.06
i <- 0.04

# calculating variables needed to find the price of the bond
coupon_size <- C * r
semiannual_rate <- i / 2
periods <- n * 2
v <- 1 / (1 + semiannual_rate)
accumulation_factor <- (1 - v ^ periods) / semiannual_rate
P <- coupon_size * accumulation_factor + C * v ^ periods

# setting up a bond amortization table with book value beginning at the valued price
bond_amortization_table <- t(matrix(c(0, 0, P), dimnames = list(c("Interest Paid", "Principal Repaid", "Book value"), NULL)))

# calculating I, PR, and OB for each row in the bond amortization table
for (i in (1:periods + 1)) {
    I <- bond_amortization_table[i - 1, 3] * semiannual_rate
    PR <- coupon_size - I
    OB <- bond_amortization_table[i - 1, 3] - PR
    bond_amortization_table <- rbind(bond_amortization_table, c(I, PR, OB))
}

# naming the columns and displaying the table
rownames(bond_amortization_table) <- paste0("t = ", (0:periods))
bond_amortization_table