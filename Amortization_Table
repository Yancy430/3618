n = 10
L = 200000
i = 0.03
monthly_rate = i / 12
periods = n * 12

v = 1 / (1 + monthly_rate)
accumulation_factor = (1 - v ^ periods) / monthly_rate
payment_size = L / accumulation_factor

amortization_table <- t(matrix(c(0, 0, L), dimnames = list(c("Interest Paid", "Principal Repaid", "Outstanding Balance"),NULL)))

for (i in (1:periods+1)) {
    I = amortization_table[i - 1, 3] * monthly_rate
    PR = payment_size - I
    OB = amortization_table[i - 1, 3] - PR
    amortization_table <- rbind(amortization_table, c(I, PR, OB))
}
rownames(amortization_table) <- paste0("n = ",(0:periods))
amortization_table
