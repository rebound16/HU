# Data Cleaning

# Detect Special Values Define function that counts special values for a variable
# newdata[, x], damit Vektor und nicht String angewendet wird
count.special = function(x) {
    l1 = length(which(newdata[, x] == -1))
    l2 = length(which(newdata[, x] == -2))
    l3 = length(which(newdata[, x] == -3))
    l4 = length(which(newdata[, x] == -4))
    l5 = length(which(newdata[, x] == -5))
    l6 = length(which(newdata[, x] == -6))
    nr = c(l1, l2, l3, l4, l5, l6)
    names(nr) = c(-1, -2, -3, -4, -5, -6)
    return(nr)
}

count.special("netinc")  # example for netinc

# Build matrix showing amount of special values for all variables
sapply(colnames(newdata), count.special)

# Omit Special Values Define function changing special values to NA
special.NA = function(x) {
    x1 = replace(x, x == -1 | x == "[-1] keine Angabe" | x == -2 | x == "[-2] trifft nicht zu" | 
        x == -3 | x == "[-3] nicht valide" | x == -4 | x == "[-4] Unzulaessige Mehrfachantwort" | 
        x == -5 | x == "[-5] In Fragebogenversion nicht enthalten" | x == -6 | x == "[-6] Fragebogenversion mit geaenderter Filterfuehrung", 
        NA)
    return(x1)
}

# New data frames with special values of all variables changed to NA
newdata.na  = special.NA(newdata)

newdata.fna = special.NA(newdata.f)

# Check whether basic measures have changed
summary(newdata$netinc)

# Save newdata.na for future use
library(foreign)
write.dta(newdata.na, "newdataNA.dta")

write.dta(newdata.fna, "newdatafNA.dta")
