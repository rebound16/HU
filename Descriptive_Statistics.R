# Install packages if not installed and read in data
libraries = c("xtable")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})

library(foreign)
newdata.fna = read.dta("newdatafNA.dta")


# Descriptive Statistics


# Matrix with basic statistics for all variables 
#Check class of variables
sapply(newdata.fna, class)

# Create function that returns the mode
findmode = function(x) {
    names(sort(-table(x)))[1]
}

# Allow Latex code for matrix to be printed
library("xtable")

# Function for summary stat. for all integer class variables, mode for all others, in a matrix

summarystats = function(df, Latex) {
    df.int             = df[sapply(df, is.numeric)]
    median.df.int      = sapply(df.int, median, na.rm = TRUE)
    mean.df.int        = sapply(df.int, mean, na.rm = TRUE)
    sd.df.int          = sapply(df.int, sd, na.rm = TRUE)
    max.df.int         = sapply(df.int, max, na.rm = TRUE)
    min.df.int         = sapply(df.int, min, na.rm = TRUE)
    summ.int           = cbind(median.df.int, mean.df.int, sd.df.int, max.df.int, min.df.int)
    colnames(summ.int) = c("Median", "Mean", "SD", "Max", "Min")
    print(summ.int)
    if (Latex == TRUE) {
        print(xtable(summ.int))
    }
    df.fac             = df[sapply(df, is.factor)]
    mod.df.fac         = sapply(df.fac, findmode)
    summ.fac           = cbind(mod.df.fac)
    colnames(summ.fac) = c("Mode")
    print(summ.fac)
    if (Latex == TRUE) {
        print(xtable(summ.fac))
    }
}

# Create matrix
summarystats(newdata.fna, Latex = FALSE)

# Create matrix and Latex code
summarystats(newdata.fna, Latex = TRUE)

# Define function creating a different plot for individual numeric or factor variables
# Define function (from help, not our work) to capitalize string in plot
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
}



# Define function to create histogram for data up to a certain percentile
hist.perc = function(x, y = 1) {
    vec = sort(na.omit(newdata.fna[, x]))[c(1:(length(na.omit(newdata.fna[, x])) * y))]
    hist(vec, main = paste("Histogram", simpleCap(x), sep = " "), xlab = NULL, ylab = "Frequency", 
        col = "blue", border = "green", breaks = 100)
}

# Define function to create nice barplot
barp.nice = function(x) {
    barplot(table(newdata.fna[, x], exclude = c("[-1] keine Angabe", "[-2] trifft nicht zu", 
        "[-3] nicht valide", "[-4] Unzulaessige Mehrfachantwort", "[-5] In Fragebogenversion nicht enthalten", 
        "[-6] Fragebogenversion mit geaenderter Filterfuehrung")), main = paste("Bar Plot", 
        simpleCap(x), sep = " "), xlab = NULL, ylab = "Frequency", col = "blue", border = "darkgreen", 
        axes = FALSE, names.arg = FALSE)
    attr = tail(head(unlist(attributes(newdata.fna[, x]), use.names = FALSE), -1), -6)
    axis(side = 2, at = (c((1:10) * 1000)))
    axis(side = 1, pos = 0, at = seq(from = 0.7, to = length(attr) + length(attr)/4, by = 1.2), 
        tck = -0.01, labels = attr, las = 2, par(mar = c(18, 4, 4, 2)))
}


# Define function
plot.distr = function(x, y = 1) {
    if (is.numeric(newdata.fna[, x]) == TRUE) {
        hist.perc(x, y)
    } else {
        barp.nice(x)
    }
}

# Apply function a few times and save plots to pdf
pdf("Activity.pdf")
par(mar = c(18, 4, 4, 2))
plot.distr("activity")
dev.off()

pdf("Lsatisf.pdf")
par(mar = c(18, 4, 4, 2))
plot.distr("Lsatisf")
dev.off()

pdf("Donation.pdf")
par(mar = c(6, 4, 4, 2))
plot.distr("donation", 0.95)
dev.off()

pdf("Netinc.pdf")
par(mar = c(6, 4, 4, 2))
plot.distr("netinc", 0.99)
dev.off()
