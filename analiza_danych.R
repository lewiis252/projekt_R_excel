visa <- read.csv("VISA.csv", header = TRUE, dec = ".")
mastercard <- read.csv("MASTERCARD.csv", header = TRUE, dec = ".")

# funkcja do obliczenia średniej kroczącej
rolling_average <- function(v) {
    srednia_kroczaca <- c()
    for (i in 4:(length(v) - 3)) {
        srednia_kroczaca[i - 3] <- mean(v[(i - 3):(i + 3)])
    }
    return(srednia_kroczaca)
}

# do średniej kroczącej ceny otwarcia
srednia_kroczaca_visa <- rolling_average(visa$Open)
srednia_kroczaca_mastercard <- rolling_average(mastercard$Open)

# wykres cen średniej kroczącej obu spółek
pdf(file = "wykres.pdf")
plot(srednia_kroczaca_visa, type = "l", col = "blue", xlab = "Data", ylab = "Cena", ylim = c(140, 400), lty = 1, xaxt = "n")
lines(srednia_kroczaca_mastercard, type = "l", col = "orange", lty = 1)
axis(1,
    at = c(0, 200, 400, 600, 800, 1000),
    labels = c(
        gsub("-", "/", substr(visa$Date[1], 3, 10)), gsub("-", "/", substr(visa$Date[200], 3, 10)), gsub("-", "/", substr(visa$Date[400], 3, 10)),
        gsub("-", "/", substr(visa$Date[600], 3, 10)), gsub("-", "/", substr(visa$Date[800], 3, 10)), gsub("-", "/", substr(visa$Date[1000], 3, 10))
    ), las = 0
)

legend(100, 400,
    legend = c("Visa", "Mastercard"),
    col = c("blue", "orange"), lty = 1, cex = 0.8
)
dev.off()

cor(srednia_kroczaca_visa, srednia_kroczaca_mastercard)

# wachania ceny w ciągu dnia
visa$Rozstep <- visa$High - visa$Low
mastercard$Rozstep <- mastercard$High - mastercard$Low
max(visa$Rozstep)
max(mastercard$Rozstep)
min(visa$Rozstep)
min(mastercard$Rozstep)

mean(visa$Rozstep)
sd(visa$Rozstep)

hist(visa$Rozstep, breaks = 15)
hist(mastercard$Rozstep, breaks = 15)

# względny zysk na dzień 31 grudnia 2022 po zainwestowaniu dolarów 2 stycznia 2019
zysk_visa <- (tail(visa$Open, 1)-visa$Open[1])/visa$Open[1]
zysk_mastercard <- (tail(mastercard$Open, 1) - mastercard$Open[1])/mastercard$Open[1]

