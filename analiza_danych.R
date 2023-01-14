# setwd("/home/lewiis/kody komputerowe/projekt")

visa <- read.csv("VISA.csv", header = TRUE, dec = ".")
mastercard <- read.csv("MASTERCARD.csv", header = TRUE, dec = ".")

# funkcja do obliczenia średniej kroczącej
rolling_average <- function(x, n = 7){
    srednia_kroczaca <- filter(x, rep(1 / n, n))
    srednia_kroczaca <- srednia_kroczaca[-(1:3)]
    srednia_kroczaca <- srednia_kroczaca[- ((length(srednia_kroczaca)-2): length(srednia_kroczaca))]
    return(srednia_kroczaca)
    }



visa$cena_srednia <- (visa$High + visa$Low) / 2
mastercard$cena_srednia <- (mastercard$High + mastercard$Low) / 2
# do średniej kroczącej ceny otwarcia
srednia_kroczaca_visa <- rolling_average(visa$cena_srednia)
srednia_kroczaca_mastercard <- rolling_average(mastercard$cena_srednia)

srednia_kroczaca_visa1 <- rolling_average1(visa$cena_srednia)
# wykres cen średniej kroczącej obu spółek
pdf(file = "wykres.pdf")
plot(srednia_kroczaca_visa, type = "l", col = "blue", xlab = "Data", ylab = "Cena", ylim = c(140, 400), lty = 1, xaxt = "n", ,main='Ceny akcji w latach 2019-2022')
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
visa$Rozstep <- visa$Close - visa$Open
mastercard$Rozstep <- mastercard$Close - mastercard$Open
max(visa$Rozstep)
max(mastercard$Rozstep)
min(visa$Rozstep)
min(mastercard$Rozstep)

mean(visa$Rozstep)
sd(visa$Rozstep)

mean(mastercard$Rozstep)
sd(mastercard$Rozstep)


pdf(file = "histogram.pdf")
par(mfrow = c(1, 2))
hist(visa$Rozstep, breaks = 20, main='Występowanie kwoty salda', xlab='Róznica cen na koniec dnia', ylab='Częstość')
hist(mastercard$Rozstep, breaks = 20, main='Występowanie kwoty salda', xlab='Róznica cen na koniec dnia', ylab='Częstość')
dev.off()
?boxplot

# par(mfrow = c(1, 2))
# boxplot(visa$Rozstep)
# boxplot(mastercard$Rozstep)

# względny przyrost ceny na dzień 31 grudnia 2022 po zainwestowaniu dolarów 2 stycznia 2019
zysk_visa <- (tail(visa$cena_srednia, 1) - visa$cena_srednia[1]) / visa$cena_srednia[1]
zysk_mastercard <- (tail(mastercard$cena_srednia, 1) - mastercard$cena_srednia[1]) / mastercard$cena_srednia[1]

stopa_zwrotu_zakup_akcji_co_20_dni <- function(dataframe_name) {
    cena_srednia <- (dataframe_name$High + dataframe_name$Low) / 2

    dzien_zakupu <- rep_len(c(1, rep(0, 19)), length.out = nrow(dataframe_name)) # make column that storage day of purchase

    liczba_akcji <- 100 / cena_srednia * dzien_zakupu

    skumulowana_liczba_akcji <- cumsum(liczba_akcji)

    wartosc_portfela <- skumulowana_liczba_akcji * cena_srednia

    zainwestowane_srodki <- cumsum(dzien_zakupu * 100)

    n <- nrow(dataframe_name)

    stopa_zwrotu <- (wartosc_portfela[n] - zainwestowane_srodki[n]) / zainwestowane_srodki[n]
    stopa_zwrotu
}

stopa_zwrotu_zakup_akcji_co_20_dni(visa)
stopa_zwrotu_zakup_akcji_co_20_dni(mastercard)

