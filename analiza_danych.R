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
gsub('-','/',substr(visa$Date[1000],3,10))
srednia_kroczaca_visa <- rolling_average(visa$Open)
srednia_kroczaca_mastercard <- rolling_average(mastercard$Open)


pdf(file = "wykres.pdf")
plot(srednia_kroczaca_visa, type = "l", col = "blue", xlab = "Data", ylab = "Cena", ylim = c(140, 400), lty = 1, xaxt='n')
lines(srednia_kroczaca_mastercard, type = "l", col = "orange", lty = 1)
axis(1, at=c(0,200,400,600,800,1000), 
    labels = c(gsub('-','/',substr(visa$Date[1],3,10)),gsub('-','/',substr(visa$Date[200],3,10)),gsub('-','/',substr(visa$Date[400],3,10)),
    gsub('-','/',substr(visa$Date[600],3,10)),gsub('-','/',substr(visa$Date[800],3,10)),gsub('-','/',substr(visa$Date[1000],3,10))), las=0)

legend(100, 400,
    legend = c("Visa", "Mastercard"),
    col = c("blue", "orange"), lty = 1, cex = 0.8
)
dev.off()

c(substr(visa$Date[1],3,10), substr(visa$Date[200],3,10), substr(visa$Date[400],3,10), substr(visa$Date[600]3,10), 
    substr(visa$Date[800],3,10), substr(visa$Date[1000],3,10))
df <- data.frame(GIS$Date[4:(length(v) - 3)], oryginalne_wyceny_bez_końcowych, srednia_kroczaca)
colnames(df) <- c("Date", "Open", "Running average")

write.csv(df, file = "dataframe.csv", row.names = FALSE)
### zadanie 3

our_matrix <- matrix(rnorm(700), ncol = 7)

usun_dwie_najmniejsze <- function(v) {
    v <- v[-which.min(v)] # odejmij ten element
    v <- v[-which.min(v)]
    return(v)
}

empty_matrix <- matrix(, nrow = 100, ncol = 5) # tymczasowo pusta macierz

for (i in 1:nrow(our_matrix)) {
    empty_matrix[i, ] <- usun_dwie_najmniejsze((our_matrix[i, ]))
}
# teraz każdy wiersz w empty_matrix nie ma 2 elementów najmniejszych
sd(empty_matrix[2, ])
odchylenia_wierszy <- apply(empty_matrix, 1, function(x) sd(x))
srednia_odchylen <- mean(odchylenia_wierszy)
?apply
# usuwanie najmniejszych dla sprawdzenia
usuniete_skrajne <- t(apply(our_matrix, 1, function(x) x[-which.min(x)][-which.min(x[-which.min(x)])]))
