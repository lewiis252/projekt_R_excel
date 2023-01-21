# Danych jest 5 pudełek ponumerowanych liczbami od 1 do 5. W każdym pudełku znajduje się 20 kul ponumerowanych liczbami od 1 do 20.
# Z każdego pudełka wybieramy jedną kulę. Oblicz prawdopodobieństwo zdarzenia polegającego na tym, że każda z wylosowanych liczb jest
# mniejsza od wszystkich liczb wylosowanych z pudełek o większych numerach oraz suma wylosowanych liczb jest podzielna przez 3.

# sposób pętla 3
# zwróci TRUE lub FALSE zależnie czy wylosowane kule spełniają warunki zadania
czy_war_spelniony <- function(x) {
    !is.unsorted(x) && !anyDuplicated(x) && (sum(x) %% 3 == 0)
}

# trwa około 10 sekund
liczba_prob <- 10^6
liczba_prob_spelniajacych_warunek <- 0

for (j in 1:liczba_prob) {
    wylosowane_liczby <- sample(1:20, 5, rep = TRUE)
    if (czy_war_spelniony(wylosowane_liczby)) {
        liczba_prob_spelniajacych_warunek <- liczba_prob_spelniajacych_warunek + 1
    }
}
prawdop <- liczba_prob_spelniajacych_warunek / liczba_prob

cat("Prawdopodobieństwo wynosi ", prawdop)

# sposób funkcyjny

# trwa około 10 sekund
N <- 10^6
wylosowane <- replicate(N, (czy_war_spelniony(sample(1:20, 5, rep = TRUE))))
prawdop <- length(wylosowane[wylosowane == TRUE]) / N

cat("Prawdopodobieństwo wynosi ", prawdop)

