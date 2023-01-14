# Danych jest 5 pudełek ponumerowanych liczbami od 1 do 5. W każdym pudełku znajduje się 20 kul ponumerowanych liczbami od 1 do 20.
# Z każdego pudełka wybieramy jedną kulę. Oblicz prawdopodobieństwo zdarzenia polegającego na tym, że każda z wylosowanych liczb jest
# mniejsza od wszystkich liczb wylosowanych z pudełek o większych numerach oraz suma wylosowanych liczb jest podzielna przez 3.


# sposób pętla 1

liczba_prob <- 10000000
liczba_prob_spelniajacych_warunek <- 0

# for (j in 1:liczba_prob) {
#     wylosowane_liczby <- sample(1:20, 5, rep = TRUE)
#     warunek_spelniony <- 0
#     for (i in 1:5) {
#         if (wylosowane_liczby[i] == min(wylosowane_liczby[i:5]) && length(wylosowane_liczby[wylosowane_liczby == wylosowane_liczby[i]]) == 1) {
#             warunek_spelniony <- warunek_spelniony + 1
#         }
#     }
#     if (warunek_spelniony == 5) {
#         liczba_prob_spelniajacych_warunek <- liczba_prob_spelniajacych_warunek + 1
#     }
# }
# liczba_prob_spelniajacych_warunek / liczba_prob

# # sposób pętla 2

# start_time <- Sys.time()

# liczba_prob <- 10^5
# liczba_prob_spelniajacych_warunek <- 0

# for (j in 1:liczba_prob) {
#     wylosowane_liczby <- sample(1:20, 5, rep = TRUE)
#     warunek_spelniony <- 0
#     for (i in 1:4) {
#         if (all(wylosowane_liczby[i] < wylosowane_liczby[(i + 1):5])) {
#             warunek_spelniony <- warunek_spelniony + 1
#         }
#     }
#     if (warunek_spelniony == 4) {
#         liczba_prob_spelniajacych_warunek <- liczba_prob_spelniajacych_warunek + 1
#     }
# }
# liczba_prob_spelniajacych_warunek / liczba_prob

# end_time <- Sys.time()
# end_time - start_time

# sposób pętla 3
# zwróci TRUE lub FALSE zależnie czy wylosowane kule spełniają warunki zadania
czy_war_spelniony <- function(x) {
    !is.unsorted(x) && !anyDuplicated(x) && (sum(x) %% 3 == 0)
}

start_time <- Sys.time()
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
end_time <- Sys.time()
end_time - start_time


# sposób funkcyjny

start_time <- Sys.time()

# trwa około 10 sekund
N <- 10^6
wylosowane <- replicate(N, (czy_war_spelniony(sample(1:20, 5, rep = TRUE))))
prawdop <- length(wylosowane[wylosowane == TRUE]) / N

cat("Prawdopodobieństwo wynosi ", prawdop)

end_time <- Sys.time()
end_time - start_time
