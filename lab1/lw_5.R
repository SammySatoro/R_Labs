# Напишите код, который заменяет случайно элементы некоторого вектора на пропущенные
# значения (NA). Ваш код должен работать для вектора любой длины. Далее необходимо
# вывести на экран индексы пропущенных значений в векторе. Напишите код, который
# считает, сколько пропущенных значений в векторе.

vec_size <- 10
vec <- sample(1:100, vec_size, replace = F)
prob = runif(1, 0, 1)
NA_vec <- sample(c(0, NA), vec_size, replace = T, prob=c(prob, 1 - prob))
res_vec <- vec + NA_vec
which(res_vec %in% NA)
missed_values <- sum(res_vec %in% NA)
