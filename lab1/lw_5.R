vec_size <- 10
vec <- sample(1:100, vec_size, replace = F)
random_replacemets_count <- sample(1:vec_size, 1)
modified_vec <- replace(vec, sample(1:vec_size, random_replacemets_count), NA)
which(modified_vec %in% NA)
