datos <- read.table(file = "C:\\Users\\julie\\Unal\\2026-2\\Probabilidad y Estadistica Fundamental\\EP.txt", header =TRUE)
baja <- datos$emision[datos$altitud == 0]
alta <- datos$emision[datos$altitud == 1]

n_baja <- length(baja)
print(n_baja)
n_alta <- length(alta)
print(n_alta)

n_min_baja <- min(baja)
n_max_baja <- max(baja)
print(n_min_baja)
print(n_max_baja)
n_min_alta <- min(alta)
n_max_alta <- max(alta)
print(n_min_alta)
print(n_max_alta)


q1_baja <- quantile(baja, 0.25)
print(q1_baja)
q2_baja <- quantile(baja, 0.50)
print(q2_baja)
q3_baja <- quantile(baja, 0.75)
print(q3_baja)
q1_alta <- quantile(alta, 0.25)
print(q1_alta)
q2_alta <- quantile(alta, 0.50)
print(q2_alta)
q3_alta <- quantile(alta, 0.75)
print(q3_alta)


promedio_n_baja <- sum(baja)/n_baja
print(promedio_n_baja)
promedio_n_alta <- sum(alta)/n_alta
print(promedio_n_alta)

mediana_baja <- median(baja, na.rm = TRUE)
mediana_alta <- median(alta, na.rm = TRUE)

s_baja <- sd(baja)
print(s_baja)
s_alta <- sd(alta)
print(s_alta)

cv_baja <- s_baja/promedio_n_baja

cv_alta <- s_alta/promedio_n_alta

print(cv_baja)

print(cv_alta)

# Punto 4

m_clase <- c(2.7345, 7.0925, 10.5985, 14.4005, 19.0015, 25.0725, 34.434, 52.939, 98.304)

print(m_clase)

n_abs <- c(305,294,331,286,306,273,334,326,290)

total = sum(n_abs)
n_rel = n_abs/total
print(n_rel)
media_agrupada1 =  (1/sum(n_abs)) * sum(n_abs * m_clase)
media_agrupada2 = sum(n_rel * m_clase)
print(media_agrupada2)
print(media_agrupada1)

P_50 <- 21.619 + 6.907 * ((0.5 * 3068 - 1522) / 273)

print(P_50)

l_k1 <- 28.526
a_k  <- 11.816
n_k  <- 334
n_k1 <- 273
n_k2 <- 326  # n_{k+1}

# Cálculo de la moda
moda <- l_k1 + a_k * ((n_k - n_k1) / (2 * n_k - n_k1 - n_k2))

# Mostrar resultado
print(moda)