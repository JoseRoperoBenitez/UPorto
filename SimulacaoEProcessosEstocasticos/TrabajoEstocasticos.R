#Proyecto Estocasticos 

# Código em R para simulação: soma de 50 lançamentos de um dado de 6 faces

# Parâmetros teóricos
mu <- 50 * 3.5            # média = 175
sd <- sqrt(50 * 35/12)    # desvio padrão ≈ 12.08

# 1) Densidade da normal aproximada
curve(dnorm(x, mu, sd),
      from = mu-4*sd, to = mu+4*sd,
      xlab = "Soma de 50 dados",
      ylab = "Densidade",
      main = "Distribuição Normal N(175, 12.08²)")
abline(v = mu, col = "red")  # linha da média

# 2) Simulações
n <- 10000
sim_norm  <- rnorm(n, mu, sd) # simulação pela 
# normal
sim_dados <- replicate(n,
                       sum(sample(1:6, 50, TRUE))
)                          # simulação real de 
# dados

# Estatísticas
cat("Normal: média =", round(mean(sim_norm),2),
    "  sd =", round(sd(sim_norm),2), "\n")
cat("Dados : média =", round(mean(sim_dados),2),
    "  sd =", round(sd(sim_dados),2), "\n")

# 3) Estimativas de P(S = k)
ks   <- 50:300

# 3a) Estimativa por Monte Carlo (frequência empírica)
freq <- table(sim_dados) / n
P_mc <- sapply(ks, function(k) {
  key <- as.character(k)
  if (key %in% names(freq)) freq[key] else 0
})

# 3b) Estimativa pela aproximação normal com 
# correção de continuidade
P_norm <- sapply(ks, function(k) {
  pnorm(k+0.5, mu, sd) - pnorm(k-0.5, mu, sd)
})

# 4) Gráfico comparativo
plot(ks, P_mc, type = "h", col = "steelblue",
     xlab = "k", ylab = "P(Soma = k)",
     main = "MC empírica vs Aproximação Normal")
lines(ks, P_norm, col = "darkorange", lwd = 2)
legend("topright",
       legend = c("MC empírica", 
                  "Normal c/ correção"),
       col    = c("steelblue", "darkorange"),
       lwd    = 2)


# 5) Dado viciado
num_simulacoes <- 10000
num_dados <- 50
valores_dado <- 1:6
probabilidades <- c(0.1, 0.1, 0.1, 0.2, 0.2, 0.3)

set.seed(123)
somas_viciado <- replicate(num_simulacoes, sum
                           (sample(valores_dado, num_dados, 
                                   replace = TRUE, prob = 
                                     probabilidades)))

summary(somas_viciado)
media_viciado=mean(somas_viciado)
desvio_viciado=sd(somas_viciado)

hist(somas_viciado,
     breaks = 50,
     col = "skyblue",
     xlim=c(160,260),
     freq=F,
     main = "Distribuição das somas de 50 
             lançamentos (dado viciado)",
     xlab = "Soma",
     ylab = "Frequência")
curve(dnorm(x,mean=media_viciado,
            sd=desvio_viciado), col="red",
      lwd=2,add=TRUE)


# 6) Dado com exclusão
num_simulacoes<- 10000
num_lancamentos<- 50
set.seed(123)
somas <- replicate(num_simulacoes, sum
                   (sample(1:5, num_lancamentos, replace =
                             TRUE)))

hist(somas,
     breaks = 40,
     main = "Histograma da soma de 50 lançamentos 
             (valores 1 a 5)",
     xlab = "Soma",
     freq=F,
     col = "skyblue",
     border = "white")
media<-mean(somas)
desvio<-sd(somas)
curve(dnorm(x,mean=media,sd=desvio),col="red",
      lwd=2,add=TRUE)