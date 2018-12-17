# AUFGABENBLATT 2
# Cordula Eggerth (00750881)

rm(list=ls())

######################################################################
# BEISPIEL 1: t-Test zum Vergleich von Stichproben
######################################################################

# ANGABE:
# Man erzeuge 1000 Stichprobenpaare (X1,X2) vom Umfang 30, wobei X1 
# NV ZV mit mu=0 und var=1 ist, und X2 NV ZV mit mu=0 und var=4 ist.
# Nun teste man für jedes der 1000 Paare die Hypothese, dass die 
# Mittelwerte der beiden Variablen X1 und X2 gleich sind mit dem 
# t-Test unter der Annahme von gleichen und ungleichen Varianzen 
# (Welch-Approximation) auf Signifikanzniveau (alpha) 5%. 
# Berechne aus den Simulationen die empirische Wahrsch. des Fehlers
# 1. Art (= Anteil der Simulationen, die zur Ablehnung der H0 führen).
# Welche Ergebnis entspricht besser dem geforderten Signifikanzniveau?

# LOESUNG: 

anteile_H0ablehnen <- function(){
  
    # ergebnisvektoren und alpha anlegen:
    ergebnis_Ttest_gleicheVarianzen <- rep(0,1000)
    ergebnis_Ttest_ungleicheVarianzen <- rep(0,1000)
    alpha <- 0.05
    
    # für 1000 stichprobenpaare den welch-two-sample-t-test machen:
    for(j in 1:1000){
      # stichprobenpaar der j-ten iteration generieren
      stichprobe_x1 <- rnorm(30, sd=1)
      stichprobe_x2 <- rnorm(30, sd=2) # geg. war varianz=4, also sd=2
      
      # welch-two-sample-t-test durchführen auf stichprobenpaar
      ttest_gleicheVarianzen <- t.test(stichprobe_x1, stichprobe_x2, var.equal=TRUE)
      ttest_ungleicheVarianzen <- t.test(stichprobe_x1, stichprobe_x2, var.equal=FALSE)
      
      # p-values den ergebnisvektoren zuweisen
      ergebnis_Ttest_gleicheVarianzen[j] <- ttest_gleicheVarianzen$p.value
      ergebnis_Ttest_ungleicheVarianzen[j] <- ttest_ungleicheVarianzen$p.value
    }
    
    # empirische wahrscheinlichkeit des fehlers 1. art bzw. anteil der 
    # simulationen, die zur ablehnung von H0 führen:
    empWahrsch_gleicheVarianzen <- sum(ergebnis_Ttest_gleicheVarianzen < alpha)/
                                   length(ergebnis_Ttest_gleicheVarianzen)
    
    empWahrsch_ungleicheVarianzen <- sum(ergebnis_Ttest_ungleicheVarianzen < alpha)/
                                     length(ergebnis_Ttest_ungleicheVarianzen)

    # return
    list=list(ttest_gleicheVar=empWahrsch_gleicheVarianzen, 
              ttest_ungleicheVar=empWahrsch_ungleicheVarianzen)

}

# berechne anteile an fehlentscheidungen für n durchläufe:
n <- 30
anteile_ttest1 <- rep(0,n)
anteile_ttest2 <- rep(0,n)

for(i in 1:n){
  anteile <- anteile_H0ablehnen()
  anteile_ttest1[i] <- anteile$ttest_gleicheVar
  anteile_ttest2[i] <- anteile$ttest_ungleicheVar
}

# plot anteile an fehlentscheidungen pro test
plot(1:30, anteile_ttest1*100, ylim=c(3,7), xlab="Iterationen",
     ylab="Anteil der Fehlentscheidungen in %", type="b",
     col="darkcyan", pch=16, main="Anteile der Fehlentscheidungen",
     lty="dotted")
axis(side=2, at=c(0.5:8), cex.axis=0.8)
lines(anteile_ttest2*100, col="darkorange", type="b", pch=2,
      lty="dotted")
legend("bottomleft",legend=c("T-Test (gleiche Varianzen)", "T-Test (ungleiche Varianzen)"),
       col = c("darkcyan", "darkorange"),
       border = "black", lty="dotted", lwd=1, pch=c(16,2))


# besser entspricht dem signifikanzniveau jene empirische wahrscheinlichkeit,
# die näher am gesuchten signifikanzniveau (hier: alpha=5%) liegt

# --------------------------------------------------------------------


######################################################################
# BEISPIEL 2: t-Test vs. Permutationstest
######################################################################

# ANGABE:
# Man erzeuge 1000 Stichprobenpaare (Y1,Y2) vom Umfang 20 aus den 
# natürlichen Zahlen (1 bis 10) (z.B. mit Funktion sample durch 
# Ziehen mit Zurücklegen).
# Diese Beobachtungspaare hätten den gleichen Mittelwert bzw. Median, 
# weisen aber viele Bindungen auf. 
# Man teste nun die Hypothese, dass diese beiden Stichproben gleichen
# Mittelwert haben mit t-Test und mit Permutationstest und vergleiche
# die Anteile der Fehlentscheidungen.
require(perm)

# LOESUNG:

anteile_fehlentscheidungen <- function(){
  
  # ergebnisvektoren und alpha anlegen:
  ergebnis_Ttest <- rep(0,1000)
  ergebnis_PERMtest <- rep(0,1000)
  alpha <- 0.05
  
  
  # T-TEST & PERM-TEST:
  # (siehe Quelle https://cran.r-project.org/web/packages/perm/perm.pdf 
  # für den Permutationstest)
  for(j in 1:1000){
    # stichprobenpaar der j-ten iteration generieren
    stichprobe_y1 <- sample(1:10, 20, replace = TRUE)
    stichprobe_y2 <- sample(1:10, 20, replace = TRUE)
    
    # t-test durchführen auf stichprobenpaar
    ttest <- t.test(stichprobe_y1, stichprobe_y2, var.equal=TRUE)
    # permutationstest durchführen auf stichprobenpaar
    permtest <- permTS(stichprobe_y1, stichprobe_y2, alternative="two.sided")
  
    # p-values den ergebnisvektoren zuweisen
    ergebnis_Ttest[j] <- ttest$p.value
    ergebnis_PERMtest[j] <- permtest$p.value
  }
  
  # empirische wahrscheinlichkeit des fehlers 1. art bzw. anteil der 
  # simulationen, die zur ablehnung von H0 führen:
  empWahrsch_Ttest <- sum(ergebnis_Ttest < alpha)/ length(ergebnis_Ttest)
  empWahrsch_PERMtest <- sum(ergebnis_PERMtest < alpha)/ length(ergebnis_PERMtest)
 
  # return
  list=list(ttest_pval=empWahrsch_Ttest, ptest_pval=empWahrsch_PERMtest)
}


# berechne anteile an fehlentscheidungen für n_iterations durchläufe:
n_iterations <- 30
anteile_ttest <- rep(0,n_iterations)
anteile_ptest <- rep(0,n_iterations)

for(i in 1:n_iterations){
  anteile <- anteile_fehlentscheidungen()
  anteile_ttest[i] <- anteile$'ttest_pval'
  anteile_ptest[i] <- anteile$ptest_pval
}

# plot anteile an fehlentscheidungen pro test
plot(1:30, anteile_ttest*100, ylim=c(3,7), xlab="Iterationen",
     ylab="Anteil der Fehlentscheidungen in %", type="b",
     col="darkcyan", pch=16, main="Anteile der Fehlentscheidungen",
     lty="dotted")
axis(side=2, at=c(0.5:8), cex.axis=0.8)
lines(anteile_ptest*100, col="darkorange", type="b", pch=2,
      lty="dotted")
legend("bottomleft",legend=c("T-Test", "Perm.-Test"),
       col = c("darkcyan", "darkorange"),
       border = "black", lty="dotted", lwd=1, pch=c(16,2))

# --------------------------------------------------------------------


######################################################################
# BEISPIEL 3: Maximum-Likelihood-Schätzer Gammaverteilung
######################################################################

# ANGABE: 
# Die Dichte einer Gamma-Verteilung Gam(v,sigma) ist durch:
#   f(x;v,sigma) = x^(v-1) / (sigma^v * gamma(v)) * e^(-(x/sigma))
#   für x>=0 
# gegeben. 
# Bei bekanntem Parameter v ("form") und n iid Beobachtungen ist der Maximum-
# Likelihood-Schätzer für Parameter sigma ("scale"): 
#   sigma_hat = x_quer/v = 1/n*sum(x_i) / v 
# gegeben.
# Generiere 1000 Zufallsstichproben vom Umfang 20 nach einer Gam(3,4)
# und bestimme empirisch die Verteilung des ML-Schätzers für den 
# Parameter sigma. 
# Vergleiche graphisch die empirische Dichte des ML-Schätzers mit der
# Verteilung der Daten.

# LOESUNG:

# ergebnisvektor anlegen:
ergebnis_sigmahat_ML <- rep(0,1000)
rate <- 0.25
shape_v <- 3
scale_sigma <- 4
stichprobenumfang <- 20
stichprobenmatrix <- matrix(0, nrow=1000, ncol=20)

# generiere 1000 gammaverteilte zufallsstichproben und berechne ML-schaetzer
# fuer sigma:
for(j in 1:1000){
  # zufallsstichprobe der j-ten iteration generieren
  stichprobenmatrix[j, ] <- rgamma(n=stichprobenumfang, shape=shape_v, scale=scale_sigma)
  # berechne sigma_hat aus den daten
  ergebnis_sigmahat_ML[j] <- (1/stichprobenumfang)*sum(stichprobenmatrix[j, ])/shape_v
}

# plot empirische verteilung des ML-schaetzers fuer sigma (d.h. sigmahat_ML):
plot(1:1000, ergebnis_sigmahat_ML, xlab="Iterationen",
     ylab="Wert des ML-Schaetzers fuer Sigma", type="p",
     col="darkcyan", pch=16, main="Empirische Verteilung des ML-Schätzers für Sigma",
     lty="dotted")
abline(h=mean(ergebnis_sigmahat_ML), col="mediumvioletred", pch=2,
      lty="dashed", lwd=3)
text(30, mean(ergebnis_sigmahat_ML)+0.1, "mean sigma", col="mediumvioletred")

# empirische dichte des ML-schaetzers:
m <- 80
seq <- 1:m
f_empirisch <- rep(0,m)
for(i in 1:m){
  f_empirisch[i] <- seq[i]^(shape_v-1) / (mean(ergebnis_sigmahat_ML)^shape_v * 
                    gamma(shape_v)) * exp(-(seq[i]/mean(ergebnis_sigmahat_ML)))
}

# verteilung der daten:
f_verteilung_der_daten <- dgamma(1:m, shape=shape_v, scale=4)

# graphischer vergleich der ergebnisse:
plot(f_verteilung_der_daten, ylab="Dichte", col="rosybrown4", pch=19, xlab="")
points(f_empirisch, col="khaki3")
legend("topright",legend=c("Dichte der gegebenen Verteilung", 
       "Empirische Dichte"), col = c("rosybrown4", "khaki3"),
       border = "black", lwd=1, pch=c(19,1))


# --------------------------------------------------------------------


######################################################################
# BEISPIEL 4: Bayes-Schätzer für Mittelwert einer Normalverteilung
######################################################################

# ANGABE: 
# Sind (x1, x2, ..., xn) unabhängig N(0,1)-verteilte Beobachtungen und
# ist die Prior-Verteilung des Parameters mu durch eine N(x0,1/n0) 
# definiert (konjugierte Prior), dann ist der Bayes-Schätzer für den
# Mittelwert der Poster-Mittelwert
# mu_dach_B = (x0*n0 + sum_1bisn(x_i)) / (n+n0) .
# Die Verteilung des Bayes-Schätzers ist eine NV N(mu_dach_B,1/(n+n0)).
# Man veranschauliche das Ergebnis für den Bayes-Schätzer für die 
# Kombinationen der folgenden Annahmen für die Prior-Verteilungen und
# den Stichprobenumfang:
#   x0 = -1, -0.5, 0, 0.5, 1
#   n0 = 1, 5, 10
#   n = 20.
# --------------------------------------------------------------------


# annahmen für priorverteilungen und stichprobenumfang:
x0 <- c(-1, -0.5, 0, 0.5, 1)
n0 <- c(1, 5, 10)
n <- 20

# berechne poster-mittelwert (mu_dach_B) für versch. kombinationen
# von annahmen:
anzahl_kombis <- length(x0)*length(n0) 
mu_dach_B_vektor <- rep(0,anzahl_kombis)
iter <- 1
beobachtungen <- rnorm(n=20, mean=0, sd=1)

for(i in 1:length(x0)){
  for(j in 1:length(n0)){
    mu_dach_B_vektor[iter] <- (x0[i]*n0[j] + sum(beobachtungen)) / (n+n0[j])
    names(mu_dach_B_vektor)[iter] <- paste0("x0=",x0[i],", n0=",n0[j])
    iter <- iter+1
  }
}

# ergebnis der kombis versch. annahmen für bayes-schätzer:
mu_dach_B_vektor

plot(mu_dach_B_vektor, xlab="Kombinationen",
     ylab="Wert des Bayes-Schätzers (mu_dach_B)", type="p",
     col=c("cyan2","cyan2", "cyan2", "brown3", "brown3", "brown3",
           "goldenrod2", "goldenrod2", "goldenrod2", 
           "deeppink1", "deeppink1", "deeppink1", 
           "mediumseagreen", "mediumseagreen", "mediumseagreen"),
     pch=16, main="Bayes-Schätzer",
     lty="dotted")
legend("topleft",legend=c("x0=-1", "x0=-0.5", "x0=0", "x0=0.5",
                           "x0=1", "n0 jeweils 1 / 5 / 10"), 
       col = c("cyan2", "brown3", "goldenrod2", "deeppink1", 
               "mediumseagreen", "white"),
       border = "black", lwd=1, pch=16)

# dichte der NV des bayes-schätzers (i.e. N(mu_dach_B,1/(n+n0))):
 # ausgangsdaten:
   mu_dach_B_vektor
   n0_vek <- rep(n0,5)

 # plot NV jeweils für mu_dach_B:
  for(i in 1:length(x0)){
      nv_bayes1 <- dnorm(x=seq(-1,1,0.007), mean = mu_dach_B_vektor[i], sd = 1/(n+n0[1]))
      nv_bayes2 <- dnorm(x=seq(-1,1,0.007), mean = mu_dach_B_vektor[i+1], sd = 1/(n+n0[2]))
      nv_bayes3 <- dnorm(x=seq(-1,1,0.007), mean = mu_dach_B_vektor[i+2], sd = 1/(n+n0[3]))
    
      plot(nv_bayes1, ylab="Dichte der NV des Bayes-Schätzers", col="rosybrown4", 
           pch=19, xlab="", ylim=c(0,10), main=paste0("Dichte der NV des Bayes-Sch. für x0=",
           x0[i]))
      points(nv_bayes2, col="mediumseagreen", pch=3)
      points(nv_bayes3, col="goldenrod2", pch=3)
      
      legend("topleft",legend=c(paste0("n0=",n0[1]), paste0("n0=",n0[2]), 
                                paste0("n0=",n0[3])),
             col = c("rosybrown4", "mediumseagreen", "goldenrod2"),
             border = "black", lwd=1, pch=16)
      i <- i*2
    }

   
   