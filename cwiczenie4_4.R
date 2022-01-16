co_ja_robie <- function(n) {
  
require(ggplot2)
require(gridExtra)

wektor <- c()
ma5 <- c()
wynik <- c()

  for(i in 1:n) {
    wektor <- c(wektor, rnorm(1))
    
    if (i < 5) 
      ma5 <- c(ma5, 0) else { 
        ma5_i <- 0
        for (j in (i-4):i) 
          ma5_i <- ma5_i + wektor[j]
        ma5_i <- ma5_i / (i - (i-4) + 1)
        ma5 <- c(ma5, ma5_i)
      }
    if (i >= 5) {
      if (ma5[i] > wektor[i]) wynik[i] <- 1
      if (ma5[i] < wektor[i]) wynik[i] <- -1
      if (ma5[i] == wektor[i]) wynik[i] <- wynik[i-1]
      }
  }
  dane <- data.frame(nr = 1:n, wektor, ma5, wynik)
  
  wykres1 <- ggplot(data = dane,
                    aes(x = nr, y = wektor)) + 
    geom_line(color = "dark gray") +
    geom_line(aes(x = nr, y = ma5),
              size = 1.5, 
              colour = "red") + 
    theme_bw()
  wykres2 <- ggplot(data = dane, 
                    aes(x = nr, ymax = wynik, ymin = 0)) + 
    geom_linerange() +
    theme_bw()

  grid.arrange(wykres1, wykres2, nrow = 2)  
}