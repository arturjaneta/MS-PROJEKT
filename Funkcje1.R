#ZADANIE 1

kwartyl <- function(dane, q)
{
  pozycjaKwartyla = sum(dane$counts) * q
  nIsk <- dane$counts   #liczebnosc skumulowana
  
  for(i in 1:length(dane$counts))
  {
    nIsk[i] = sum(dane$counts[1:i])
  }
  
  znaleziona = 0
  pozycja = 1
  while (znaleziona == 0)
  {
    if (pozycjaKwartyla < nIsk[pozycja] )
      znaleziona = 1
    else
      pozycja = pozycja + 1
  }
  
  xI0 = dane$breaks[pozycja]       #dolna wartosc przedzialu z kwantylem
  nIskminus1 = nIsk[pozycja-1]    #liczebnosc skumulowana przedzialu poprzedzajacego kwantyl
  nI0 = dane$counts[pozycja]       #liczebnosc przedzialu z kwantylem
  rozpietoscPrzedzialu = dane$breaks[pozycja+1] - dane$breaks[pozycja]
  wynik = xI0 + ((pozycjaKwartyla - nIskminus1) * (rozpietoscPrzedzialu / nI0))
  return (wynik)
}

#ZADANIE 1

kwartyl <- function(dane, q)
{
  pozycjaKwartyla = sum(dane$counts) * q
  nIsk <- dane$counts   #liczebnosc skumulowana
  
  for(i in 1:length(dane$counts))
  {
    nIsk[i] = sum(dane$counts[1:i])
  }
  
  znaleziona = 0
  pozycja = 1
  while (znaleziona == 0)
  {
    if (pozycjaKwartyla < nIsk[pozycja] )
      znaleziona = 1
    else
      pozycja = pozycja + 1
  }
  
  xI0 = dane$breaks[pozycja]       #dolna wartosc przedzialu z kwantylem
  nIskminus1 = nIsk[pozycja-1]    #liczebnosc skumulowana przedzialu poprzedzajacego kwantyl
  nI0 = dane$counts[pozycja]       #liczebnosc przedzialu z kwantylem
  rozpietoscPrzedzialu = dane$breaks[pozycja+1] - dane$breaks[pozycja]
  wynik = xI0 + ((pozycjaKwartyla - nIskminus1) * (rozpietoscPrzedzialu / nI0))
  return (wynik)
}

zad1a <- function(wektor)
{
  ssd1 = sort(wektor)  #szereg szczegolowy
  
  sredniaArytmetyczna = mean(ssd1)
  sredniaHarmoniczna = 1 / mean(1 / ssd1)
  sredniaGeometryczna = prod(ssd1) ^ (1 / length(ssd1))
  wariancja = var(ssd1)
  odchylenieStandardowe = sqrt(wariancja)
  mediana = median(ssd1)
  odchyleniePrzecietneMed = sum(abs(ssd1 - mediana)) / length(ssd1)
  kwartylQ1 = quantile(ssd1, 0.25)
  kwartylQ3 = quantile(ssd1, 0.75)
  odchylenieCwiartkowe = (quantile(ssd1, 0.75) - quantile(ssd1, 0.25)) / 2  
  wspolczynnikZmiennosci =  odchylenieStandardowe / sredniaArytmetyczna * 100 
  wspolczynnikAsymetrii = (sum((ssd1 - sredniaArytmetyczna) ^ 3) / length(ssd1)) / (odchylenieStandardowe ^ 3)
  wspolczynnikSkosnosci = 3*(sredniaArytmetyczna - mediana) / odchylenieStandardowe
  kurtoza = (sum((ssd1 - sredniaArytmetyczna) ^ 4) / length(ssd1)) / (odchylenieStandardowe ^ 4)
  exces = kurtoza - 3
  wynik <- c(sredniaArytmetyczna, sredniaHarmoniczna, sredniaGeometryczna, wariancja, odchylenieStandardowe, mediana, odchyleniePrzecietneMed, kwartylQ1, kwartylQ3, odchylenieCwiartkowe, wspolczynnikZmiennosci, wspolczynnikAsymetrii, wspolczynnikSkosnosci, kurtoza,exces)
  return (wynik)
}

zad1b <- function(wektor)
{
  sr1 = hist(wektor)   #szereg rozdzielczy
  
  sredniaArytmetyczna = sum(sr1$counts * sr1$mids) / sum(sr1$counts)
  sredniaHarmoniczna = sum(sr1$counts) / sum(sr1$counts / sr1$mids)
  sredniaGeometryczna = (prod(sr1$mids ^ sr1$counts)) ^ (1 / sum(sr1$counts))
  wariancja = sum(((sr1$mids-sredniaArytmetyczna) ^ 2) * sr1$counts) / sum(sr1$counts)
  odchylenieStandardowe = sqrt(wariancja)
  mediana = kwartyl(sr1,0.5)
  odchyleniePrzecietneMed = sum(abs(sr1$mids - mediana) * sr1$counts) / sum(sr1$counts)
  kwartylQ1 = kwartyl(sr1, 0.25)
  kwartylQ3 = kwartyl(sr1, 0.75)
  odchylenieCwiartkowe = (kwartyl(sr1, 0.75) - kwartyl(sr1, 0.25)) / 2
  wspolczynnikZmiennosci = odchylenieStandardowe / sredniaArytmetyczna * 100
  wspolczynnikAsymetrii = (sum((sr1$mids - sredniaArytmetyczna) ^ 3 * sr1$counts) / sum(sr1$counts)) / (odchylenieStandardowe ^ 3)
  wspolczynnikSkosnosci = 3*(sredniaArytmetyczna - mediana) / odchylenieStandardowe
  kurtoza = (sum((sr1$mids - sredniaArytmetyczna) ^ 4 * sr1$counts) / sum(sr1$counts)) / (odchylenieStandardowe ^ 4)
  exces = kurtoza - 3
  wynik <- c(sredniaArytmetyczna, sredniaHarmoniczna, sredniaGeometryczna, wariancja, odchylenieStandardowe, mediana, odchyleniePrzecietneMed, kwartylQ1, kwartylQ3, odchylenieCwiartkowe, wspolczynnikZmiennosci, wspolczynnikAsymetrii, wspolczynnikSkosnosci, kurtoza,exces)
  return (wynik)
}

zadanie1 <- function(wektor1, wektor2) 
{
  d_rozstep = max(wektor1)-min(wektor1)
  d_n = 50
  d_breaks = seq(min(wektor1),max(wektor1), by = d_rozstep/round(sqrt(d_n)))
  
  #miary1
  wy1a <- zad1a(wektor1)
  wy1b <- zad1b(wektor1)
  t = data.frame(szczegolowy=wy1a, rozdzielczy=wy1b)
  row.names(t) = c("Srednia arytmetyczna:", "Srednia harmoniczna:", "Srednia geometryczna:", "Wariancja nieobciazona:", "Odchylenie standardowe:", "Mediana:", "Odchylenie przecietne od mediany:","Kwartyl Q1:", "Kwartyl Q3:", "Odchylenie cwiartkowe:", "Wspolczynnik zmiennosci:", "Wspolczynnik asymetrii:", "Wspolczynnik skosnosci:", "Kurtoza:","Exces:")
  print(t)
  
  #histogram1 
  histogramD1 = hist(wektor1, breaks = d_breaks, labels = TRUE, plot = TRUE)
  
  
  
  
  d_rozstep = max(wektor2)-min(wektor2)
  d_n = 48
  d_breaks = seq(min(wektor2),max(wektor2), by = d_rozstep/round(sqrt(d_n)))
  
  #miary2
  wy1a <- zad1a(wektor2)
  wy1b <- zad1b(wektor2)
  t = data.frame(szczegolowy=wy1a, rozdzielczy=wy1b)
  row.names(t) = c("Srednia arytmetyczna:", "Srednia harmoniczna:", "Srednia geometryczna:", "Wariancja nieobciazona:", "Odchylenie standardowe:", "Mediana:", "Odchylenie przecietne od miediany:", "Kwartyl Q1:", "Kwartyl Q3:", "Odchylenie cwiartkowe:", "Wspolczynnik zmiennosci:", "Wspolczynnik asymetrii:", "Wspolczynnik skosnosci:", "Kurtoza:", "Exces:")
  print(t)
  
  #histogram2
  histogramD2 = hist(wektor2, breaks = d_breaks, labels = TRUE, plot = TRUE)
  
}

#ZADANIE 2

zadanie2 <- function(dane) 
{
  
  dane = sort(dane) # sortowanie danych
  n = length(dane) # obliczanie, ile element?w
  
  # przyjmowanie odpowiedniego k, w zale?no?ci od liczby element?w, brane z tablicy
  if(length(dane) == 50) { k = 0.1253 }
  else { k = 0.1279 }
  
  p = pnorm((dane - mean(dane))/sd(dane))
  # pnorm - The Normal Distribution ? rozk?ad normalny
  # mean - Arithmetic Mean ? ?rednia arytmetyczna
  # sd -  odchylenie standardowe 
  # seq - Sequence Generation
  
  
  Dplus = max(seq(1:n)/n - p)
  Dminus = max(p - (seq(1:n) - 1)/n)
  d = max(Dplus, Dminus)
  
  if(d < k) 
  { cat("Dane maja rozklad normalny.\n") } 
  else 
  { cat("Dane nie maja rozkladu normalnego.\n") }
  
}



