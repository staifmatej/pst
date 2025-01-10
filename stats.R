# R code used in .ipynb for visualization of statistics on the GitHub profile.

data("case0402", package = "Sleuth2")

# Nastavení seed pro opakovatelnost.
set.seed(123)

# 

# Načtení knihovny
library(knitr)
library(crayon)
library(ggplot2)

cat("\n")
cat(bold("Výpis prvních několika řádků dat:"))
kable(head(case0402), format = "simple")
cat("\n")

#

# Zobrazení struktury dat
cat("\n")
cat(bold("Struktura dat:\n"))
str(case0402)

# Tabulka prvních řádků dat v hezčím formátu
cat(bold("\nUkázka dat:\n"))
kable(head(case0402), format = "pipe", align = "c")  # Formát 'pipe' zlepší čitelnost

# Souhrnný přehled dat
cat(bold("\nSouhrnný přehled dat:\n"))
summary_table <- as.data.frame(summary(case0402))  # Převod na tabulku
kable(summary_table, format = "pipe", align = "l")  # Formátování tabulky

cat(bold("\nVizualizace grafů:\n"))

# Boxplot podle skupin
ggplot(case0402, aes(x = Treatmt, y = Time, fill = Treatmt)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Modified" = "lightgreen", "Conventional" = "skyblue")) +
  labs(title = "Boxplot of Time by Groups",
       x = "Group",
       y = "Time") +
  theme_minimal()

# Histogramy pro jednotlivé skupiny
ggplot(case0402, aes(x = Time, fill = Treatmt)) +
  geom_histogram(position = "dodge", bins = 10, alpha = 0.7) +
  scale_fill_manual(values = c("Modified" = "lightgreen", "Conventional" = "skyblue")) +
  labs(title = "Histogram of Time by Groups",
       x = "Time",
       y = "Count") +
  theme_minimal()

# Hustotní graf (Density plot) podle skupin
ggplot(case0402, aes(x = Time, color = Treatmt)) +
  geom_density(linewidth = 1) +  # Použití linewidth místo size
  scale_color_manual(values = c("Modified" = "darkgreen", "Conventional" = "steelblue")) +
  labs(title = "Density of Time by Groups",
       x = "Time",
       y = "Density") +
  theme_minimal()

#

library(ggplot2)

# Rozdělení na dvě skupiny podle sloupce 'Treatmt'
group1 <- subset(case0402, Treatmt == "Modified")  # První skupina
group2 <- subset(case0402, Treatmt == "Conventional")  # Druhá skupina

# Výpočty pro skupinu 1
mean1 <- mean(group1$Time)  # Střední hodnota
var1 <- var(group1$Time)    # Rozptyl
median1 <- median(group1$Time)  # Medián

# Výpočty pro skupinu 2
mean2 <- mean(group2
Time)
median2 <- median(group2$Time)

# Dynamický výstup s popisem pro skupinu 1
cat(bold("\nVýsledky pro první skupinu (Modified):"))
cat("\n- Střední hodnota (mean):", mean1, "\n")
cat("- Rozptyl (variance):", var1, "\n")
cat("- Medián (median):", median1, "\n\n")

# Dynamický výstup s popisem pro skupinu 2
cat(bold("Výsledky pro druhou skupinu (Conventional):"))
cat("\n- Střední hodnota (mean):", mean2, "\n")
cat("- Rozptyl (variance):", var2, "\n")
cat("- Medián (median):", median2, "\n\n")

#

# Načtení datového souboru
data("case0402", package = "Sleuth2")

# Rozdělení na dvě skupiny podle sloupce 'Treatmt'
group1 <- subset(case0402, Treatmt == "Modified")  # Skupina 1
group2 <- subset(case0402, Treatmt == "Conventional")  # Skupina 2

# Vykreslení histogramu pro skupinu 1
hist(
  group1$Time,
  main = "Histogram - Group 1 (Modified)",
  xlab = "Time",
  ylab = "Density", 
  col = "lightgreen",  # Barva výplně
  border = "darkgreen",  # Barva okrajů
  freq = FALSE # Nastavení pro hustotu místo frekvence
)
lines(density(group1$Time), col = "forestgreen", lwd = 2) # Hustotní křivka

# Vykreslení histogramu pro skupinu 2
hist(
  group2$Time,
  main = "Histogram - Group 2 (Conventional)",
  xlab = "Time",
  ylab = "Density", 
  col = "skyblue",  # Barva výplně
  border = "steelblue",  # Barva okrajů
  freq = FALSE
)
lines(density(group2$Time), col = "navy", lwd = 2) # Hustotní křivka

# Empirická distribuční funkce pro skupinu 1
plot(
  ecdf(group1$Time),
  main = "Empirical Distribution Function - Group 1 (Modified)",
  xlab = "Time",
  ylab = "Probability",
  col = "darkgreen",  
  lwd = 2
)

# Empirická distribuční funkce pro skupinu 2
plot(
  ecdf(group2$Time),
  main = "Empirical Distribution Function - Group 2 (Conventional)",
  xlab = "Time",
  ylab = "Probability",
  col = "navy", 
  lwd = 2
)

#

# Funkce pro vykreslení hustoty s histogramem
plot_densities <- function(data, group) {
  # Histogram
  hist(
    data,
    freq = FALSE,
    main = paste("Histogram with densities -", group),
    xlab = "Time",
    ylab = "Density",
    col = if (group == "Group 1 (Modified)") "lightgreen" else "skyblue",
    border = if (group == "Group 1 (Modified)") "darkgreen" else "steelblue"
  )
  
  # Odhad parametrů
  mean_val <- mean(data)  # Průměr - odhadnut jako aritmetický průměr dat
  sd_val <- sd(data)      # Směrodatná odchylka - vypočtena z dat pomocí standardní funkce sd()
  lambda_val <- 1 / mean_val  # Exponenciální parametr - odhadnut jako 1/průměr
  min_val <- min(data)    # Minimum pro rovnoměrné rozdělení - získáno jako minimum z dat
  max_val <- max(data)    # Maximum pro rovnoměrné rozdělení - získáno jako maximum z dat
  
  # Vysvětlení odhadu
  cat(bold(paste0("\nVysvětlení odhadu parametrů pro ", group,":")), "\n")
  cat("- Normální rozdělení: Průměr (aritmetický průměr) a směrodatná odchylka vypočtené přímo z dat.\n")
  cat("- Exponenciální rozdělení: Lambda (intenzita) je odhadnuta jako 1/průměr.\n")
  cat("- Rovnoměrné rozdělení: Min a Max jsou odhadnuty jako nejmenší a největší hodnota dat.\n\n")
  
  # Normální hustota
  curve(dnorm(x, mean = mean_val, sd = sd_val),
        col = "red", lwd = 2, add = TRUE, lty = 1)
  
  # Exponenciální hustota
  curve(dexp(x, rate = lambda_val),
        col = "blue", lwd = 2, add = TRUE, lty = 2)
  
  # Rovnoměrná hustota
  curve(dunif(x, min = min_val, max = max_val),
        col = "purple", lwd = 2, add = TRUE, lty = 3)
  
  # Legenda
  legend_position <- if (group == "Group 1 (Modified)") "topright" else "topleft"
  legend(legend_position, legend = c("Normal (Normalni)", "Exponential (Exponencialni)", "Uniform (Rovnomerne)"),
         col = c("red", "blue", "purple"), lty = 1:3, cex = 0.8)

  # Výpis parametrů
  cat(bold(paste0("Vysvětlení odhadu parametrů pro ", group, ":")), "\n")
  cat("- Normální: průměr =", round(mean_val, 2), "\n")
  cat("- Směrodatná odchylka =", round(sd_val, 2), "\n")
  cat("- Exponenciální: lambda =", round(lambda_val, 2), "\n")
  cat("- Rovnoměrné: min =", round(min_val, 2), ", max =", round(max_val, 2), "\n\n")
  
  # Diskuze pro jednotlivé skupiny
  if (group == "Group 1 (Modified)") {
    cat(bold("Diskuze a zamyšlení pro Group 1 (Modified):"))
    cat("\nPo analýze grafu je zřejmé, že data Group 1 (Modified) lépe odpovídají exponenciálnímu rozdělení. Tvar histogramu a exponenciální hustota ukazují na asymetrické rozdělení s větším počtem nižších hodnot času.\n\n")
  } else if (group == "Group 2 (Conventional)") {
    cat(bold("Diskuze a zamyšlení:"))
    cat("\nGroup 2 (Conventional):\n")
    cat("Po analýze grafu je zřejmé, že data Group 2 (Conventional) nevykazují jednoznačné odpovídající rozdělení. I když normální rozdělení (červená čára) přibližně odpovídá tvaru histogramu, rozdíly v okrajích dat naznačují odchylky od teoretického normálního rozložení.\n")
    cat("\n")
    cat("Exponenciální rozdělení (modrá čára) ani rovnoměrné rozdělení (fialová čára) se však histogramu nepřibližují dostatečně. Nejlepší aproximací se tedy jeví normální rozdělení, ačkoliv s jistými výhradami. Tato skupina dat pravděpodobně vyžaduje detailnější analýzu nebo testy pro přesnější identifikaci správného modelu rozdělení.\n\n")
  }
}

# Vykreslení pro skupinu 1
plot_densities(group1$Time, "Group 1 (Modified)")

# Vykreslení pro skupinu 2
plot_densities(group2$Time, "Group 2 (Conventional)")

#

# Funkce pro generování a porovnání simulovaných dat
compare_histograms_improved <- function(data, simulated_data, group, distribution_type) {
  
  # Definice společných intervalů (breaks) pro oba histogramy
  breaks_common <- seq(
    floor(min(c(data, simulated_data))),
    ceiling(max(c(data, simulated_data))),
    length.out = 15
  )
  
# Nastavení okrajů a rozložení pro grafy pod sebou
layout(matrix(c(1, 2), nrow = 2, ncol = 1, byrow = TRUE))  # Grafy pod sebou

# Histogram původních dat
hist(
  data,
  breaks = breaks_common,
  freq = FALSE,
  main = paste("Original Data -", group),
  xlab = "Time",
  ylab = "Density",
  col = "lightgray",
  border = "darkgray",
  ylim = c(0, max(density(data)
y))
)
lines(density(data), col = "black", lwd = 2)  # Hustota původních dat

# Histogram simulovaných dat
hist(
  simulated_data,
  breaks = breaks_common,
  freq = FALSE,
  main = paste("Simulated Data -", group),
  xlab = "Time",
  ylab = "Density",
  col = if (distribution_type == "Exponenciální") "lightgreen" else "skyblue",
  border = if (distribution_type == "Exponenciální") "darkgreen" else "steelblue",
  ylim = c(0, max(density(data)
y))
)
lines(density(simulated_data), col = if (distribution_type == "Exponenciální") "green" else "blue", lwd = 2, lty = 2)  # Hustota simulovaných dat

# Resetování rozložení
layout(1)

  
# Číselné shrnutí
cat("\n")
cat(bold(paste0("Číselné shrnutí pro ", group, ":")), "\n")
cat("\n")
        
cat(bold("Původní data:\n"))
 cat("- Minimum:", round(min(data), 2), "\n")
  cat("- První kvartil:", round(quantile(data, 0.25), 2), "\n")
  cat("- Medián:", round(median(data), 2), "\n")
  cat("- Průměr:", round(mean(data), 2), "\n") 
  cat("- Třetí kvartil:", round(quantile(data, 0.75), 2), "\n")
  cat("- Maximum:", round(max(data), 2), "\n")
  cat("- Směrodatná odchylka:", round(sd(data), 2), "\n")
  cat("\n")
        
cat(bold("Simulovaná data:\n"))
  cat("- Minimum:", round(min(simulated_data), 2), "\n")
  cat("- První kvartil:", round(quantile(simulated_data, 0.25), 2), "\n")
  cat("- Medián:", round(median(simulated_data), 2), "\n")
  cat("- Průměr:", round(mean(simulated_data), 2), "\n")
  cat("- Třetí kvartil:", round(quantile(simulated_data, 0.75), 2), "\n")
  cat("- Maximum:", round(max(simulated_data), 2), "\n")
  cat("- Směrodatná odchylka:", round(sd(simulated_data), 2), "\n")
  cat("\n")
}


# Generování simulovaných dat a porovnání pro Group 1 (Modified) - Exponenciální rozdělení
lambda1 <- 1 / mean(group1$Time)
simulated_group1 <- rexp(100, rate = lambda1)
compare_histograms_improved(group1$Time, simulated_group1, "Group 1 (Modified)", "Exponenciální")

# Generování simulovaných dat a porovnání pro Group 2 (Conventional) - Normální rozdělení
mean2 <- mean(group2
Time)
simulated_group2 <- rnorm(100, mean = mean2, sd = sd2)
compare_histograms_improved(group2$Time, simulated_group2, "Group 2 (Conventional)", "Normální")

# Diskuze
cat(bold("Diskuze:"))
cat("\nPro Group 1 (Modified) byla použita exponenciální simulace. Histogram simulovaných dat ukazuje podobnou asymetrii a tvar jako původní data, přičemž simulovaná data mají o něco ostřejší rozložení.\n")
cat("Číselné shrnutí potvrzuje, že průměr a medián jsou blízké, ale směrodatná odchylka může být mírně odlišná.\n\n")

cat("Pro Group 2 (Conventional) byla použita normální simulace. Histogram simulovaných dat se dobře shoduje s původními daty, zejména ve tvaru kolem průměru.\n")
cat("Číselné shrnutí ukazuje, že průměr a medián simulovaných dat jsou velmi podobné původním datům, což potvrzuje vhodnost normálního rozdělení pro tuto skupinu.\n")

#

cat("\n")
# Funkce pro výpočet a výpis 95% konfidenčního intervalu pro střední hodnotu
calculate_confidence_interval <- function(data, group_name) {
# Použití t.test pro získání konfidenčního intervalu
  test <- t.test(data, conf.level = 0.95)
  
# Výpis výsledků    
  cat(bold("Skupina:", group_name))
  cat("\n")
  cat("95% konfidenční interval pro střední hodnotu:", 
      round(test$conf.int[1], 2), "-", round(test$conf.int[2], 2), "\n\n")
}

# Výpočet konfidenčního intervalu pro Group 1 (Modified)
calculate_confidence_interval(group1$Time, "Group 1 (Modified)")

# Výpočet konfidenčního intervalu pro Group 2 (Conventional)
calculate_confidence_interval(group2$Time, "Group 2 (Conventional)")

#

cat("\n")
# Rozdělení dat na skupiny
group1 <- subset(case0402, Treatmt == "Modified")
group2 <- subset(case0402, Treatmt == "Conventional")

# Definice hodnoty K
K <- 19 # den mých narozenin

# Funkce pro provedení dvouvýběrového t-testu a výpis výsledků
perform_group_comparison <- function(data1, data2) {
  # Provedení dvouvýběrového t-testu
  test <- t.test(data1, data2, alternative = "two.sided", var.equal = TRUE)
  
  # Výpis výsledků
  cat(bold("Porovnání skupin (Modified vs. Conventional):"))
  cat("\n")
  cat("- Hypotéza H0: Střední hodnoty obou skupin jsou shodné.\n")
  cat("- Hypotéza Ha: Střední hodnoty obou skupin se liší.\n")
  cat("- Testová statistika (t):", round(test$statistic, 3), "\n")
  cat("- P-hodnota:", round(test$p.value, 4), "\n")
  cat("\n")

  # Rozhodnutí o zamítnutí H0:
  cat(bold("Rozhodnutí o zamítnutí H0")) 
  cat("\n")
  if (test$p.value < 0.05) {
    cat("Zamítáme H0 na hladině významnosti 5 %. Střední hodnoty skupin se liší.\n")
  } else {
    cat("Nezamítáme H0 na hladině významnosti 5 %. Střední hodnoty skupin se neliší.\n")
  }
  return(test)
}

# Vizualizace dat pomocí boxplotu s jednotnými barvami
ggplot(case0402, aes(x = Treatmt, y = Time, fill = Treatmt)) +
  geom_boxplot(color = c("darkgreen", "steelblue"), lwd = 1.2) +  # Okraje podle barev
  scale_fill_manual(values = c("Modified" = "lightgreen", "Conventional" = "skyblue")) + # Jednotné výplně
  labs(title = "Boxplot of Time by Groups",
       x = "Skupina",
       y = "Time") +
  theme_minimal()

# Provedení testu pro obě skupiny
t_test_result <- perform_group_comparison(group1
Time)

# Praktická interpretace výsledků
  cat("\n")

if (t_test_result$p.value < 0.05) {
  cat(bold("Interpretace:"))
  cat("\n")
  cat("Na hladině významnosti 5 % existují dostatečné důkazy pro tvrzení, že se průměrné hodnoty času mezi skupinami 'Modified' a 'Conventional' liší.\n")
} else {
  cat(bold("Interpretace:"))
  cat("\n")
  cat("Na hladině významnosti 5 % neexistují dostatečné důkazy pro tvrzení, že se průměrné hodnoty času mezi skupinami 'Modified' a 'Conventional' liší.\n")
}
cat("\n")

###

 data("case0402", package = "Sleuth2")

# Setting seed for reproducibility.
set.seed(123)
     

# Loading libraries
library(knitr)
library(crayon)
library(ggplot2)

cat("\n")
cat(bold("Displaying the first few rows of data:"))
kable(head(case0402), format = "simple")
cat("\n")
     
#
      
# Displaying the structure of the data
cat("\n")
cat(bold("Data Structure:\n"))
str(case0402)

# Table of the first rows of data in a nicer format
cat(bold("\nData Sample:\n"))
kable(head(case0402), format = "pipe", align = "c")  # The 'pipe' format improves readability

# Summary overview of the data
cat(bold("\nSummary Overview of Data:\n"))
summary_table <- as.data.frame(summary(case0402))  # Convert to a table
kable(summary_table, format = "pipe", align = "l")  # Table formatting

cat(bold("\nVisualization of Graphs:\n"))

# Boxplot by groups
ggplot(case0402, aes(x = Treatmt, y = Time, fill = Treatmt)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Modified" = "lightgreen", "Conventional" = "skyblue")) +
  labs(title = "Boxplot of Time by Groups",
       x = "Group",
       y = "Time") +
  theme_minimal()

# Histograms for individual groups
ggplot(case0402, aes(x = Time, fill = Treatmt)) +
  geom_histogram(position = "dodge", bins = 10, alpha = 0.7) +
  scale_fill_manual(values = c("Modified" = "lightgreen", "Conventional" = "skyblue")) +
  labs(title = "Histogram of Time by Groups",
       x = "Time",
       y = "Count") +
  theme_minimal()

# Density plot by groups
ggplot(case0402, aes(x = Time, color = Treatmt)) +
  geom_density(linewidth = 1) +  # Using linewidth instead of size
  scale_color_manual(values = c("Modified" = "darkgreen", "Conventional" = "steelblue")) +
  labs(title = "Density of Time by Groups",
       x = "Time",
       y = "Density") +
  theme_minimal()
      
#    
    
library(ggplot2)

# Splitting into two groups based on the 'Treatmt' column
group1 <- subset(case0402, Treatmt == "Modified")  # First group
group2 <- subset(case0402, Treatmt == "Conventional")  # Second group

# Calculations for group 1
mean1 <- mean(group1$Time)  # Mean
var1 <- var(group1$Time)    # Variance
median1 <- median(group1$Time)  # Median

# Calculations for group 2
mean2 <- mean(group2
Time)
median2 <- median(group2$Time)

# Dynamic output with description for group 1
cat(bold("\nResults for the first group (Modified):"))
cat("\n- Mean:", mean1, "\n")
cat("- Variance:", var1, "\n")
cat("- Median:", median1, "\n\n")

# Dynamic output with description for group 2
cat(bold("Results for the second group (Conventional):"))
cat("\n- Mean:", mean2, "\n")
cat("- Variance:", var2, "\n")
cat("- Median:", median2, "\n\n")

#

# Loading the data file
data("case0402", package = "Sleuth2")

# Splitting into two groups based on the 'Treatmt' column
group1 <- subset(case0402, Treatmt == "Modified")  # Group 1
group2 <- subset(case0402, Treatmt == "Conventional")  # Group 2

# Plotting histogram for group 1
hist(
  group1$Time,
  main = "Histogram - Group 1 (Modified)",
  xlab = "Time",
  ylab = "Density", 
  col = "lightgreen",  # Fill color
  border = "darkgreen",  # Border color
  freq = FALSE # Setting for density instead of frequency
)
lines(density(group1$Time), col = "forestgreen", lwd = 2) # Density curve

# Plotting histogram for group 2
hist(
  group2$Time,
  main = "Histogram - Group 2 (Conventional)",
  xlab = "Time",
  ylab = "Density", 
  col = "skyblue",  # Fill color
  border = "steelblue",  # Border color
  freq = FALSE
)
lines(density(group2$Time), col = "navy", lwd = 2) # Density curve

# Empirical distribution function for group 1
plot(
  ecdf(group1$Time),
  main = "Empirical Distribution Function - Group 1 (Modified)",
  xlab = "Time",
  ylab = "Probability",
  col = "darkgreen",  
  lwd = 2
)

# Empirical distribution function for group 2
plot(
  ecdf(group2$Time),
  main = "Empirical Distribution Function - Group 2 (Conventional)",
  xlab = "Time",
  ylab = "Probability",
  col = "navy", 
  lwd = 2
)      

#

cat("\n")
# Function to plot density with histogram
plot_densities <- function(data, group) {
  # Histogram
  hist(
    data,
    freq = FALSE,
    main = paste("Histogram with Densities -", group),
    xlab = "Time",
    ylab = "Density",
    col = if (group == "Group 1 (Modified)") "lightgreen" else "skyblue",
    border = if (group == "Group 1 (Modified)") "darkgreen" else "steelblue"
  )
  
  # Parameter estimation
  mean_val <- mean(data)  # Mean - estimated as the arithmetic average of the data
  sd_val <- sd(data)      # Standard deviation - calculated from the data using the standard sd() function
  lambda_val <- 1 / mean_val  # Exponential parameter - estimated as 1/mean
  min_val <- min(data)    # Minimum for uniform distribution - obtained as the minimum of the data
  max_val <- max(data)    # Maximum for uniform distribution - obtained as the maximum of the data
  
  # Explanation of parameter estimation
  cat(bold(paste0("\nExplanation of Parameter Estimation for ", group, ":")), "\n")
  cat("- Normal Distribution: Mean (arithmetic average) and standard deviation calculated directly from the data.\n")
  cat("- Exponential Distribution: Lambda (rate) is estimated as 1/mean.\n")
  cat("- Uniform Distribution: Min and Max are estimated as the smallest and largest values of the data.\n\n")
  
  # Normal density
  curve(dnorm(x, mean = mean_val, sd = sd_val),
        col = "red", lwd = 2, add = TRUE, lty = 1)
  
  # Exponential density
  curve(dexp(x, rate = lambda_val),
        col = "blue", lwd = 2, add = TRUE, lty = 2)
  
  # Uniform density
  curve(dunif(x, min = min_val, max = max_val),
        col = "purple", lwd = 2, add = TRUE, lty = 3)
  
  # Legend
  legend_position <- if (group == "Group 1 (Modified)") "topright" else "topleft"
  legend(legend_position, legend = c("Normal", "Exponential", "Uniform"),
         col = c("red", "blue", "purple"), lty = 1:3, cex = 0.8)

  # Displaying parameters
  cat(bold(paste0("Explanation of Parameter Estimation for ", group, ":")), "\n")
  cat("- Normal: mean =", round(mean_val, 2), "\n")
  cat("- Standard Deviation =", round(sd_val, 2), "\n")
  cat("- Exponential: lambda =", round(lambda_val, 2), "\n")
  cat("- Uniform: min =", round(min_val, 2), ", max =", round(max_val, 2), "\n\n")
  
  # Discussion for individual groups
  if (group == "Group 1 (Modified)") {
    cat(bold("Discussion and Reflection for Group 1 (Modified):"))
    cat("\nAfter analyzing the graph, it is evident that the data for Group 1 (Modified) better fits an exponential distribution. The shape of the histogram and the exponential density curve indicate an asymmetrical distribution with a higher number of lower time values.\n\n")
  } else if (group == "Group 2 (Conventional)") {
    cat(bold("Discussion and Reflection:"))
    cat("\nGroup 2 (Conventional):\n")
    cat("After analyzing the graph, it is clear that the data for Group 2 (Conventional) do not exhibit a clearly fitting distribution. Although the normal distribution (red line) roughly matches the shape of the histogram, differences at the data extremes suggest deviations from the theoretical normal distribution.\n")
    cat("\n")
    cat("However, the exponential distribution (blue line) and the uniform distribution (purple line) do not sufficiently approximate the histogram. Therefore, the normal distribution appears to be the best approximation, albeit with certain reservations. This data group likely requires a more detailed analysis or tests for a more accurate identification of the appropriate distribution model.\n\n")
  }
}

# Plotting for Group 1
plot_densities(group1$Time, "Group 1 (Modified)")

# Plotting for Group 2
plot_densities(group2$Time, "Group 2 (Conventional)")

#

# Function to generate and compare simulated data
compare_histograms_improved <- function(data, simulated_data, group, distribution_type) {
  
  # Define common breaks for both histograms
  breaks_common <- seq(
    floor(min(c(data, simulated_data))),
    ceiling(max(c(data, simulated_data))),
    length.out = 15
  )
  
  # Setting layout for vertically stacked plots
  layout(matrix(c(1, 2), nrow = 2, ncol = 1, byrow = TRUE))  # Plots stacked vertically
  
  # Histogram of original data
  hist(
    data,
    breaks = breaks_common,
    freq = FALSE,
    main = paste("Original Data -", group),
    xlab = "Time",
    ylab = "Density",
    col = "lightgray",  # Fill color
    border = "darkgray",  # Border color
    ylim = c(0, max(density(data)
y))
  )
  lines(density(data), col = "black", lwd = 2)  # Density of original data
  
  # Histogram of simulated data
  hist(
    simulated_data,
    breaks = breaks_common,
    freq = FALSE,
    main = paste("Simulated Data -", group),
    xlab = "Time",
    ylab = "Density",
    col = if (distribution_type == "Exponential") "lightgreen" else "skyblue",
    border = if (distribution_type == "Exponential") "darkgreen" else "steelblue",
    ylim = c(0, max(density(data)
y))
  )
  lines(density(simulated_data), col = if (distribution_type == "Exponential") "green" else "blue", lwd = 2, lty = 2)  # Density of simulated data
  
  # Resetting layout
  layout(1)
  
  # Numerical summary
  cat("\n")
  cat(bold(paste0("Numerical Summary for ", group, ":")), "\n")
  cat("\n")
          
  cat(bold("Original Data:\n"))
  cat("- Minimum:", round(min(data), 2), "\n")
  cat("- First Quartile:", round(quantile(data, 0.25), 2), "\n")
  cat("- Median:", round(median(data), 2), "\n")
  cat("- Mean:", round(mean(data), 2), "\n") 
  cat("- Third Quartile:", round(quantile(data, 0.75), 2), "\n")
  cat("- Maximum:", round(max(data), 2), "\n")
  cat("- Standard Deviation:", round(sd(data), 2), "\n")
  cat("\n")
          
  cat(bold("Simulated Data:\n"))
  cat("- Minimum:", round(min(simulated_data), 2), "\n")
  cat("- First Quartile:", round(quantile(simulated_data, 0.25), 2), "\n")
  cat("- Median:", round(median(simulated_data), 2), "\n")
  cat("- Mean:", round(mean(simulated_data), 2), "\n")
  cat("- Third Quartile:", round(quantile(simulated_data, 0.75), 2), "\n")
  cat("- Maximum:", round(max(simulated_data), 2), "\n")
  cat("- Standard Deviation:", round(sd(simulated_data), 2), "\n")
  cat("\n")
}


# Generating simulated data and comparison for Group 1 (Modified) - Exponential distribution
lambda1 <- 1 / mean(group1$Time)
simulated_group1 <- rexp(100, rate = lambda1)
compare_histograms_improved(group1$Time, simulated_group1, "Group 1 (Modified)", "Exponential")

# Generating simulated data and comparison for Group 2 (Conventional) - Normal distribution
mean2 <- mean(group2
Time)
simulated_group2 <- rnorm(100, mean = mean2, sd = sd2)
compare_histograms_improved(group2$Time, simulated_group2, "Group 2 (Conventional)", "Normal")

# Discussion
cat(bold("Discussion:"))
cat("\nFor Group 1 (Modified), an exponential simulation was used. The histogram of the simulated data shows a similar skewness and shape to the original data, with the simulated data having a slightly sharper distribution.\n")
cat("The numerical summary confirms that the mean and median are close, but the standard deviation may differ slightly.\n\n")

cat("For Group 2 (Conventional), a normal simulation was used. The histogram of the simulated data aligns well with the original data, especially around the mean.\n")
cat("The numerical summary shows that the mean and median of the simulated data are very similar to the original data, confirming the suitability of the normal distribution for this group.\n")

#

cat("\n")
# Function to calculate and display the 95% confidence interval for the mean
calculate_confidence_interval <- function(data, group_name) {
  # Using t.test to obtain the confidence interval
  test <- t.test(data, conf.level = 0.95)
  
  # Displaying the results    
  cat(bold("Group:", group_name))
  cat("\n")
  cat("95% Confidence Interval for the Mean:", 
      round(test$conf.int[1], 2), "-", round(test$conf.int[2], 2), "\n\n")
}

# Calculating the confidence interval for Group 1 (Modified)
calculate_confidence_interval(group1$Time, "Group 1 (Modified)")

# Calculating the confidence interval for Group 2 (Conventional)
calculate_confidence_interval(group2$Time, "Group 2 (Conventional)")

#        
cat("\n")
# Function to calculate and display the 95% confidence interval for the mean
calculate_confidence_interval <- function(data, group_name) {
  # Using t.test to obtain the confidence interval
  test <- t.test(data, conf.level = 0.95)
  
  # Displaying the results    
  cat(bold("Group:", group_name))
  cat("\n")
  cat("95% Confidence Interval for the Mean:", 
      round(test$conf.int[1], 2), "-", round(test$conf.int[2], 2), "\n\n")
}

# Calculating the confidence interval for Group 1 (Modified)
calculate_confidence_interval(group1$Time, "Group 1 (Modified)")

# Calculating the confidence interval for Group 2 (Conventional)
calculate_confidence_interval(group2$Time, "Group 2 (Conventional)")

cat("\n")
# Splitting data into groups
group1 <- subset(case0402, Treatmt == "Modified")
group2 <- subset(case0402, Treatmt == "Conventional")

# Defining the value of K
K <- 19 # the day of my birthday

# Function to perform a two-sample t-test and display the results
perform_group_comparison <- function(data1, data2) {
  # Performing a two-sample t-test
  test <- t.test(data1, data2, alternative = "two.sided", var.equal = TRUE)
  
  # Displaying the results
  cat(bold("Group Comparison (Modified vs. Conventional):"))
  cat("\n")
  cat("- Null Hypothesis H0: The means of both groups are equal.\n")
  cat("- Alternative Hypothesis Ha: The means of both groups are different.\n")
  cat("- Test Statistic (t):", round(test$statistic, 3), "\n")
  cat("- P-value:", round(test$p.value, 4), "\n")
  cat("\n")

  # Decision on rejecting H0:
  cat(bold("Decision on Rejecting H0")) 
  cat("\n")
  if (test$p.value < 0.05) {
    cat("We reject H0 at the 5% significance level. The means of the groups are different.\n")
  } else {
    cat("We do not reject H0 at the 5% significance level. The means of the groups are not different.\n")
  }
  return(test)
}

# Visualization of data using boxplot with consistent colors
ggplot(case0402, aes(x = Treatmt, y = Time, fill = Treatmt)) +
  geom_boxplot(color = c("darkgreen", "steelblue"), lwd = 1.2) +  # Borders according to colors
  scale_fill_manual(values = c("Modified" = "lightgreen", "Conventional" = "skyblue")) + # Consistent fills
  labs(title = "Boxplot of Time by Groups",
       x = "Group",
       y = "Time") +
  theme_minimal()

# Performing the test for both groups
t_test_result <- perform_group_comparison(group1
Time)

# Practical interpretation of the results
cat("\n")

if (t_test_result$p.value < 0.05) {
  cat(bold("Interpretation:"))
  cat("\n")
  cat("At the 5% significance level, there is sufficient evidence to state that the average time values between the 'Modified' and 'Conventional' groups differ.\n")
} else {
  cat(bold("Interpretation:"))
  cat("\n")
  cat("At the 5% significance level, there is insufficient evidence to state that the average time values between the 'Modified' and 'Conventional' groups differ.\n")
}
cat("\n")

# end.

        
        
        
