library(tidyverse)
# Author: Gabi Overgaard Øvlisen
# Description: PhD - Basal statistik - Opgaver til uge 36


# Opgave 1 - Indlæsning af data
wr <- read.table("Dropbox/Gabi og Lars (1)/Ph.d.-kursus i basal statistik, hold 7/Data/wright.txt", header=T)

# Opgave 2 - gennemsnit og differencer
wr$wright_gennemsnit <- ((wr$wright1 + wr$wright2) / 2)
wr$mini_gennemsnit <- ((wr$mini1 + wr$mini2) / 2)
wr$wright_diff <- (wr$wright1 - wr$wright2)
wr$mini_diff <- (wr$mini1 - wr$mini2)

# Bland-Altman Plot
plot(x = wr$wright_gennemsnit, y = wr$wright_dif, xlab = "Average", ylab = "diff") 
abline(0,0,col="red",lwd=2)

# Opgave 3 - Reproducérbarhed - normalområder for differencer
wright_diff_gennemsnit <- mean(x = wr$wright_diff)
mini_diff_gennemsnit <- mean(x = wr$mini_diff)
wright_diff_SD <- sd(wr$wright_diff)
mini_diff_SD <- sd(wr$mini_diff)
wright_SD_plus <- (wright_diff_gennemsnit + (2 * wright_diff_SD))
wright_SD_minus <- (wright_diff_gennemsnit - (2 * wright_diff_SD))
mini_SD_plus <- (mini_diff_gennemsnit + (2 * mini_diff_SD))
mini_SD_minus <- (mini_diff_gennemsnit - (2 * mini_diff_SD))
# Antager at gennemsnittet og standardafvigelsen af differenserne er konstante. 
# Ligegyldig omfanget af den målte enhed antager man at gennemsnittet og standardafvigelsen er den samme.
# ikke særlige rimelige, får negative værdier.

plot(wr$wright_gennemsnit, wr$wright_diff) 
abline(wright_SD_minus,0,col="green",lwd=2)
abline(wright_SD_plus,0,col="green",lwd=2)
plot(wr$mini_gennemsnit, wr$mini_diff, ylim = c(-100,100))
abline(mini_SD_minus,0,col="green",lwd=2)
abline(mini_SD_plus,0,col="green",lwd=2)

# Opgave 4 - Bedste reproducerbarhed
wright_abs_range <- wright_SD_plus - wright_SD_minus
mini_abs_range <- mini_SD_plus - mini_SD_minus
# hvor målingerne mindste spredning, hvor forskellen/ spredningen mellem måingerne indenfor samme metode er mindst = wright

# Opgave 5 - Tegn et scatter plot af de numeriske differenser mellem dobbeltbestemmelser,
# med de to metoder på hver sin akse, og vurder på baggrund af dette, 
# om der er nogle personer, der ser ud til at være mere ustabile at måle på end andre.

ggplot(wr, aes(x=wright_diff, y=mini_diff)) + geom_point(size=2) + geom_text(label=rownames(wr), vjust = -0.8)
# Ingen ser ud til at være usikre at måle på begge metoder. I så fald ville de ligge helt ude i et af hvert hjørne.

# Opgave 6 : Overensstemmelse - Sammenlign nu gennemsnittene af dobbeltbestemmelserne
# for de to metoder, dvs. tegn igen Bland-Altman plot og udregn limits of agreement, 
# denne gang for sammenligning af de to målemetoder. Kommenter den kliniske anvendelighed af disse grænser.
wr$wright_mini_gennemsnit <- ((wr$wright_gennemsnit + wr$mini_gennemsnit) / 2)
wr$wright_mini_diff <- ((wr$wright_gennemsnit) - (wr$mini_gennemsnit))
m <- mean(wr$wright_mini_diff)
sdm <- sd(wr$wright_mini_diff)
m + 2*sdm
m - 2*sdm
m

# Opgave 7


