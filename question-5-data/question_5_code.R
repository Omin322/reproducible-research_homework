#libraries
library(dplyr)

#Download Data
dsdnavirus <- read.csv("question-5-data/Cui_etal2014.csv")
str(dsdnavirus)

#Log Transformation
dsdnalog <- mutate(dsdnavirus, vol_log = log(Virion.volume..nm.nm.nm.))
dsdnalog2 <- mutate(dsdnalog, gen_log = log(Genome.length..kb.))

#Model Relationship
dsdnamod <- lm(vol_log ~ gen_log, dsdnalog2)
summary(dsdnamod)

#Plot data and model
ggplot(aes(gen_log, vol_log), data = dsdnalog2) +
  geom_point() +
  stat_smooth(method = "lm", col = "blue") +
  xlab("log[Genome Length(kb)]") +
  ylab("log[Virion Volume(nm^3)]")

#Equation values
a <- exp(7.0748)
b <- 1.5152

#Function to find volume from genome length
allo_fun <- function(Genome.length..kb.) {
  
  N <- a*Genome.length..kb.^b
  
  return(N)
  
}

#Find volume with 300kb dsDNA
allo_fun(300)

