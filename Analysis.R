require(dplyr)
require(raster)

new_data <- read.csv("Woody.csv")

plot(new_data$BA, new_data$Density, main="Basal Area vs. Density",
     xlab="Basal Area", ylab="Stem Density")
new_data1 <- new_data[which(new_data$BA < 100),]
plot(new_data1$Density, new_data1$BA, main="Basal Area vs. Density",
     xlab="Stem Density", ylab="Basal Area")
new_data2 <- new_data1[which(new_data1$Density < 4000),]
plot(new_data2$Density, new_data2$BA, main="Basal Area vs. Density",
     xlab="Stem Density", ylab="Basal Area")
model <- lm(Density~BA, data = new_data2)
summary(model)

new_data$FireOn <- NULL
for (i in 1:nrow(new_data)) {
  if(new_data$FireFreq[i] == 0) { 
    new_data$FireOn[i] = FALSE
  }
  else {
    new_data$FireOn[i] = TRUE 
  }
}

library(plyr)
ba <- new_data[which(!is.na(new_data$BA)),]
ba_means <- ddply(ba, .(Site, FireOn), summarise, N = length(BA), 
               mean = mean(BA), sd = sd(BA), se = sd/sqrt(N))

for(i in 1:nrow(ba_means)) { 
  for(j in 1:nrow(new_data)) {
    if(ba_means$Site[i] == new_data$Site[j]) {
      ba_means$Temp[i] = new_data$Temp[j]
      ba_means$Precip[i] = new_data$Precip[j]
      ba_means$grassy[i] = new_data$grassy[j]
    } 
  }
}

ba_means <- ba_means[which(ba_means$mean < 75),]

library(ggplot2)


ba_precip <- ggplot(ba_means, aes(Precip, mean, 
                               colour = interaction(FireOn, grassy), 
                               shape = grassy)) + 
  geom_point() + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, 
                               show.legend = FALSE) + 
  theme_bw() + xlab("Mean Annual Precipitation") + ylab("Basal Area")

ba_precip
ba_precip + coord_trans(y = 'log10')

#Stems

stems <- new_data[which(!is.na(new_data$Density)),]
stem_means <- ddply(stems, .(Site, FireOn), summarise, N = length(Density), 
                  mean = mean(Density), sd = sd(Density), se = sd/sqrt(N))

for(i in 1:nrow(stem_means)) { 
  for(j in 1:nrow(new_data)) {
    if(stem_means$Site[i] == new_data$Site[j]) {
      stem_means$Temp[i] = new_data$Temp[j]
      stem_means$Precip[i] = new_data$Precip[j]
      stem_means$grassy[i] = new_data$grassy[j]
    } 
  }
}

stem_means <- stem_means[which(stem_means$mean < 1000),]

stem_precip <- ggplot(stem_means, aes(Precip, mean, 
                                  colour = interaction(FireOn, grassy), 
                                  shape = grassy)) + 
  geom_point() + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, 
                               show.legend = FALSE) + 
  theme_bw() + xlab("Mean Annual Precipitation") + ylab("Density (Stems/Ha)")



stem_precip
stem_precip + coord_trans(y = 'log10')

# Normalize length

norm_length <- new_data
norm_length$Duration <- 50

obj <- lm(BA ~ Duration + Precip + FireFreq*grassy, data = new_data)
obj <- lm(BA ~ Duration*Precip*Temp*FireFreq*grassy, data = new_data)




obj2 <- lm(BA ~ Duration + FireFreq + Precip, data = new_data)

norm_length$predictedba <- predict(obj, norm_length)
norm_length$diff <- norm_length$BA - norm_length$predictedba

means_norm <- ddply(norm_length, .(Site, FireOn), summarise,
                    N = length(predictedba),
                    mean = mean(predictedba), sd = sd(predictedba), se = sd/sqrt(N))

means_norm <- means_norm[which(means_norm$Site != "Okmulgee"),]
means_norm <- means_norm[which(means_norm$Site != "Wharton"),]

for(i in 1:nrow(means_norm)) {
  for(j in 1:nrow(new_data)) {
    if(means_norm$Site[i] == new_data$Site[j]) {
      means_norm$Temp[i] = new_data$Temp[j]
      means_norm$Precip[i] = new_data$Precip[j]
      means_norm$grassy[i] = new_data$grassy[j]
    }
  }
}

ba_precip2 <- ggplot(means_norm, aes(Precip, mean, 
                                  colour = interaction(FireOn, grassy), 
                                  shape = grassy)) + 
  geom_point() + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, 
                               show.legend = FALSE) + 
  theme_bw() + xlab("Mean Annual Precipitation") + ylab("Basal Area")

ba_precip2

# require(lme4) 
# obj2 <- lmer(BA ~ FireFreq + Precip + Duration + (1|Site), data=new_data)
# 
# norm_length$predictedba2 <- predict(obj2, norm_length)
# norm_length$diff2 <- norm_length$BA - norm_length$predictedba2
# 
# means_norm2 <- ddply(norm_length, .(Site, FireOn), summarise, 
#                      N = length(predictedba2), 
#                      mean = mean(predictedba2), sd = sd(predictedba2), 
#                      se = sd/sqrt(N))
# 
# for(i in 1:nrow(means_norm2)) { 
#   for(j in 1:nrow(new_data)) {
#     if(means_norm2$Site[i] == new_data$Site[j]) {
#         means_norm2$Temp[i] = new_data$Temp[j]
#         means_norm2$Precip[i] = new_data$Precip[j]
#         means_norm2$grassy[i] = new_data$grassy[j]
#     } 
#   }
# }
# 
# ba_precip3 <- ggplot(means_norm2, aes(Precip, mean, 
#                                       colour = interaction(FireOn, grassy), 
#                                       shape = grassy)) + 
#   geom_point() + theme_bw() + xlab("mean annual precipitation") + 
#   ylab("basal area") + 
# 
# ba_precip3
# ba_precip3 + coord_trans(y = 'log10')
