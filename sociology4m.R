library(foreign) 
library(lme4) 
library(sjPlot) 
library(ggplot2) 
library(ggrepel) 
library(influence.ME)
data <- read.spss("WV6_Data_spss_v_2016_01_01.sav", to.data.frame = T)
datac <- read.csv("clevel_.csv", header = T)
vars <- c(
  "V2",  #код страны
  "V104",     #Вопрос <How much you trust: People you know personally>
  "V105",     #Вопрос <How much you trust: People you meet for the first time>
  "V239",     #шкала доходов
  "V240",     #пол
  "V242",     #возраст
  "V253",     #размер населенного пункта
  "V248"      #достигнутый уровень образования
)
data <- data[vars]
rm(vars)
#Зависимая переменная:
data$trustknown <- as.numeric(data$V104)*(-1)+5
data$trustunknown <- as.numeric(data$V105)*(-1)+5
data$index <- data$trustknown + data$trustunknown
#Независимые переменные:
data$income <- as.numeric(data$V239)     #Scale of incomes
data$gndr <- as.factor(data$V240)  #Sex
data$ages <- scale(as.numeric(data$V242)) #Age
data$educ <- rep(NA, length(data$V248)) #Education
data$educ[data$V248 == "No formal education" | 
            data$V248 == "Incomplete primary school" | 
            data$V248 == "Complete primary school"] <- "primary"
data$educ[data$V248=="Incomplete secondary school: technical/ vocational type" | 
            data$V248 == "Complete secondary school: university-preparatory type" | 
            data$V248 == "Complete secondary school: technical/ vocational type" | 
            data$V248 == "Incomplete secondary school: university-preparatory type" ] <- "secondary"
data$educ[data$V248 == "Some university-level education, without degree" | 
            data$V248 == "University - level education, with degree" ] <- "tertiary"
data$educf <- as.factor(data$educ)
data$V2 <- droplevels(data$V2)
names(data)[1] <- "country"
data <- merge(data, datac, all.x = T)
data$logGDP <- scale(log(as.numeric(data$GDP)))
savevars <- c("country", "trustknown","trustunknown", "index", "income", "gndr", "ages", "educf", "logGDP", "secular")
data1 <- na.omit(data[savevars] )
#Модели
m0 <- lmer(index ~ (1 | country), REML = F, data = data1)
summary(m0) #80421 наблюдений, 56 стран
0.2039 / (0.2039 + 1.5685) # ICC = 11.5%
m1 <- lmer(index ~ ages + (1 | country), REML = F, data = data1)
m2 <- lmer(index ~ gndr + (1 | country), REML = F, data = data1)
m3 <- lmer(index ~ income + (1 | country), REML = F, data = data1)
m4 <- lmer(index ~ educf + (1 | country), REML = F, data = data1)
anova(m0, m1)
anova(m0, m2)
anova(m0, m3)
anova(m0, m4)
m5 <- lmer(index ~ educf + gndr + income + ages + (1 | country), REML = F, data = data1)
m6 <- lmer(index ~ educf + gndr + income + ages + (1 + educf | country), REML = F, data = data1)
m7 <- lmer(index ~ gndr + ages + income + educf + logGDP + (1 + educf  | country), REML = F, data = data1)
data1$educf5 <- relevel(data1$educf, ref = "secondary")#среднее образование как опорная категория
m7.1 <- lmer(index ~ gndr + ages + income + educf5 + logGDP + (1 + educf5  | country), REML = F, data = data1)
m8 <- lmer(index ~ gndr + ages + income + educf * logGDP + (1 + educf  | country), REML = F, data = data1)
sjt.lmer(m1, m2, m3, m4, m5, p.kr = F, show.ci = F, show.se = T, digits.est = 3)
sjt.lmer(m5, m6, m7, m8, p.kr = F, show.ci = F, show.se = T, digits.est = 3)
sjp.lmer(m0, sort.est = "(Intercept)" , fade.ns = T, geom.colors = c("black", "black")) #нулевая модель, средние значения доверия по странам
sjp.lmer(m5, sort.est = "(Intercept)", fade.ns = T, geom.colors = c("black", "black")) #модель 5, means as outcomes
sjp.lmer(m8, sort.est = "(Intercept)", fade.ns = T, geom.colors = c("black", "black")) #модель 8, межуровневая интеракция, рандомизированный эффект образования
sjp.int(m8, type = "cond", p.kr = F) #маргинальный  эффект образования на доверие
sjp.int(m8, type = "eff", p.kr = F, show.ci = T) #межуровневая интеракция
# Диагностика модели
countrytrust <- tapply(data1$index, data1$country, mean) #визуальная оценка
countrytrust <- as.data.frame(countrytrust)
countrytrust$country <- rownames(countrytrust)
datac1 <- merge(datac, countrytrust, all.x = T)
datac1 <- na.omit(datac1[,c(1,4,6,10)])
ggplot(datac1) + 
  geom_point(aes(log(datac1$GDP), datac1$countrytrust), color = 'black') + 
  geom_text_repel(aes(log(datac1$GDP), datac1$countrytrust, label = datac1$country.code3)) + 
  theme_classic(base_size = 16) + labs(x = "логарифм ВВП", y = "индекс доверия")
m8i <- influence(m8, group = "country") #комплексная диагностика итоговой модели
cooktable <- cooks.distance(m8i) #дистанции Кука
plot(m8i, which = "cook", cutoff = 4/56, sort = T, xlab = "Cook's Distance", ylab = "Country")
dftable <- dfbetas(m8i) #DFBETAs
plot(m8i, which = "dfbetas", cutoff = 2/sqrt(56), xlab = "Cook's Distance", ylab = "Country")
