
# Librerias ---------------------------------------------------------------

library(choroplethr)
library(funModeling)
library(tidyverse)
library(ggthemes)


# EDA ---------------------------------------------------------------------

datos <- read.csv("./data/censusn.csv", sep = ";")
datos$id <- NULL
head(datos)
datos$mas.5k <- ifelse(datos$salary == " >50K", 1, 0)

# Resumen de la data

df_status(datos)
describe(datos)
freq(datos$education)


# Analisi univariado ------------------------------------------------------

# 1. Variables categoricas

# variable marital.status
table(datos$marital.status)
prop.table(table(datos$marital.status))
status.m <- data.frame(cbind(Freq.sm = table(datos$marital.status),
                             Percent.sm = prop.table(table(datos$marital.status))*100))
status.m.2 <- status.m[order(status.m$Freq.sm, decreasing = T),]
status.m.2 %>% 
  ggplot(mapping = aes(x = reorder(row.names(status.m.2), +Freq.sm), y = Freq.sm, fill = row.names(status.m.2))) +
  geom_bar(stat = "identity") +
  geom_label(mapping = aes(label = Freq.sm), fill = "#CDDDDD") +
  coord_flip() +
  theme_economist() +
  ylab("Frecuencia") +
  xlab("Estado marital") +
  theme(legend.position = "none")

# 2. Variables numericas

# Variable age
ggplot(data = datos, mapping = aes(x = age)) +
  geom_histogram(bins = 80, fill = "blue")

datos %>% 
  filter(mas.5k == 1) %>% 
  ggplot(mapping = aes(x= age)) + geom_histogram(bins = 80)

datos %>% 
  filter(mas.5k == 0) %>% 
  ggplot(mapping = aes(x= age)) + geom_histogram(bins = 80)

# variable final.weight
ggplot(data = datos, mapping = aes(x = final.weight)) +
  geom_histogram(bins = 80)

datos %>% 
  filter(mas.5k == 0) %>% 
  ggplot(mapping = aes(x= final.weight)) + geom_histogram(bins = 80)

datos %>% 
  filter(mas.5k == 1) %>% 
  ggplot(mapping = aes(x= final.weight)) + geom_histogram(bins = 80)

# variable education
unique(datos$education.num)
table(datos$education.num)
prop.table(table(datos$education.num))
educ.num <- data.frame(cbind(Freq.edu = table(datos$education.num),
                       Percent.edu = prop.table(table(datos$education.num))*100))
educ.num.2 <- educ.num[order(educ.num$Freq.edu, decreasing = T),]

educ.num.2 %>% 
  ggplot(mapping = aes(x = Freq.edu, y = rownames(educ.num.2))) +
  geom_bar(stat = "identity", width = .1) +
  coord_flip()

# Analisis Bivariado ------------------------------------------------------

library(corrplot)
library(GGally)
library(psych)
df_status(datos)

# 1. Entre variables numericas

#  variable cuantitativa por pares
vars_num <- df_status(datos, print_results = F) %>% 
  as.data.frame() %>% 
  select(variable, type) %>% 
  filter(type == "integer")

datos_col_num <- datos[ , vars_num$variable]
head(datos_col_num)
corPlot(datos_col_num, scale = F, stars = T, cex = 1)
ggpairs(datos_col_num)

# por categorias
ggpairs(data = datos, 
        columns = c(1, 3, 5, 11,12, 13, 16),
        aes(color = gender, alpha = 0.5))

# 2. Entre variables categoricas
df_status(datos)
table(datos$gender, datos$salary)
datos %>% 
  ggplot(aes(x = gender, fill = salary)) +
  geom_bar(position = "fill") 
  # geom_label(x = gender, position = "fill")

table(datos$gender, datos$salary)
prop.table(table(datos$gender, datos$salary))
datos %>% 
  cross_plot(input = "gender", target = "salary", plot_type = "percentual")

datos %>% 
  cross_plot(input = "race", target = "salary", plot_type = "percentual")


# 3. Una variable continua vs una variable categorica
datos %>% 
  ggplot(mapping = aes(x = race, y = age, fill = race)) +
  geom_boxplot() +
  theme_economist() +
  coord_flip() +
  theme(legend.position = "none")
