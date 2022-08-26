library(ggplot2)
library(dplyr)
library(lubridate)

Investigacion <- read.csv("Producción_grupos_investigación.csv")

summary(Investigacion)

Investigacion_1 <- Investigacion %>% 
  rename(Tipologia=NME_TIPOLOGIA_PD, CLASE=NME_CLASE_PD, AÑO=ANO_CONVO)

Tabla_Inv <- Investigacion_1 %>% 
  select(AÑO, CLASE, Tipologia)

summary(Tabla_Inv)

Inv_2013 <- filter(Tabla_Inv, AÑO == '01/01/2013') #Dataset with fixed date
                   
summary(Inv_2013)

#First stage Identifying the Investigation groups of the whole year

ggplot(data = Inv_2013) + geom_bar(mapping = aes(x = Tipologia)) +
  theme(axis.text.x = element_text(angle = 70)) +
  theme(axis.text.x = element_text(hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 22)) +
  labs(title = "PRODUCTOS DE GRUPOS DE INVESTIGACION", 
       subtitle = "Ciencia, Tecnologia e Innovación-- Año 2013", caption = "Datos tomados de Datos.Gov.co") +
  theme(title = element_text(size = 20)) +
  ylab("Recuento")+
  facet_wrap(~CLASE)


# most notorious groups of the year 2013


Focos_Inv_2013 <- Inv_2013 %>% 
  group_by(Tipologia) %>% 
  filter(n()>3000)

View(Focos_Inv_2013)

#Unique values
Inv_Uni_2013 <- table(Inv_2013$Tipologia) %>% 
  sort(Focos_Inv_2013=FALSE)

View(Inv_Uni_2013)

ggplot(data = Focos_Inv_2013) + geom_bar(mapping = aes(x = Tipologia, 
  fill=CLASE)) +
  theme(axis.text.x = element_text(angle = 60)) +
  theme(axis.text.x = element_text(hjust = 1)) +
  theme(axis.text.x = element_text(size = 15)) +
  theme(axis.title.y = element_text(size = 30)) +
labs(title = "FOCOS DE INVESTIGACION (+3000)", 
     subtitle = "Ciencia, Tecnologia e Innovación-- Año 2013", caption = "Datos tomados de Datos.Gov.co") +
  theme(title = element_text(size = 20)) +
  ylab("Recuento")


#Data for year 2014

Inv_2014 <- filter(Tabla_Inv, AÑO == '01/01/2014') #Dataset with fixed date

summary(Inv_2014)

#First stage Identifying the Investigation groups of the whole year


ggplot(data = Inv_2014) + geom_bar(mapping = aes(x = Tipologia)) +
  theme(axis.text.x = element_text(angle = 70)) +
  theme(axis.text.x = element_text(hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 22)) +
  labs(title = "PRODUCTOS DE GRUPOS DE INVESTIGACION", 
       subtitle = "Ciencia, Tecnologia e Innovación-- Año 2014", caption = "Datos tomados de Datos.Gov.co") +
  theme(title = element_text(size = 20)) +
  ylab("Recuento")+
  facet_wrap(~CLASE)


# most notorious groups of the year 2014


Focos_Inv_2014 <- Inv_2014 %>% 
  group_by(Tipologia) %>% 
  filter(n()>3000)

View(Focos_Inv_2014)

#Unique values
Inv_Uni_2014 <- table(Inv_2014$Tipologia) %>% 
  sort(Focos_Inv_2014=FALSE)

View(Inv_Uni_2014)

ggplot(data = Focos_Inv_2014) + geom_bar(mapping = aes(x = Tipologia, 
  fill=CLASE)) +
  theme(axis.text.x = element_text(angle = 60)) +
  theme(axis.text.x = element_text(hjust = 1)) +
  theme(axis.text.x = element_text(size = 15)) +
  theme(axis.title.y = element_text(size = 30)) +
  labs(title = "FOCOS DE INVESTIGACION (+3000)", 
       subtitle = "Ciencia, Tecnologia e Innovación-- Año 2014", caption = "Datos tomados de Datos.Gov.co") +
  theme(title = element_text(size = 20)) +
  ylab("Recuento")


#Data for year 2015

Inv_2015 <- filter(Tabla_Inv, AÑO == '01/01/2015') 

summary(Inv_2015)

#First stage Identifying the Investigation groups of the whole year


ggplot(data = Inv_2015) + geom_bar(mapping = aes(x = Tipologia)) +
  theme(axis.text.x = element_text(angle = 70)) +
  theme(axis.text.x = element_text(hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 22)) +
  labs(title = "PRODUCTOS DE GRUPOS DE INVESTIGACION", 
       subtitle = "Ciencia, Tecnologia e Innovación-- Año 2015", caption = "Datos tomados de Datos.Gov.co") +
  theme(title = element_text(size = 20)) +
  ylab("Recuento")+
  facet_wrap(~CLASE)


# most notorious groups of the year 2015


Focos_Inv_2015 <- Inv_2015 %>% 
  group_by(Tipologia) %>% 
  filter(n()>3000)

View(Focos_Inv_2015)

#Unique values
Inv_Uni_2015 <- table(Inv_2015$Tipologia) %>% 
  sort(Focos_Inv_2015=FALSE)

View(Inv_Uni_2015)

ggplot(data = Focos_Inv_2015) + geom_bar(mapping = aes(x = Tipologia, 
  fill=CLASE)) +
  theme(axis.text.x = element_text(angle = 60)) +
  theme(axis.text.x = element_text(hjust = 1)) +
  theme(axis.text.x = element_text(size = 15)) +
  theme(axis.title.y = element_text(size = 30)) +
  labs(title = "FOCOS DE INVESTIGACION (+3000)", 
       subtitle = "Ciencia, Tecnologia e Innovación-- Año 2015", caption = "Datos tomados de Datos.Gov.co") +
  theme(title = element_text(size = 20)) +
  ylab("Recuento")




#Data for year 2017 as there's no info on 2016 in the dataset

Inv_2017 <- filter(Tabla_Inv, AÑO == '01/01/2017') 

summary(Inv_2017)

#First stage Identifying the Investigation groups of the whole year

ggplot(data = Inv_2017) + geom_bar(mapping = aes(x = Tipologia)) +
  theme(axis.text.x = element_text(angle = 70)) +
  theme(axis.text.x = element_text(hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 22)) +
  labs(title = "PRODUCTOS DE GRUPOS DE INVESTIGACION", 
  subtitle = "Ciencia, Tecnologia e Innovación-- Año 2017", caption = "Datos tomados de Datos.Gov.co") +
  theme(title = element_text(size = 20)) +
  ylab("Recuento")+
  facet_wrap(~CLASE)


# most notorious groups of the year 2017

Focos_Inv_2017 <- Inv_2017 %>% 
  group_by(Tipologia) %>% 
  filter(n()>3000)

View(Focos_Inv_2017)

#Unique values
Inv_Uni_2017 <- table(Inv_2017$Tipologia) %>% 
  sort(Focos_Inv_2017=FALSE)

View(Inv_Uni_2017)

ggplot(data = Focos_Inv_2017) + geom_bar(mapping = aes(x = Tipologia, 
  fill=CLASE)) +
  theme(axis.text.x = element_text(angle = 60)) +
  theme(axis.text.x = element_text(hjust = 1)) +
  theme(axis.text.x = element_text(size = 15)) +
  theme(axis.title.y = element_text(size = 30)) +
  labs(title = "FOCOS DE INVESTIGACION (+3000)", 
  subtitle = "Ciencia, Tecnologia e Innovación-- Año 2017", caption = "Datos tomados de Datos.Gov.co") +
  theme(title = element_text(size = 20)) +
  ylab("Recuento")

#Data for year 2019

Inv_2019 <- filter(Tabla_Inv, AÑO >= '01/01/2019') 

summary(Inv_2019)

#First stage Identifying the Investigation groups of the whole year

ggplot(data = Inv_2019) + geom_bar(mapping = aes(x = Tipologia)) +
  theme(axis.text.x = element_text(angle = 70)) +
  theme(axis.text.x = element_text(hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 22)) +
  labs(title = "PRODUCTOS DE GRUPOS DE INVESTIGACION", 
       subtitle = "Ciencia, Tecnologia e Innovación-- Año 2019", caption = "Datos tomados de Datos.Gov.co") +
  theme(title = element_text(size = 20)) +
  ylab("Recuento")+
  facet_wrap(~CLASE)

# most notorious groups of the year 2019

Focos_Inv_2019 <- Inv_2019 %>% 
  group_by(Tipologia) %>% 
  filter(n()>3000)

View(Focos_Inv_2019)

#Unique values
Inv_Uni_2019 <- table(Inv_2019$Tipologia) %>% 
  sort(Focos_Inv_2019=FALSE) %>% 
  
View(Inv_Uni_2019)

ggplot(data = Focos_Inv_2019) + geom_bar(mapping = aes(x = Tipologia, 
  fill=CLASE)) +
  theme(axis.text.x = element_text(angle = 60)) +
  theme(axis.text.x = element_text(hjust = 1)) +
  theme(axis.text.x = element_text(size = 15)) +
  theme(axis.title.y = element_text(size = 30)) +
  labs(title = "FOCOS DE INVESTIGACION (+3000)", 
       subtitle = "Ciencia, Tecnologia e Innovación-- Año 2019", caption = "Datos tomados de Datos.Gov.co") +
  theme(title = element_text(size = 20)) +
  ylab("Recuento")

#R&D seems to be the most critical of the four Class so let's check the Class

ggplot(data = Tabla_Inv ) + geom_bar(mapping = aes(x = CLASE, fill = CLASE)) +
  theme(axis.text.x = element_text(angle = 60)) +
  theme(axis.text.x = element_text(hjust = 1)) +
  theme(axis.text.x = element_text(size = 15)) +
  theme(axis.title.y = element_text(size = 25)) +
  labs(title = "CLASES DE INVESTIGACION Y DESARROLLO", 
  subtitle = "Ciencia, Tecnologia e Innovación-- Años 2013, 2014, 2015, 
  2017 y 2019", caption = "Dataset tomado de Datos.Gov.co") +
  theme(title = element_text(size = 20)) +
  scale_y_continuous(name="Recuento", limits=c(0, 1000000))

#Now we'll explore the R&D further

Inv_DT <- filter(Tabla_Inv, CLASE == 'Desarrollo tecnológico e innovación') 

summary(Inv_DT)

ggplot(data = Inv_DT ) + geom_bar(mapping = aes(x = AÑO, fill = CLASE)) +
  theme(axis.text.x = element_text(size = 15)) +
  theme(axis.title.y = element_text(size = 30)) +
  labs(title = "Desarrollo tecnológico e innovación", 
       subtitle = "Ciencia, Tecnologia e Innovación-- Años 2013, 2014, 2015, 
  2017 y 2019", caption = "Dataset tomado de Datos.Gov.co") +
  ylab("Recuento") +
  theme(title = element_text(size = 20))+
  scale_x_discrete("AÑO",labels = c(
    "01/01/2013" = "2013",
    "01/01/2014" = "2014",
    "01/01/2015" = "2015",
    "01/01/2017" = "2017",
    "06/12/2019" = "2019"))#Fixing data shown on graph

#Now a more specific outlook of the Tipology of that Class

Focos_RD <- Inv_DT %>% 
  group_by(Tipologia) %>% 
  filter(n()>2000)

ggplot(data = Focos_RD ) + geom_bar(mapping = aes(x = AÑO, fill = Tipologia)) +
  theme(axis.text.x = element_text(size = 15)) +
  theme(axis.title.y = element_text(size = 30)) +
  labs(title = "Desarrollo tecnológico e innovación", 
       subtitle = "Tipologias-- Años 2013, 2014, 2015, 
  2017 y 2019", caption = "Dataset tomado de Datos.Gov.co") +
  ylab("Recuento") +
  theme(title = element_text(size = 20))+
  scale_x_discrete("AÑO",labels = c(
      "01/01/2013" = "2013",
      "01/01/2014" = "2014",
      "01/01/2015" = "2015",
      "01/01/2017" = "2017",
      "06/12/2019" = "2019"))#Fixing data shown on graph

#Thanks for reading, any suggestions are welcome
#CHeck .Rmd for more details
