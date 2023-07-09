#File to create dataset
#CREAZIONE DATASET
setwd("D:/SCUOLA/Milano Data Science/PRIMO ANNO/STATISTICAL LEARNING AND MACHINE LEARNING/Statistical learning/Datasets")
dati_Fertility<- read.csv("/SCUOLA/Milano Data Science/PRIMO ANNO/STATISTICAL LEARNING AND MACHINE LEARNING/Statistical learning/Variabili/Fertility.csv", sep=";")
dati_LifeExpectancy <- read.csv("/SCUOLA/Milano Data Science/PRIMO ANNO/STATISTICAL LEARNING AND MACHINE LEARNING/Statistical learning/Variabili/Life expectancy.csv")
dataset <- left_join(dati_Fertility, dati_LifeExpectancy, by = "ISO.code")
dataset <- dataset[,-c(4)]
unique(dataset$Country.x)
dataset <- dataset %>% rename(Country = Country.x)
dataset <- dataset[!duplicated(dataset$Country), ]

dati_religion <- read.csv("/SCUOLA/Milano Data Science/PRIMO ANNO/STATISTICAL LEARNING AND MACHINE LEARNING/Statistical learning/Variabili/Religion.csv", sep = ";")
dataset2 <- left_join(dataset, dati_religion, by = "Country")

dati_unemployment <- read.csv("/SCUOLA/Milano Data Science/PRIMO ANNO/STATISTICAL LEARNING AND MACHINE LEARNING/Statistical learning/Variabili/Unemployment rate 2.csv", sep = ";")
dati_unemployment <- dati_unemployment[,-c(1)]
dataset3 <- left_join(dataset2, dati_unemployment, by = "Country")
dataset3 <- dataset3[!duplicated(dataset3$Country), ]

dati_alcohol <- read.csv("/SCUOLA/Milano Data Science/PRIMO ANNO/STATISTICAL LEARNING AND MACHINE LEARNING/Statistical learning/Variabili/alcoholSubstanceAbuse.csv")
selected_columns <- subset(dati_alcohol, Dim1 == "Female")
selected_columns <- subset(selected_columns, Period == "2018")
colnames(selected_columns)
selected_columns <- selected_columns %>% 
  rename(
    Country = ï..Location)
selected_columns <- selected_columns[, -c(2:4)]
dataset4 <- left_join(dataset3, selected_columns, by = "Country")
dataset4  <- dataset4 %>% 
  rename(
    Alcohol = First.Tooltip)

dati_school <- read.csv("/SCUOLA/Milano Data Science/PRIMO ANNO/STATISTICAL LEARNING AND MACHINE LEARNING/Statistical learning/Variabili/mean-years-of-schooling-long-run.csv")
selected_columns2 <- subset(dati_school, Year == "2017")
selected_columns2 <- selected_columns2 %>% 
  rename(
    ISO.code = Code)
dataset5 <- left_join(dataset4, selected_columns2, by = "ISO.code")
dataset5 <- dataset5[, -c(8:9)]
colnames(dataset5)
dataset5 <- dataset5 %>% 
  rename(
    year_of_school = Average.Total.Years.of.Schooling.for.Adult.Population..Lee.Lee..2016...Barro.Lee..2018..and.UNDP..2018..)

dati_contrapceptive <-read.csv("/SCUOLA/Milano Data Science/PRIMO ANNO/STATISTICAL LEARNING AND MACHINE LEARNING/Statistical learning/Variabili/Use contrapceptive.csv", sep=";")
dataset6 <- left_join(dataset5, dati_contrapceptive, by = "Country")
dataset6 <- dataset6[!duplicated(dataset6$Country), ]



dati_GNI_maternity <-read.csv("/SCUOLA/Milano Data Science/PRIMO ANNO/STATISTICAL LEARNING AND MACHINE LEARNING/Statistical learning/Variabili/GNI e maternity.csv", sep=";")
dataset7 <- left_join(dataset6, dati_GNI_maternity, by = "Country")
dataset7 <- dataset7[!duplicated(dataset7$Country), ]

dati_Freedom <-read.csv("/SCUOLA/Milano Data Science/PRIMO ANNO/STATISTICAL LEARNING AND MACHINE LEARNING/Statistical learning/Variabili/Freedom.csv", sep=";")
dataset8 <- left_join(dataset7, dati_Freedom, by = "Country")
dataset8 <- dataset8[!duplicated(dataset8$Country), ]

dati_Migration <-read.csv("/SCUOLA/Milano Data Science/PRIMO ANNO/STATISTICAL LEARNING AND MACHINE LEARNING/Statistical learning/Variabili/Migration rates.csv", sep=";")
dataset9 <- left_join(dataset8, dati_Migration, by = "Country")
dataset9 <- dataset9[!duplicated(dataset8$Country), ]
dataset9 <- dataset9[, -c(3, 14,15)]
dataset9$Migration.rate<- as.numeric(dataset9$Migration.rate)

rowSums(!is.na(dataset9))
rows_with_na <- which(apply(is.na(dataset9), 1, any))
nrow(dataset9[rows_with_na, ])    
#75 righe contengono missing values
df_with_nulls <- dataset9[!complete.cases(dataset9), ]
df_with_alot_Na<- df_with_nulls[rowSums(is.na(df_with_nulls))>=2,] #20 righe
#droppiamo tutte le righe con più di 2 valori nulli, le consideriamo incomplete e non 
#abbiamo abb informazioni
df_with_lessNA <-anti_join(dataset9, df_with_alot_Na, by = "Country")
#ci rimangono 185 righe
#vediamo qunti null values per ogni riga
sum(is.na(df_with_lessNA$Fertility)) #0
sum(is.na(df_with_lessNA$Life.expectancy)) #5
sum(is.na(df_with_lessNA$Religion)) #0
df_with_lessNA[]
sum(is.na(df_with_lessNA$Unemployment.female)) #2
sum(is.na(df_with_lessNA$Alcohol)) #2
sum(is.na(df_with_lessNA$year_of_school)) #0
sum(is.na(df_with_lessNA$Use.of.contrapcetive)) #0
sum(is.na(df_with_lessNA$Mandatory.paid.maternity.leave..days.)) #5
sum(is.na(df_with_lessNA$Gross.national.income..GNI..per.capita)) #0
sum(is.na(df_with_lessNA$Freedom.hous.rates)) #7
sum(is.na(df_with_lessNA$Migration.rate)) #2

china_row_index <- which(df_with_lessNA$Country == "China")
# Replace the NA value in the 'religion' column of the 'China' row with a new value
df_with_lessNA[china_row_index, "Religion"] <- "Buddhism"



# Replace NA values with column means
df_num_lessNA <- subset(df_with_lessNA, select = -c(Country,Religion))
df_nonnum <- subset(df_with_lessNA, select = c(Country,Religion))
df<- na.aggregate(df_num_lessNA, FUN = mean, na.rm = TRUE)
df<- cbind(df_nonnum, df)
sum(is.na(df))
df<- df %>% rename( Unemployment = Unemployment.female,Years.school =year_of_school, 
                    Contraceptive = Use.of.contrapcetive, Maternity.days=Mandatory.paid.maternity.leave..days.,
                    GNI=Gross.national.income..GNI..per.capita, Freedom = Freedom.hous.rates)
summary(df)
write.csv(df, file = "/SCUOLA/Milano Data Science/PRIMO ANNO/STATISTICAL LEARNING AND MACHINE LEARNING/Statistical learning/Variabili/df_csv.csv", row.names = FALSE)
