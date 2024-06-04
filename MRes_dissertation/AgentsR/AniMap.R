library(readr)
library(readxl)


FinalDataMap8 <- read_csv("Desktop/main/MRes_paper/Data/data/FinalDataMap8.csv")
cc_filosofi_2021_COM <- read_delim("Desktop/main/MRes_paper/Data/data/base-cc-filosofi-2021-geo2023_CSV/cc_filosofi_2021_COM.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)
res2022com <- read_excel("Desktop/resultats-par-niveau-subcom-t1-france-entiere.xlsx")


df = FinalDataMap8



#  impact animation 

d1 = df$DomDens1
d2 = df$DomDens2
d3 = df$DomDens3
d4 = df$DomDens4
d5 = df$DomDens5

# State 1
d1.5 = d1/5
d1.4 = d1/4
d1.3 = d1/3
d1.2 = d1/2
d1.1 = d1



# State 2
d2.5 = d2/5
d2.4 = d2/4
d2.3 = d2/3
d2.2 = d2/2
d2.1 = d2

# State 3
d3.5 = d3/5
d3.4 = d3/4
d3.3 = d3/3
d3.2 = d3/2
d3.1 = d3

# State 4
d4.5 = d4/5
d4.4 = d4/4
d4.3 = d4/3
d4.2 = d4/2
d4.1 = d4

# State 5
d5.5 = d5/5
d5.4 = d5/4
d5.3 = d5/3
d5.2 = d5/2
d5.1 = d5

# transition state 1-2

T1.1 = 4/5*d1.1+1/5*d2.5
T1.2 = 3/5*d1.1+2/5*d2.5
T1.3 = 2/5*d1.1+3/5*d2.5
T1.4 = 1/5*d1.1+4/5*d2.5


# transition state 2-3

T2.1 = 4/5*d2.1+1/5*d3.5
T2.2 = 3/5*d2.1+2/5*d3.5
T2.3 = 2/5*d2.1+3/5*d3.5
T2.4 = 1/5*d2.1+4/5*d3.5

# transition state 3-4

T3.1 = 4/5*d3.1+1/5*d4.5
T3.2 = 3/5*d3.1+2/5*d4.5
T3.3 = 2/5*d3.1+3/5*d4.5
T3.4 = 1/5*d3.1+4/5*d4.5

# Transition state 4-5

T4.1 = 4/5*d4.1+1/5*d5.5
T4.2 = 3/5*d4.1+2/5*d5.5
T4.3 = 2/5*d4.1+3/5*d5.5
T4.4 = 1/5*d4.1+4/5*d5.5

#Code 
Code = df$code

AniMap = data.frame(Code,d1.5,d1.4,d1.3,d1.2,d1.1,T1.1,T1.2,T1.3,T1.4,d2.5,d2.4,d2.3,d2.2,d2.1,T2.1,T2.2,T2.3,T2.4,d3.5,d3.4,d3.3,d3.2,d3.1,T3.1,T3.2,T3.3,T3.4,d4.5,d4.4,d4.3,d4.2,d4.1,
                    T4.1,T4.2,T4.3,T4.4,d5.5,d5.4,d5.3,d5.2,d5.1)


write.csv(AniMap, file = "Desktop/Animap.csv")