#Contamos con una base de datos de todos los partidos del circuito atp los ultimos 50 años, nos vamos
#a centrar en Francisco Cerúndolo

atp_matches_2024 = read.csv("C:/Users/Usuario/Desktop/Estadistica/Analisis-Francisco-Cerundolo/atp_matches_2024.csv")
atp_matches_2023 = read.csv("C:/Users/Usuario/Desktop/Estadistica/Analisis-Francisco-Cerundolo/atp_matches_2023.csv")
atp_matches_2022 = read.csv("C:/Users/Usuario/Desktop/Estadistica/Analisis-Francisco-Cerundolo/atp_matches_2022.csv")
atp_matches_2021 = read.csv("C:/Users/Usuario/Desktop/Estadistica/Analisis-Francisco-Cerundolo/atp_matches_2021.csv")
atp_matches_2020 = read.csv("C:/Users/Usuario/Desktop/Estadistica/Analisis-Francisco-Cerundolo/atp_matches_2020.csv")
atp_matches_2019 = read.csv("C:/Users/Usuario/Desktop/Estadistica/Analisis-Francisco-Cerundolo/atp_matches_2019.csv")



#Filtramos la base con sus partidos en cada uno de los años desde su debut
cerundolo_w = subset(atp_matches_2024, winner_name == "Francisco Cerundolo")
cerundolo_l = subset(atp_matches_2024, loser_name == "Francisco Cerundolo")
cerundolo = rbind(cerundolo_w,cerundolo_l)


cerundolo_w = subset(atp_matches_2023, winner_name == "Francisco Cerundolo")
cerundolo_l = subset(atp_matches_2023, loser_name == "Francisco Cerundolo")
cerundolo2 = rbind(cerundolo_w,cerundolo_l)

cerundolo_w = subset(atp_matches_2022, winner_name == "Francisco Cerundolo")
cerundolo_l = subset(atp_matches_2022, loser_name == "Francisco Cerundolo")
cerundolo3 = rbind(cerundolo_w,cerundolo_l)

cerundolo_w = subset(atp_matches_2021, winner_name == "Francisco Cerundolo")
cerundolo_l = subset(atp_matches_2021, loser_name == "Francisco Cerundolo")
cerundolo4 = rbind(cerundolo_w,cerundolo_l)

cerundolo_w = subset(atp_matches_2020, winner_name == "Francisco Cerundolo")
cerundolo_l = subset(atp_matches_2020, loser_name == "Francisco Cerundolo")
cerundolo5 = rbind(cerundolo_w,cerundolo_l)


cerundolo_w = subset(atp_matches_2019, winner_name == "Francisco Cerundolo")
cerundolo_l = subset(atp_matches_2019, loser_name == "Francisco Cerundolo")
cerundolo6 = rbind(cerundolo_w,cerundolo_l)

cerundolo_final = rbind(cerundolo,cerundolo2,cerundolo3,cerundolo4,cerundolo5,cerundolo6)

#Filtramos las variables irrelevantes
#Corregimos los datos faltantes, tanto de rankings,alturas y variables relacionadas al partido
#a mano

library(readxl)
cerundolo_final1 <- read_excel("C:/Users/Usuario/Desktop/Estadistica/Analisis-Francisco-Cerundolo/cerundolo_final1.xlsx")

df = subset(cerundolo_final, tourney_date > "20240526")
cerundolo_final1 = rbind(cerundolo_final1,df)

cerundolo_final1 = subset(cerundolo_final1, select = -c(tourney_id,tourney_name,draw_size,tourney_date,match_num,winner_id,winner_seed,winner_entry,winner_ioc,loser_id,loser_seed,loser_entry,loser_ioc,score,winner_rank_points,loser_rank_points,w_SvGms,l_SvGms))
is.na(cerundolo_final1)


cerundolo_final1$fran_win = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",1,0)
#Creamos variables de interes en base a variables con las que contabamos
cerundolo_final1$opp_hand = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",cerundolo_final1$loser_hand,cerundolo_final1$winner_hand)
cerundolo_final1$opp_ht = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",cerundolo_final1$loser_ht,cerundolo_final1$winner_ht)
cerundolo_final1$opp_age = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",cerundolo_final1$loser_age,cerundolo_final1$winner_age)

# Instalar el paquete si no lo tienes
install.packages("DataExplorer")

# Cargar el paquete
library(DataExplorer)
library(kableExtra)
library(tidyverse)


#porcentaje de victorias

sum(cerundolo_final1$fran_win)/189 #gano el 52.3% de los partidos que jugo

cerundolo_final1$fran_win = as.factor(cerundolo_final1$fran_win)

#por superficie
table(cerundolo_final1$surface)


#Calculo opp ranking

cerundolo_final1$opp_rank = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",cerundolo_final1$loser_rank,cerundolo_final1$winner_rank)


w=cerundolo_final1$opp_rank[cerundolo_final1$fran_win == "1"]
median(w, na.rm=TRUE)
l=cerundolo_final1$opp_rank[cerundolo_final1$fran_win == "0"]
median(l, na.rm=TRUE)

#ranking trasnformado

cerundolo_final1$fran_rank = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",cerundolo_final1$winner_rank,cerundolo_final1$loser_rank)

cerundolo_final1$fran_rank_trans = 8 - log2(cerundolo_final1$fran_rank)
cerundolo_final1$opp_rank_trans = 8 - log2(cerundolo_final1$opp_rank)

cerundolo_final1$dif_rank = cerundolo_final1$fran_rank_trans - cerundolo_final1$opp_rank_trans


mean(cerundolo_final1$dif_rank, na.rm=TRUE)

#Creamos fran_age y opp_age

cerundolo_final1$fran_age = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",cerundolo_final1$winner_age,cerundolo_final1$loser_age)
cerundolo_final1$opp_age = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",cerundolo_final1$loser_age,cerundolo_final1$winner_age)

#Planteamos las estadísticas del partido para fran y los oponentes

cerundolo_final1$fran_ace = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",cerundolo_final1$w_ace,cerundolo_final1$l_ace)
cerundolo_final1$opp_ace = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",cerundolo_final1$l_ace,cerundolo_final1$w_ace)
cerundolo_final1$fran_df = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",cerundolo_final1$w_df,cerundolo_final1$l_df)
cerundolo_final1$opp_df = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",cerundolo_final1$l_df,cerundolo_final1$w_df)
cerundolo_final1$fran_svpt = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",cerundolo_final1$w_svpt,cerundolo_final1$l_svpt)
cerundolo_final1$opp_svpt = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",cerundolo_final1$l_svpt,cerundolo_final1$w_svpt)
cerundolo_final1$fran_1stIn = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",cerundolo_final1$w_1stIn,cerundolo_final1$l_1stIn)
cerundolo_final1$opp_1stIn = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",cerundolo_final1$l_1stIn,cerundolo_final1$w_1stIn)
cerundolo_final1$fran_1stWon = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",cerundolo_final1$w_1stWon,cerundolo_final1$l_1stWon)
cerundolo_final1$opp_1stWon = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",cerundolo_final1$l_1stWon,cerundolo_final1$w_1stWon)
cerundolo_final1$fran_2ndWon = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",cerundolo_final1$w_2ndWon,cerundolo_final1$l_2ndWon)
cerundolo_final1$opp_2ndWon = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",cerundolo_final1$l_2ndWon,cerundolo_final1$w_2ndWon)
cerundolo_final1$fran_bpSaved = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",cerundolo_final1$w_bpSaved,cerundolo_final1$l_bpSaved)
cerundolo_final1$opp_bpSaved = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",cerundolo_final1$l_bpSaved,cerundolo_final1$w_bpSaved)
cerundolo_final1$fran_bpFaced = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",cerundolo_final1$w_bpFaced,cerundolo_final1$l_bpFaced)
cerundolo_final1$opp_bpFaced = ifelse(cerundolo_final1$winner_name == "Francisco Cerundolo",cerundolo_final1$l_bpFaced,cerundolo_final1$w_bpFaced)

#borramos las variables de winner y loser

cerundolo_final1 = subset(cerundolo_final1, select = -c(winner_hand,winner_ht,winner_age,winner_rank,loser_rank,loser_hand,loser_ht,loser_age,w_ace,w_df,w_svpt,w_1stIn,w_1stWon,w_2ndWon,w_bpSaved,w_bpFaced,l_ace,l_df,l_svpt,l_1stIn,l_1stWon,l_2ndWon,l_bpSaved,l_bpFaced,fran_rank,opp_rank))

#Realizamos la transformacion de las variables: cant de servicios, cantidad de primeros servicios
#en juego, cant de juegos de primer servicio ganados, cant de juegos de segundo servicio ganados,
#cant de break points y cant de aces.

#Porcentaje de primeros servicios dentro
cerundolo_final1$pct_fran_1stIn= (cerundolo_final1$fran_1stIn/cerundolo_final1$fran_svpt)
cerundolo_final1$pct_opp_1stIn= (cerundolo_final1$opp_1stIn/cerundolo_final1$opp_svpt)
#Porcentaje de primeros servicios dentro ganados
cerundolo_final1$pct_fran_1stInWon= (cerundolo_final1$fran_1stWon/cerundolo_final1$fran_1stIn)
cerundolo_final1$pct_opp_1stInWon= (cerundolo_final1$opp_1stWon/cerundolo_final1$opp_1stIn)
#Porcentaje de primeros servicios ganados
cerundolo_final1$pct_fran_1stWon= cerundolo_final1$pct_fran_1stInWon*cerundolo_final1$pct_fran_1stIn
cerundolo_final1$pct_opp_1stWon= cerundolo_final1$pct_opp_1stInWon*cerundolo_final1$pct_opp_1stIn

#Porcentaje de segundos servicios dentro
cerundolo_final1$pct_fran_2ndIn= ((cerundolo_final1$fran_svpt-cerundolo_final1$fran_1stIn-cerundolo_final1$fran_df)/(cerundolo_final1$fran_svpt-cerundolo_final1$fran_1stIn))
cerundolo_final1$pct_opp_2ndIn= ((cerundolo_final1$opp_svpt-cerundolo_final1$opp_1stIn-cerundolo_final1$opp_df)/(cerundolo_final1$opp_svpt-cerundolo_final1$opp_1stIn))
#Porcentaje de segundos servicios dentro ganados
cerundolo_final1$pct_fran_2ndInWon= (cerundolo_final1$fran_2ndWon/(cerundolo_final1$fran_svpt-cerundolo_final1$fran_1stIn-cerundolo_final1$fran_df))
cerundolo_final1$pct_opp_2ndInWon= (cerundolo_final1$opp_2ndWon/(cerundolo_final1$opp_svpt-cerundolo_final1$opp_1stIn-cerundolo_final1$opp_df))
#Porcentaje de segundos servicios ganados
cerundolo_final1$pct_fran_2ndWon= (cerundolo_final1$fran_2ndWon/(cerundolo_final1$fran_svpt-cerundolo_final1$fran_1stIn))
cerundolo_final1$pct_opp_2ndWon= (cerundolo_final1$opp_2ndWon/(cerundolo_final1$opp_svpt-cerundolo_final1$opp_1stIn))


#Ver si trabajamos con los aces o dobles faltas

#dobles faltas
cerundolo_final1$pct_fran_df= (cerundolo_final1$fran_df/cerundolo_final1$fran_svpt)
cerundolo_final1$pct_opp_df= (cerundolo_final1$opp_df/cerundolo_final1$opp_svpt)
#aces
cerundolo_final1$pct_fran_ace= (cerundolo_final1$fran_ace/cerundolo_final1$fran_svpt)
cerundolo_final1$pct_opp_ace= (cerundolo_final1$opp_ace/cerundolo_final1$opp_svpt)

#break points convertidos

cerundolo_final1$pct_fran_bp_wins= ((cerundolo_final1$opp_bpFaced-cerundolo_final1$opp_bpSaved)/cerundolo_final1$opp_bpFaced)
cerundolo_final1$pct_opp_bp_wins= ((cerundolo_final1$fran_bpFaced-cerundolo_final1$fran_bpSaved)/cerundolo_final1$fran_bpFaced)

#break points salvados
cerundolo_final1$pct_fran_bp_saved = 1 - cerundolo_final1$pct_opp_bp_wins
cerundolo_final1$pct_opp_bp_saved = 1 - cerundolo_final1$pct_fran_bp_wins

#imputamos los valores de la mano habil de los q faltaban

cerundolo_final1$opp_hand[cerundolo_final1$loser_name == "Aleksandar Kovacevic"] <- "R"
cerundolo_final1$opp_hand[cerundolo_final1$loser_name == "Vilius Gaubas"] <- "R"
cerundolo_final1$opp_hand[cerundolo_final1$loser_name == "Manuel Guinard"] <- "R"
cerundolo_final1$opp_hand[cerundolo_final1$loser_name == "Justin Engel"] <- "R"
cerundolo_final1$opp_hand[cerundolo_final1$winner_name == "Ryan Peniston"] <- "L"

cerundolo_final1$opp_ht[cerundolo_final1$loser_name == "Justin Engel"] <- 185

cerundolo_final1$minutes[cerundolo_final1$loser_name == "Yannick Hanfmann" & cerundolo_final1$round == "R128"] <- 118
cerundolo_final1$minutes[cerundolo_final1$loser_name == "Tommy Paul" & cerundolo_final1$tourney_level == "G"] <- 170
cerundolo_final1$minutes[cerundolo_final1$loser_name == "Filip Misolic" & cerundolo_final1$tourney_level == "G"] <- 124
cerundolo_final1$minutes[cerundolo_final1$loser_name == "Tomas Barrios Vera"] <- 80
cerundolo_final1$minutes[cerundolo_final1$loser_name == "Ugo Humbert"] <- 165
cerundolo_final1$minutes[cerundolo_final1$winner_name == "Novak Djokovic"] <- 279
cerundolo_final1$minutes[cerundolo_final1$loser_name == "Casper Ruud" & cerundolo_final1$tourney_level == "O"] <- 86

sum(is.na(cerundolo_final1))
