# Cerundolo vs Diallo - SF ATP 250 Almaty

cer_vs_dia = as.data.frame(cbind(0.67,0.68,0.51,0.44,0.43,0.35))
colnames(cer_vs_dia) = c("pct_fran_1stWon","pct_opp_1stWon","pct_fran_2ndWon","pct_opp_2ndWon", "pct_fran_bp_wins","pct_opp_bp_wins")
cer_vs_dia <- cer_vs_dia %>%
  mutate(cat2= cut(pct_opp_1stWon, breaks = c(0, 0.6, 0.65, 0.7, 0.75, Inf)))

pred_cer_vs_dia <- predict(modelo_final, newdata = cer_vs_dia, type = "response")                           
pred_cer_vs_dia

# En base a las estadisticas promedio del ultimo año, la probabilidad estimada de que Cerundolo le gane 
# a Diallo es de 0.877, y con un punto de corte de 0.5 el modelo predice que Fran gana


# Cerundolo vs Tsisipas - 1ra Ronda ATP 500 Basilea

# cerundolo
# 33/45, 42/64, 29/36, 24/26, 20/26 -- 0.7512
# 8/21, 13/24, 17/26, 11/24, 19/35 -- 0.5230
# 3/6, 2/5, 2/4, 3/6, 0/1 -- 0.4545

# tsisipas
# 38/45, 25/33, 37/52, 53/66, 27/42 -- 0.7563
# 17/30, 15/21, 13/31, 21/38, 18/30 -- 0.56
# 1/6, 4/7, 1/1, 1/2, 0/0 -- 0.4375

cer_vs_tsi = as.data.frame(cbind(0.7512,0.7563,0.5230,0.56,0.4545,0.4375))
colnames(cer_vs_tsi) = c("pct_fran_1stWon","pct_opp_1stWon","pct_fran_2ndWon","pct_opp_2ndWon", "pct_fran_bp_wins","pct_opp_bp_wins")
cer_vs_tsi <- cer_vs_tsi %>%
  mutate(cat2= cut(pct_opp_1stWon, breaks = c(0, 0.6, 0.65, 0.7, 0.75, Inf)))

pred_cer_vs_tsi <- predict(modelo_final, newdata = cer_vs_tsi, type = "response")                           
pred_cer_vs_tsi

# En base a las estadisticas promedio de los ultimos 5 partidos, la probabilidad estimada de que 
# Cerundolo le gane a Tsisipas Diallo es de 0.005, y con un punto de corte de 0.5 el modelo predice 
# que Fran pierde

cer_vs_tsi2 = as.data.frame(cbind(44/71,57/74,25/40,21/46,2/7,3/12))
colnames(cer_vs_tsi2) = c("pct_fran_1stWon","pct_opp_1stWon","pct_fran_2ndWon","pct_opp_2ndWon", "pct_fran_bp_wins","pct_opp_bp_wins")
cer_vs_tsi2 <- cer_vs_tsi2 %>%
  mutate(cat2= cut(pct_opp_1stWon, breaks = c(0, 0.6, 0.65, 0.7, 0.75, Inf)))

pred_cer_vs_tsi2 <- predict(modelo_final, newdata = cer_vs_tsi2, type = "response")                           
pred_cer_vs_tsi2
