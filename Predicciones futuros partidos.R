# Cerundolo vs Diallo - SF ATP 250 Almaty

cer_vs_dia = as.data.frame(cbind(0.67,0.68,0.51,0.44,0.43,0.35))
colnames(cer_vs_dia) = c("pct_fran_1stWon","pct_opp_1stWon","pct_fran_2ndWon","pct_opp_2ndWon", "pct_fran_bp_wins","pct_opp_bp_wins")
cer_vs_dia <- cer_vs_dia %>%
  mutate(cat2= cut(pct_opp_1stWon, breaks = c(0, 0.6, 0.65, 0.7, 0.75, Inf)))

pred_cer_vs_dia <- predict(modelo_final, newdata = cer_vs_dia, type = "response")                           
pred_cer_vs_dia

# En base a las estadisticas promedio del ultimo año, la probabilidad estimada de que Cerundolo le gane 
# a Diallo es de 0.877, y con un punto de corte de 0.5 el modelo predice que Fran gana
