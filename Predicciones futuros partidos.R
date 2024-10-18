# Cerundolo vs Diallo - SF ATP 250 Almaty

cer_vs_dia = as.data.frame(cbind(0.67,0.69,0.5,0.44,0.41,0.37))
colnames(cer_vs_dia) = c("pct_fran_1stWon","pct_opp_1stWon","pct_fran_2ndWon","pct_opp_2ndWon", "pct_fran_bp_wins","pct_opp_bp_wins")
cer_vs_dia <- cer_vs_dia %>%
  mutate(cat2= cut(pct_opp_1stWon, breaks = c(0, 0.6, 0.65, 0.7, 0.75, Inf)))

pred_cer_vs_dia <- predict(modelo_final, newdata = cer_vs_dia, type = "response")                           
pred_cer_vs_dia

# La probabilidad estimada de que Cerundolo le gane a Diallo es de 0.809, y con un punto de corte de 0.5
# el modelo predice que Fran gana
