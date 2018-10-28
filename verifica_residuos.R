# Verificação de propriedades dos resíduos, ver pág. 110 e seguintes

verifica_residuos = function(resids, obs, adjs) {
  # P2                                                                 ====
  soma = sum(resids)
  # P3                                                                 ====
  media = mean(resids)
  # P4                                                                 ====
  diff_values = sum(obs) - sum(adjs)
  # P5                                                                 ====
  diff_medias = mean(obs) - mean(adjs)
  # P7                                                                 ====
  soma_ponderada = sum(resids * adjs)
  
  result = list()
  result$soma.resids = soma
  result$media.resids = media
  result$diff.values.P4 = diff_values
  result$diff.medias.P5 = diff_medias
  result$soma.ponderada = soma_ponderada
  results$is.small = (soma + media + diff_medias + diff_values + soma_ponderada < 0.0000000001)
  
  return(result)
  
}