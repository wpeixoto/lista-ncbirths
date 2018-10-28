# Verificação de propriedades dos resíduos de regressão linear, 
# ver pág. 110 e seguintes

verifica_residuos = function(resids,  # vetor de resíduos
                             obs,     # vetor de valores observados
                             adjs,    # vetor de valores ajustados
                             valor_pequeno=0.0000000001) 
  {
  # P2: soma dos resíduos é zero (se existir intercepto)               ====
  soma = sum(resids)
  # P3: a média dos resíduos é zero (se \beta_0 != 0)                   ====
  media = mean(resids)
  # P4: a soma dos observados é igual à dos ajustados                  ====
  diff_values = sum(obs) - sum(adjs)
  # P5: a média dos observados é igual à dos ajustados                 ====
  diff_medias = mean(obs) - mean(adjs)
  # P7: soma dos resíduos ponderada pelo valor ajustado é zero         ====
  soma_ponderada = sum(resids * adjs)
  
  result = list()
  result$soma.resids = soma
  result$media.resids = media
  result$diff.values.P4 = diff_values
  result$diff.medias.P5 = diff_medias
  result$soma.ponderada = soma_ponderada
  results$is.small = (soma + media + diff_medias + diff_values + soma_ponderada < valor_pequeno)
  
  return(result)
  
}