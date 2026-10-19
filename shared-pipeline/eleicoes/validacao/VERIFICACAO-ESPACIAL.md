# Verificação espacial independente do post

O script `050_Verificar_Espacial_Independente.R`, chamado por `030`, sorteia
2.000 setores incluídos com semente 20261016, estratificação por UF e ordem
determinística. Lê novamente as malhas originais, refaz pontos internos e
reconstrói locais elegíveis a partir do cadastro original, votos e municípios.
Confere globalmente a presença de cada local incluído nas duas fontes.

Para cada setor amostrado, calcula contra **todos** os locais do município a
distância esférica pela fórmula de haversine em R base, raio 6.371.010 m,
independentemente do índice e das funções de distância S2 da produção.
Exige mínimo geodésico, diferença de distância salva até 0,05 m e as três
primeiras chaves/distâncias na ordem distância/chave, incluindo empates.
Municípios com menos de três locais usam todos os disponíveis.

Também calcula por força bruta a distância euclidiana EPSG:5880 pedida na
auditoria original. Esse critério revelou uma limitação da projeção como
referência métrica para todo o Brasil. Na primeira execução nacional, 16 de
outubro de 2026, 502 dos 2.000 setores não passaram o critério combinado:
489 apenas por diferença relativa de pelo menos 1%; outros 13 também tinham
um mínimo projetado distinto do mínimo esférico. A maior diferença relativa
observada foi 4,9304409511%. Os 2.000 locais salvos eram mínimos geodésicos,
e as distâncias geodésicas reconstruídas coincidiam com as salvas. Nenhuma
das 410.010 chaves de setores incluídos estava ausente dos votos ou dos
90.269 locais elegíveis reconstruídos.

A evidência inicial está preservada no cache ignorado como
`data-raw/eleicoes-renda/verificacao-espacial-primeira-falha.csv`.
Esse resultado não demonstra erro do estimador geodésico: demonstra que a
igualdade do mínimo e a tolerância de 1% em EPSG:5880 não são uma condição
nacional adequada para validá-lo. A substituição da referência de aceitação
pela fórmula esférica independente deve ser registrada e revisada no PR.
Os campos projetados, chaves divergentes, empates e indicador
`projected_diagnostic_pass` permanecem no CSV; não são descartados.

O `pass` final exige o critério esférico independente e a conferência k3.
Isso valida a implementação da associação assumida, sem provar que os
eleitores de um setor votam no local geograficamente mais próximo.
