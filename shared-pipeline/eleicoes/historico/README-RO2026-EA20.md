# Recuperação oficial dos deputados de Rondônia em 2026

O arquivo TSE `votacao_candidato_munzona_2026.zip` omite os cargos 6 e 7 de Rondônia tanto no membro UF quanto no membro BRASIL. `ro2026-ea20.R` recupera exclusivamente essas duas disputas usando o arquivo oficial EA20 por UF/cargo. Não modifica o reconciliador geral nem infere validade a partir de elegibilidade, APTO ou nomes.

```sh
Rscript --vanilla shared-pipeline/eleicoes/historico/ro2026-ea20.R
Rscript --vanilla shared-pipeline/eleicoes/historico/test-ro2026-ea20.R
```

A função `ro2026_ea20(install_canonical=TRUE)` também instala o cache combinado no nome canônico `places-2026-RO-3_5_6_7_8-legal.rds`, depois de comprovar os checks anteriores de governador/senador. O número 8 nesse nome descreve o pedido original de cargos; não cria deputado distrital em RO. A instalação deve ocorrer depois de o batch estadual ter passado por RO, para evitar escrita concorrente.

A rotina exige:

1. Ambiente oficial (`f=o`), eleição 6259, primeiro turno, abrangência UF RO e cargo correto nos JSONs.
2. Correspondência exata dos votos de cada candidato na UF entre seção e `vap` do EA20. Nenhum candidato com votos positivos pode estar ausente.
3. Correspondência exata do `SQ_CANDIDATO` entre seção e `sqcand`, sem sequenciais conflitantes para um número.
4. Aplicação somente da destinação oficial `dvt`: válido nominal, válido em legenda, anulado ou anulado sub judice. Destino desconhecido causa erro.
5. Reconciliação exata de nominal válido, legenda válida, total válido e nulo técnico em **cada município/zona**, contra o CSV oficial `detalhe_votacao_munzona`. O número ausente do EA20 só é excluído depois de esse resíduo fechar exatamente como nulo técnico.

O resultado comprovado nos snapshots preservados foi de 125 candidaturas federais e 248 estaduais, sem divergência de votos ou sequencial. As 56 zonas de cada cargo fecharam em todas as quatro medidas. Nominais válidos: 904.018 federais e 896.601 estaduais; totais válidos: 932.939 e 942.021. Os nulos técnicos são 243 e 58, respectivamente.

Os JSONs são snapshots **provisórios** de 13/10/2026, 16:26:47 e 16:26:48, com `tf=n` e `and=p`. A auditoria conserva URL, MD5, geração, IDG, andamento e condição provisória. O fato de os totais coincidirem com o detalhe de 14/10/2026 não significa totalização jurídica final. O cache conserva `vote_audit$provisional=TRUE` e `vote_audit$snapshots`.

Saídas próprias: `places-2026-RO-6_7-legal.rds` e cache combinado `places-2026-RO-3_5_6_7-legal.rds`. As evidências e os JSONs permanecem em `data-raw/eleicoes-renda/historico/ro-alternative/`. A combinação mantém os checks certificados de governador/senador e remove de `unavailable` somente as duas disputas recuperadas, preservando eventuais outras indisponibilidades.

Fontes primárias: [EA20 federal de RO](https://resultados.tse.jus.br/oficial/ele2026/6259/dados/ro/ro-c0006-e006259-u.json), [EA20 estadual de RO](https://resultados.tse.jus.br/oficial/ele2026/6259/dados/ro/ro-c0007-e006259-u.json), [especificação EA20](https://www.tse.jus.br/eleicoes/eleicoes-2026-content/arquivos/divulgacao-de-resultados/tse-ea20-arquivo-de-resultado-unificado), [orientações e códigos oficiais](https://www.tse.jus.br/eleicoes/informacoes-tecnicas-sobre-a-divulgacao-de-resultados).
