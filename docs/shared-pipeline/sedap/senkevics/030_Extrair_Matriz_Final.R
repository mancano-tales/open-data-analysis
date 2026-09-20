# ==============================================================================
# ODA: portado de 4-DA-Code/2026-08_Replicacao_Senkevics2024/ em 2026-09-20; saidas em
#      data-raw/sedap/senkevics/coorte_<ano>/. Exige SEDAP_TOKEN (Tier B).
# 030_Extrair_Matriz_Final.R
# ==============================================================================
# Extrai a matriz (decil de renda x decil de desempenho x destino), que e a
# UNICA fonte do modelo e da figura. O destino vem do LEFT JOIN real com o
# Censo da Educacao Superior 2013-2017 — nunca de formula.
#
# Regra de destino (2.cruzar_e_preparar_bases_para_analise.do, linhas 35-42):
#   ano_ingresso = menor ano em que o individuo aparece no CENSUP 2013-2017
#   categ 1-3 -> Publica ; 4-9 -> Privada ; ausente -> Nao ingresso
#
# COUNT(DISTINCT b.CPF_MASC), e nao COUNT(*): o SUP_ALUNO tem uma linha por
# vinculo, entao um individuo com dois cursos apareceria duas vezes. Os autores
# resolvem isso com um filtro aleatorio de individualizacao, que exige operacao
# por individuo — proibida sob privacidade diferencial. O DISTINCT resolve a
# duplicacao dentro da celula; a residual (individuo classificado em dois
# setores) e medida ao final e reportada.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
})

source(here::here("shared-pipeline", "sedap", "010_SEDAP_Cliente.R"))
source(here::here("shared-pipeline", "sedap", "senkevics", "015_Especificacao.R"))

cortes <- read_csv(file.path(DIR_DADOS, "025_pontos_corte.csv"), show_col_types = FALSE)
base_r <- read_csv(file.path(DIR_DADOS, "025_base_faixa_real.csv"), show_col_types = FALSE)

# Todos os helpers de SQL (filtro_coorte, expr_media, expr_presenca,
# join_ultima_edicao, expr_rfpc, expr_decil, expr_destino, join_censup) e a
# config de coorte vem de 015_Especificacao.R.

# ------------------------------------------------------------------------------
cat("=== Matriz renda x desempenho x destino, por edicao ===\n")

matriz <- map_dfr(ANOS_ENEM, function(ano) {
  ue <- join_ultima_edicao(ano)
  sql <- sprintf(
    "
SELECT
  %s AS income_D,
  %s AS performance,
  %s AS choice,
  COUNT(DISTINCT b.CPF_MASC) AS N
FROM %s b
INNER JOIN raw.ENEM_%d_SEDAP e ON b.CPF_MASC = e.CPF_MASC
%s
%s
WHERE %s
  AND %s
%s
GROUP BY income_D, performance, choice
",
    expr_decil(expr_rfpc(ano, base_r$sm_real[base_r$ano_enem == ano]), cortes$corte_renda),
    expr_decil(expr_media(ano), cortes$corte_nota),
    expr_destino(),
    TABELA_COORTE, ano, ue$join, join_censup,
    filtro_coorte("b"), expr_presenca(ano), ue$where
  )

  cat(sprintf("  ENEM %d ... ", ano))
  r <- sedap_query(sql, verbose = FALSE)
  if (is.null(r) || !nrow(r)) {
    cat("FALHOU\n")
    return(NULL)
  }
  cat(sprintf("%d celulas, N = %s\n", nrow(r), format(sum(r$N), big.mark = " ")))
  r %>% mutate(ano_enem = ano)
})

if (!nrow(matriz)) stop("Nenhuma edicao retornou dados — nada a agregar.")

# Desvio D7: renda ou nota ausente sai do CASE como 'NA' e e descartada aqui,
# com a contagem reportada. Antes de 2026-09-14 esses casos caiam no 10D.
excluidos <- matriz %>% filter(income_D == "NA" | performance == "NA")
matriz <- matriz %>% filter(income_D != "NA", performance != "NA")
cat(sprintf(
  "\nD7 — excluidos por renda/nota ausente: %s individuos em %d celulas (%.2f%% do total extraido)\n",
  format(sum(excluidos$N), big.mark = " "), nrow(excluidos),
  100 * sum(excluidos$N) / (sum(excluidos$N) + sum(matriz$N))
))
if (nrow(excluidos)) {
  print(as.data.frame(excluidos %>% group_by(ano_enem, performance) %>%
    summarise(N = sum(N), .groups = "drop") %>% arrange(ano_enem, performance)))
}
write_csv(excluidos, file.path(DIR_DADOS, "030_excluidos_D7.csv"))

# Consolidacao entre edicoes. Cada individuo entra por uma unica edicao
# (anti-join), entao somar as edicoes nao duplica ninguem.
matriz_final <- matriz %>%
  group_by(income_D, performance, choice) %>%
  summarise(n = sum(N), .groups = "drop")

write_csv(matriz, file.path(DIR_DADOS, "030_matriz_por_edicao.csv"))
write_csv(matriz_final, file.path(DIR_DADOS, "030_matriz_final.csv"))

# ------------------------------------------------------------------------------
# Validacao contra os numeros publicados
# ------------------------------------------------------------------------------
tot <- sum(matriz_final$n)
por_destino <- matriz_final %>%
  count(choice, wt = n) %>%
  mutate(p = n / tot)

cat("
=== Validacao ===
")

# N de referencia: o total deduplicado que o 020 mediu para ESTA coorte.
n_020 <- sum(read_csv(file.path(DIR_DADOS, "020_dist_nota.csv"), show_col_types = FALSE)$N)
cat(sprintf(
  "N total: %s   (020 desta coorte: %s)
",
  format(tot, big.mark = " "), format(n_020, big.mark = " ")
))
print(as.data.frame(por_destino %>% mutate(p = sprintf("%.1f%%", 100 * p))))

acesso <- por_destino %>% filter(choice != "No Access")
if (nrow(acesso) == 2) {
  cat(sprintf(
    "
Acesso: %.1f%%. Entre os que ingressaram: %.1f%% privado, %.1f%% publico
",
    100 * sum(acesso$n) / tot,
    100 * acesso$n[acesso$choice == "Private"] / sum(acesso$n),
    100 * acesso$n[acesso$choice == "Public"] / sum(acesso$n)
  ))
  if (COORTE_ANO == 2012) {
    cat("Artigo (p. 869): 68,9% de acesso; destes 75,8% privado e 24,2% publico
")
  }
}

# Individuo classificado em dois setores inflaria o total acima do N do 020.
cat(sprintf("Inflacao por classificacao dupla de setor: %+.2f%%
", 100 * (tot / n_020 - 1)))

cat("\n[OK] 030 concluido.\n")
