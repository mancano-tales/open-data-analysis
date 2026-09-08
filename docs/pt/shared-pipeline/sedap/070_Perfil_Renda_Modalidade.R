# ==============================================================================
# 070_Perfil_Renda_Modalidade.R
# ==============================================================================
# WP1: Extração da Distribuição de Renda (Q006) de Ingressantes no SEDAP+
# por Modalidade de Ensino (Presencial x EaD) e Setor (Privado x Público) (2014-2024).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

source(here::here("4-DA-Code", "2026-08_SEDAP", "010_SEDAP_Cliente.R"))

cat("======================================================================\n")
cat("1. EXTRAINDO Q006 DE INGRESSANTES (2014-2024) POR MODALIDADE E SETOR\n")
cat("======================================================================\n\n")

anos <- 2014:2024
lista_resultados <- list()

query_setor <- function(tab_sup, tab_enem, ano, setor_nome, cods_priv_pub) {
  # Tenta primeiro com aspas (string), depois sem aspas (int)
  if (cods_priv_pub == "privado") {
    str_cats <- c("'4', '5', '7'", "4, 5, 7")
  } else {
    str_cats <- c("'1', '2', '3'", "1, 2, 3")
  }

  sql_fmt <- "
    SELECT
      a.TP_MODALIDADE_ENSINO,
      e.Q006,
      COUNT(*) AS N
    FROM %s a
    INNER JOIN %s e ON a.CPF_MASC = e.CPF_MASC
    WHERE a.IN_MATRICULA = 1
      AND a.NU_ANO_INGRESSO = %d
      AND a.TP_CATEGORIA_ADMINISTRATIVA IN (%s)
      AND e.Q006 IS NOT NULL AND e.Q006 <> '' AND e.Q006 <> 'NULL' AND e.Q006 <> '*'
    GROUP BY a.TP_MODALIDADE_ENSINO, e.Q006
  "

  res <- NULL
  for (cat_expr in str_cats) {
    sql <- sprintf(sql_fmt, tab_sup, tab_enem, ano, cat_expr)
    res <- tryCatch(
      {
        sedap_query(sql)
      },
      error = function(e) {
        NULL
      }
    )
    if (!is.null(res) && is.data.frame(res) && nrow(res) > 0) break
  }

  if (!is.null(res) && is.data.frame(res) && nrow(res) > 0) {
    res %>%
      mutate(
        ano = ano,
        setor = setor_nome,
        N = as.numeric(N),
        Modalidade = ifelse(as.numeric(TP_MODALIDADE_ENSINO) == 1, "Presencial", "EaD")
      )
  } else {
    NULL
  }
}

for (ano in anos) {
  tab_sup <- sprintf("raw.SUP_ALUNO_%d", ano)
  tab_enem <- sprintf("raw.ENEM_%d_SEDAP", ano)

  cat(sprintf("Extraindo %s x %s (Ingressantes %d)...\n", tab_sup, tab_enem, ano))

  # Privado
  res_priv <- query_setor(tab_sup, tab_enem, ano, "Privado", "privado")
  if (!is.null(res_priv)) {
    lista_resultados[[length(lista_resultados) + 1]] <- res_priv
    cat(sprintf("  -> Setor Privado %d: %.0f registros extraídos\n", ano, sum(res_priv$N)))
  } else {
    cat(sprintf("  [X] Falha no setor privado (%d)\n", ano))
  }

  # Público
  res_pub <- query_setor(tab_sup, tab_enem, ano, "Público", "publico")
  if (!is.null(res_pub)) {
    lista_resultados[[length(lista_resultados) + 1]] <- res_pub
    cat(sprintf("  -> Setor Público %d: %.0f registros extraídos\n", ano, sum(res_pub$N)))
  } else {
    cat(sprintf("  [X] Falha no setor público (%d)\n", ano))
  }
}

df_extraido_completo <- bind_rows(lista_resultados)

out_csv <- here::here("4-DA-Code", "2026-08_SEDAP", "060_Analise_ENEM_Renda", "extraido_perfil_renda_modalidade_2014_2024.csv")
write_csv(df_extraido_completo, out_csv)
cat(sprintf("\n======================================================================\n"))
cat(sprintf("[WP1 CONCLUÍDO] Total de registros: %.0f | Salvo em: %s\n", sum(df_extraido_completo$N), out_csv))
cat(sprintf("======================================================================\n"))
