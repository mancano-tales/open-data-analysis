# ==============================================================================
# SCRIPT: 037_Modelos_Logit_RRA_Renda.R
#
# OBJETIVO / PURPOSE:
#   Réplica adaptada da Figura 3 de Salata, Bringhenti e Miranda (2025, Dados)
#   — "Riscos Relativos Ajustados" (RRA) de ingresso no ensino superior (DEP2),
#   por decil E quintil de renda domiciliar per capita, Brasil 1992-2025.
#
# MÉTODO (notas de rodapé 18-19 do artigo):
#   Para cada ano: logit de ens_sup em função de decil/quintil de renda +
#   controles (idade, sexo, cor, região, rural/urbano), com peso amostral.
#   RRA = razão entre a probabilidade média ajustada (over estimation sample)
#   do estrato mais rico e do mais pobre — equivalente ao par Stata
#   margins/nlcom (ou adjrr) que o artigo usa. Aqui: marginaleffects::
#   avg_predictions() + hypotheses(), delta method para o IC 95%.
#
# DIFERENÇA DELIBERADA vs. Salata et al. (2025):
#   população geral 18-24 (não só "filhos morando com os pais"/rec_pos), logo
#   a magnitude não bate ponto a ponto com a Figura 3 publicada — ver
#   Methodology do post `rra-renda-decil-quintil` para a discussão completa.
#
# ACHADO TÉCNICO: peso PNAD bruto (média ~483, chega a ~10mil) quebra a
#   convergência do glm(family=quasibinomial, weights=peso) — normalizamos
#   peso_norm = peso/mean(peso) por ano antes de ajustar (mesma MLE
#   ponderada, só estabiliza numericamente).
#
# PRÉ-REQUISITO: rodar 035_Splice_Microdados.R antes (gera o parquet lido
#   abaixo).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(here)
  library(marginaleffects)
})

options(scipen = 999)

BASE_DIR   <- here::here("data-raw", "harmonizing-br-data")
OUTPUT_DIR <- file.path(BASE_DIR, "output")

# Anos sem PNAD/PNADC no painel harmonizado: 1994/2000/2010 (sem coleta) e
# 2020/2021 (excluídos por incompatibilidade metodológica da PNADC
# retroponderada/5ª visita daqueles anos). Interpolados na seção 4 abaixo.
ANOS_SEM_DADO <- c(1994L, 2000L, 2010L, 2020L, 2021L)

REGIOES <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")  # Norte = referência

cat("══════ Modelos logit anuais e RRA de renda (decil e quintil) ══════\n")

# ── 1. Carregar painel harmonizado (18-24) ──────────────────────────────────
df <- arrow::read_parquet(
  file.path(OUTPUT_DIR, "Microdados_Jovens_18_24_1992_2025.parquet"),
  col_select = c("ano", "fonte", "idade", "peso", "ens_sup",
                 "decil", "quintil", "sexo", "cor", "regiao", "rural")
)

anos_disponiveis <- sort(unique(df$ano))
cat(sprintf("Anos com dado no painel: %d (%d a %d). Faltantes a interpolar: %s\n",
            length(anos_disponiveis), min(anos_disponiveis), max(anos_disponiveis),
            paste(ANOS_SEM_DADO, collapse = ", ")))

# ── 2. Função: RRA ajustado para um ano × variável de renda ────────────────
# var: "decil" (n_cat=10) ou "quintil" (n_cat=5). Retorna 1 linha de resultado.
calcular_rra_ano <- function(ano_i, dados_ano, var, n_cat) {
  d <- dados_ano %>%
    mutate(
      cat_     = factor(.data[[var]]),
      sexo_f   = factor(sexo,   levels = c(1L, 2L), labels = c("Homem", "Mulher")),
      cor_f    = factor(cor,    levels = c(0L, 1L), labels = c("Branca", "Negra")),
      regiao_f = factor(regiao, levels = REGIOES),
      rural_f  = factor(rural,  levels = c(0L, 1L), labels = c("Urbano", "Rural"))
    ) %>%
    filter(!is.na(cat_), !is.na(idade), !is.na(sexo_f), !is.na(cor_f),
           !is.na(regiao_f), !is.na(rural_f), !is.na(peso), peso > 0,
           !is.na(ens_sup))

  n_niveis <- nlevels(droplevels(d$cat_))
  if (n_niveis != n_cat) {
    return(data.frame(ano = ano_i, agregacao = var, rra = NA_real_,
                       ic_inf = NA_real_, ic_sup = NA_real_,
                       p_top = NA_real_, p_bottom = NA_real_, n = nrow(d),
                       obs = sprintf("abortado: %d categorias de %s, esperava %d",
                                     n_niveis, var, n_cat)))
  }

  # Normalização de peso (ver cabeçalho) — só estabilidade numérica do IRLS,
  # não muda a MLE ponderada (invariante a reescala constante do peso).
  d <- d %>% mutate(peso_norm = peso / mean(peso))

  modelo <- tryCatch(
    glm(ens_sup ~ cat_ + idade + sexo_f + cor_f + regiao_f + rural_f,
        weights = peso_norm, family = quasibinomial(), data = d),
    error = function(e) e
  )
  if (inherits(modelo, "error") || !isTRUE(modelo$converged)) {
    return(data.frame(ano = ano_i, agregacao = var, rra = NA_real_,
                       ic_inf = NA_real_, ic_sup = NA_real_,
                       p_top = NA_real_, p_bottom = NA_real_, n = nrow(d),
                       obs = "modelo nao convergiu"))
  }

  preds <- tryCatch(
    marginaleffects::avg_predictions(modelo, variables = "cat_", wts = "peso_norm"),
    error = function(e) e
  )
  if (inherits(preds, "error")) {
    return(data.frame(ano = ano_i, agregacao = var, rra = NA_real_,
                       ic_inf = NA_real_, ic_sup = NA_real_,
                       p_top = NA_real_, p_bottom = NA_real_, n = nrow(d),
                       obs = paste("avg_predictions falhou:", conditionMessage(preds))))
  }

  preds_ord <- preds[order(as.integer(as.character(preds$cat_))), ]
  ordem_esperada <- as.character(1:n_cat)
  if (!identical(as.character(preds_ord$cat_), ordem_esperada)) {
    stop(sprintf(
      "Ano %d (%s): ordem de categorias de avg_predictions (%s) nao bate com 1:%d — ",
      ano_i, var, paste(preds_ord$cat_, collapse = ","), n_cat),
      "inseguro usar indices posicionais b1/bN em hypotheses().")
  }

  # IC via delta method na escala LOG, não na escala direta da razão. Com o
  # denominador (p_bottom) muito próximo de zero — comum nos anos 90, ex.
  # D1=0,10% de acesso bruto — o delta method direto em b10/b1 produz IC
  # absurdo (chega a incluir valores negativos, impossível para uma razão de
  # probabilidades). Na escala log, de log(b10)-log(b1), o IC fica sempre
  # positivo depois de exp() — prática padrão para razões de risco. O ponto
  # estimado da razão é idêntico nas duas escalas.
  hip_log <- sprintf("log(b%d) - log(b1) = 0", n_cat)
  razao   <- marginaleffects::hypotheses(preds_ord, hypothesis = hip_log)

  data.frame(
    ano = ano_i, agregacao = var,
    rra = exp(razao$estimate), ic_inf = exp(razao$conf.low), ic_sup = exp(razao$conf.high),
    p_top = preds_ord$estimate[n_cat], p_bottom = preds_ord$estimate[1],
    n = nrow(d), obs = ""
  )
}

# ── 3. Loop principal: cada ano disponível × {decil, quintil} ──────────────
resultados <- list()
for (ano_i in anos_disponiveis) {
  dados_ano <- df %>% filter(ano == ano_i)
  cat(sprintf("  → %d (n=%d)... ", ano_i, nrow(dados_ano)))

  r_decil   <- calcular_rra_ano(ano_i, dados_ano, "decil", 10L)
  r_quintil <- calcular_rra_ano(ano_i, dados_ano, "quintil", 5L)
  resultados[[length(resultados) + 1L]] <- r_decil
  resultados[[length(resultados) + 1L]] <- r_quintil

  cat(sprintf("decil RRA=%.2f [%.2f-%.2f]%s | quintil RRA=%.2f [%.2f-%.2f]%s\n",
              r_decil$rra, r_decil$ic_inf, r_decil$ic_sup,
              if (nzchar(r_decil$obs)) paste0(" (", r_decil$obs, ")") else "",
              r_quintil$rra, r_quintil$ic_inf, r_quintil$ic_sup,
              if (nzchar(r_quintil$obs)) paste0(" (", r_quintil$obs, ")") else ""))
}

df_rra <- bind_rows(resultados)

falhas <- df_rra %>% filter(is.na(rra))
if (nrow(falhas) > 0L) {
  cat(sprintf("\n⚠ %d ano×agregação falharam (modelo não convergiu ou categoria ausente):\n",
              nrow(falhas)))
  print(as.data.frame(falhas))
}

# ── 4. Interpolação dos anos sem PNAD/PNADC ─────────────────────────────────
# Nota 21 do artigo: "Para os anos em que não houve coleta da PNAD, estimamos
# o RRA como ponto médio do período imediatamente anterior e posterior."
# Aplicado ao ponto (rra) e aos limites do IC (ic_inf, ic_sup). 2020-2021
# entram na mesma lógica.
interpolar_ano <- function(df_agreg, ano_alvo) {
  anos_ord <- sort(df_agreg$ano)
  antes <- max(anos_ord[anos_ord < ano_alvo])
  depois <- min(anos_ord[anos_ord > ano_alvo])
  linha_antes  <- df_agreg %>% filter(ano == antes)
  linha_depois <- df_agreg %>% filter(ano == depois)
  data.frame(
    ano = ano_alvo, agregacao = unique(df_agreg$agregacao),
    rra    = mean(c(linha_antes$rra,    linha_depois$rra)),
    ic_inf = mean(c(linha_antes$ic_inf, linha_depois$ic_inf)),
    ic_sup = mean(c(linha_antes$ic_sup, linha_depois$ic_sup)),
    p_top = NA_real_, p_bottom = NA_real_, n = NA_integer_,
    obs = sprintf("interpolado (ponto medio de %d e %d)", antes, depois)
  )
}

interpolados <- list()
for (var in c("decil", "quintil")) {
  df_agreg <- df_rra %>% filter(agregacao == var, !is.na(rra))
  for (ano_alvo in ANOS_SEM_DADO) {
    interpolados[[length(interpolados) + 1L]] <- interpolar_ano(df_agreg, ano_alvo)
  }
}
df_rra <- bind_rows(df_rra, interpolados) %>% arrange(agregacao, ano)

# ── 5. Salvar ────────────────────────────────────────────────────────────────
output_file <- file.path(OUTPUT_DIR, "RRA_Renda_1992_2025.csv")
write.csv(df_rra, output_file, row.names = FALSE, fileEncoding = "UTF-8")

cat(sprintf("\n✅ Concluído! %d linhas (%d anos × 2 agregações, incl. %d interpolados) salvas em %s\n",
            nrow(df_rra), length(anos_disponiveis) + length(ANOS_SEM_DADO),
            length(ANOS_SEM_DADO) * 2L, output_file))
