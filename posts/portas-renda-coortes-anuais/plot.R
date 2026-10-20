# ==============================================================================
# posts/portas-renda-coortes-anuais/plot.R
#
# Vias de ingresso no ensino superior por decil de renda per capita, coorte a
# coorte, de 2012 a 2021. Estende os posts portas-renda-coortes-empilhado e
# portas-renda-coortes-ead (so 2013 e 2020) e, diferente deles, conta apenas
# quem esteve presente nas quatro provas objetivas do ENEM do ano da coorte
# (ver shared-pipeline/sedap/portas-anuais/020_Extrair_Coortes_Anuais.R).
#
# Renda per capita e decis: mesmos pontos medios e mesma regra de 052/055
# (celulas faixa x grupo de moradores ordenadas por renda per capita e
# cortadas em decis dentro de cada coorte, celulas na fronteira divididas
# proporcionalmente).
#
# Figuras (cada uma em ingles, -en, e portugues, -pt):
#   fig-vias       probabilidade de ingresso em ate 3 anos, total e por via
#   fig-modalidade probabilidade de ingresso em EaD e presencial
#   fig-cobertura  parcela da coorte com CPF e presente no ENEM
# thumbnail.png e a fig-vias em ingles.
#
# Le data/ (redistribuido) ou, se existir, a reextracao em data-raw/.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(here)
})

source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())

POST <- here::here("posts", "portas-renda-coortes-anuais")
DIR_DATA <- file.path(POST, "data")
DIR_RAW <- here::here("data-raw", "sedap", "portas-anuais")
COORTES <- 2012:2021

SM_17 <- c(
  A = 0, B = 0.5, C = 1.25, D = 1.75, E = 2.25, F = 2.75, G = 3.5,
  H = 4.5, I = 5.5, J = 6.5, K = 7.5, L = 8.5, M = 9.5, N = 11,
  O = 13.5, P = 17.5, Q = 25
)
MOR_MEDIO <- c("1_2" = 1.8, "3_4" = 3.5, "5_6" = 5.4, "7_mais" = 7.8)

ler <- function(nome) {
  arq <- file.path(DIR_RAW, nome)
  if (!file.exists(arq)) arq <- file.path(DIR_DATA, nome)
  if (!file.exists(arq)) stop("Arquivo ausente: ", nome, ". Rode shared-pipeline/sedap/portas-anuais/.")
  read.csv(arq, stringsAsFactors = FALSE)
}

# Celulas faixa x moradores -> decis de renda per capita (regra de 051/052).
por_decil <- function(m, k = 10) {
  cel <- m |>
    mutate(COD = as.character(COD), rpc = unname(SM_17[FAIXA_RENDA]) / unname(MOR_MEDIO[MORADORES_GRUPO])) |>
    pivot_wider(
      id_cols = c(FAIXA_RENDA, MORADORES_GRUPO, rpc), names_from = COD,
      values_from = N, values_fill = 0, names_prefix = "c"
    )
  if (anyNA(cel$rpc)) stop("Celula sem renda per capita (faixa ou grupo de moradores fora do mapa).")
  cel <- cel |>
    mutate(n_cel = rowSums(across(starts_with("c")))) |>
    arrange(rpc, desc(unname(MOR_MEDIO[MORADORES_GRUPO])))
  tot <- sum(cel$n_cel)
  ini <- c(0, cumsum(cel$n_cel)[-nrow(cel)]) / tot
  fim <- cumsum(cel$n_cel) / tot
  cortes <- seq(0, 1, length.out = k + 1)
  cods <- grep("^c[0-9]$", names(cel), value = TRUE)
  bind_rows(lapply(seq_len(k), function(d) {
    w <- pmax(0, pmin(fim, cortes[d + 1]) - pmax(ini, cortes[d])) / pmax(fim - ini, .Machine$double.eps)
    n_d <- sum(w * cel$n_cel)
    data.frame(
      decil = d, n = n_d, cod = sub("^c", "", cods),
      p = 100 * vapply(cods, function(cc) sum(w * cel[[cc]]), numeric(1)) / n_d,
      row.names = NULL
    )
  }))
}

vias <- bind_rows(lapply(COORTES, function(a) por_decil(ler(sprintf("matriz_%d_porta.csv", a))) |> mutate(coorte = a)))
modal <- bind_rows(lapply(COORTES, function(a) por_decil(ler(sprintf("matriz_%d_modalidade.csv", a))) |> mutate(coorte = a)))
stopifnot(all(vias$p >= 0 & vias$p <= 100), all(modal$p >= 0 & modal$p <= 100))

# Total = soma das vias (cod 1-5); cod 0 = nao ingressou.
vias_tot <- vias |>
  filter(cod != "0") |>
  group_by(coorte, decil) |>
  summarise(p = sum(p), .groups = "drop") |>
  mutate(cod = "T")
vias <- bind_rows(vias |> filter(cod != "0"), vias_tot)

# Tabela de saida com os numeros da figura, para conferencia.
write.csv(
  bind_rows(vias |> mutate(tipo = "via"), modal |> filter(cod != "0") |> mutate(tipo = "modalidade")) |>
    select(tipo, coorte, decil, cod, p) |> mutate(p = round(p, 2)),
  file.path(DIR_DATA, "probabilidades_por_decil.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------------------------
# Rotulos
# ------------------------------------------------------------------------------
TXT <- list(
  en = list(
    vias = c(
      T = "Total entry", `1` = "Public, affirmative action", `2` = "Public, open competition",
      `3` = "ProUni", `4` = "FIES", `5` = "Private, without subsidy"
    ),
    modal = c(`1` = "Distance learning", `2` = "In-person"),
    x = "Secondary-school cohort (entry within the next three years)",
    y = "Probability of entry", decil = "Income decile",
    decis = c("1st", "2nd", "3rd", paste0(4:10, "th")),
    cob = c(cpf = "With CPF (linkable)", enem = "Present at all four ENEM tests"),
    ycob = "Share of the final-year cohort",
    sep = "."
  ),
  pt = list(
    vias = c(
      T = "Ingresso total", `1` = "Pública, reserva de vagas", `2` = "Pública, ampla concorrência",
      `3` = "ProUni", `4` = "FIES", `5` = "Privada sem subsídio"
    ),
    modal = c(`1` = "Educação a distância", `2` = "Presencial"),
    x = "Coorte do ensino médio (ingresso nos três anos seguintes)",
    y = "Probabilidade de ingresso", decil = "Decil de renda",
    decis = paste0(1:10, "º"),
    cob = c(cpf = "Com CPF (vinculável)", enem = "Presente nas quatro provas do ENEM"),
    ycob = "Parcela da coorte da série final",
    sep = ","
  )
)

CORES_DECIL <- rev(.paleta_sequencial_hcl(12)[1:10]) # claro = mais pobre

grafico_decis <- function(d, rot, L, ncol) {
  d <- d |> mutate(
    painel = factor(rot[cod], levels = rot),
    decil_f = factor(decil, levels = 1:10, labels = L$decis)
  )
  ggplot(d, aes(coorte, p / 100, colour = decil_f, group = decil_f)) +
    geom_line(linewidth = 0.45) +
    geom_point(size = 0.7) +
    facet_wrap(~painel, ncol = ncol, scales = "free_y") +
    scale_colour_manual(
      values = CORES_DECIL, name = L$decil,
      guide = guide_legend(nrow = 1, label.position = "bottom", keywidth = unit(11, "pt"))
    ) +
    scale_x_continuous(breaks = seq(2012, 2021, 3), minor_breaks = NULL) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1, decimal.mark = L$sep)) +
    expand_limits(y = 0) +
    labs(x = L$x, y = L$y) +
    theme(legend.position = "top")
}

# Cobertura: BAS (sondagem) e N presente no ENEM (soma da matriz da coorte).
bas <- ler("sondagem_bas.csv") |> filter(ano %in% COORTES)
n_enem <- vapply(COORTES, function(a) sum(ler(sprintf("matriz_%d_porta.csv", a))$N), numeric(1))
cob <- bas |>
  transmute(coorte = ano, cpf = N_COM_CPF / N_SERIE_FINAL, enem = n_enem[match(ano, COORTES)] / N_SERIE_FINAL) |>
  pivot_longer(c(cpf, enem), names_to = "serie", values_to = "s")
write.csv(cob |> mutate(s = round(100 * s, 2)), file.path(DIR_DATA, "cobertura.csv"), row.names = FALSE)

salvar <- function(p, nome, altura) {
  ggsave(file.path(POST, nome), p, width = LARGURA_TEXTO, height = altura, dpi = 300, bg = "white")
}

for (lg in c("en", "pt")) {
  L <- TXT[[lg]]
  f1 <- grafico_decis(vias, L$vias, L, ncol = 2)
  f2 <- grafico_decis(modal |> filter(cod != "0"), L$modal, L, ncol = 2)
  f3 <- ggplot(cob, aes(coorte, s, colour = serie)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.2) +
    scale_colour_manual(values = unname(PALETA_QUALITATIVA[1:2]), labels = L$cob, name = NULL) +
    scale_x_continuous(breaks = COORTES, minor_breaks = NULL) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1, decimal.mark = L$sep), limits = c(0, 1)) +
    labs(x = NULL, y = L$ycob) +
    theme(legend.position = "top")
  salvar(f1, sprintf("fig-vias-%s.png", lg), 7.6)
  salvar(f2, sprintf("fig-modalidade-%s.png", lg), 3.6)
  salvar(f3, sprintf("fig-cobertura-%s.png", lg), 3.0)
}
file.copy(file.path(POST, "fig-vias-en.png"), file.path(POST, "thumbnail.png"), overwrite = TRUE)

cat("\n[OK] figuras gravadas em posts/portas-renda-coortes-anuais/\n")
