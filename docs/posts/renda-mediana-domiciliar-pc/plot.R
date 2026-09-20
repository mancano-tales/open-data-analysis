# ==============================================================================
# SCRIPT: 242_Tese_Renda_Mediana_Domiciliar.R
# ODA: portado de 6-images-tables/final/renda-mediana-domiciliar-pc/2026-09-16_1107_242_Tese_Renda_Mediana_Domiciliar.R
#      em 2026-09-20; caminhos relativos a data-raw/.
#
# RENDA DOMICILIAR PER CAPITA MEDIANA, BRASIL, 1992-2025, em R$ de jan/2024.
#
# REFORMULADO EM 2026-09-16 (decisao do autor, apos ler o pacote de replicacao
# de Souza & Hecksher em 4-DA-Code/2026-09_Replicacao_Souza-Hecksher/). A
# primeira versao consumia o parquet principal, e por isso herdava dele duas
# escolhas feitas para OUTRO fim (medir desigualdade de ACESSO por decil) que
# nao servem a esta figura:
#
#   (a) UNIDADE DE RENDA. O parquet usa renda FAMILIAR per capita ate 2015
#       (V4722/V4724) e DOMICILIAR de 2016 (VD5008) — decisao D21, tomada
#       porque o efeito sobre o indice de Wagstaff e de 0,0008 a 0,0063 (101).
#       Sobre a MEDIANA o efeito e de outra ordem: familia e unidade menor e
#       mais homogenea, entao a renda per capita do meio da distribuicao sobe.
#       Medido contra a serie de Souza & Hecksher (que e domiciliar em toda a
#       serie), o desvio da nossa mediana era de +8 a +10% nos anos de PNAD e
#       de +1 a +3% nos anos de PNADC — ou seja, quase todo ele era a unidade.
#       AQUI: domiciliar em TODA a serie.
#
#   (b) RENDA ZERO. O parquet aplica a D02 (renda > 0), necessaria para
#       atribuir decil, mas que para uma figura de "renda do domicilio tipico"
#       descarta justamente os domicilios sem renda. Medido: excluir os zeros
#       eleva a mediana em +4,9% (2003), +2,4% (2009) e +1,4% (2014).
#       AQUI: universo OFICIAL, com renda zero — mesma escolha que o 238 ja
#       fez para o Gini/Palma, e a mesma de Souza & Hecksher.
#
# COMO A RENDA DOMICILIAR E CONSTRUIDA (1992-2015, PNAD):
#   renda_dom_pcta = V4721 / (moradores com V0401 fora de {6,7,8})
#   V4721 = "Rendimento mensal domiciliar (exclusive o rendimento das pessoas
#   cuja condicao na unidade domiciliar era pensionista, empregado domestico
#   ou parente do empregado domestico)" — por isso o denominador coerente
#   exclui exatamente essas tres condicoes. E a mesma construcao do
#   101_Sensitivity_Renda_Familiar_vs_Domiciliar.R (de onde vem toda a
#   maquinaria de leitura do bruto abaixo, incluindo a armadilha da chave de
#   domicilio) e a mesma de Souza & Hecksher (harmoniza_pnad.R: tamdom =
#   membros com v0401 <= 5).
#   De 2016 em diante a PNADC ja entrega VD5008, que e domiciliar per capita
#   pela mesma convencao — nao ha o que reconstruir.
#
# DEFLATOR: IPCA, jan/2024 (D01) — mantido, por decisao do autor, igual ao de
#   todas as demais figuras de valor real da tese (232/233/236/220B).
#   Souza & Hecksher usam precos MEDIOS de 2024; o fator entre as duas bases e
#   1,02211 (IPCA medio-2024 / IPCA jan-2024). Para comparar numero a numero
#   com a NT 120, multiplicar esta serie por 1,02211.
#
# EMENDA 2015/2016: medida e declarada, sem reescala (decisao do autor) — ver
#   o bloco de diagnostico ao final, que recalcula o degrau na sobreposicao
#   2012-2015 com a unidade ja homogeneizada. Souza & Hecksher, ao contrario,
#   encaixam em 2012 com fator POR ESTATISTICA (0,9619 para a mediana, 1,0069
#   para a media); aqui a serie e leitura direta do dado, como nas demais.
#
# 2020-2021: PNADC visita 5 (script 022), coletada por telefone (CATI) —
#   marcador vazado, mesma ressalva de vies da versao media (hoje deprecada).
#
# RESULTADO DA REFORMULACAO (medido em 2026-09-16, secao 4b deste script, com
# a serie deles convertida para jan/2024). Desvio nosso menos deles:
#
#                                     antes (familiar, s/ zeros)   depois
#   media, toda a serie                        +2,6%                +0,2%
#   mediana, toda a serie                      +6,5%                +3,2%
#   mediana, anos de PNAD (<=2015)             +8,2%                +4,3%
#   mediana, anos de PNADC (s/ 2022)           +1,6%                +0,4%
#
# A media passou a bater praticamente na virgula, e a mediana nos anos de
# PNADC tambem. Sobra um residuo sistematico de ~4% na mediana dos anos de
# PNAD. NAO e o algoritmo de quantil: testado em 2003 e 2013, a mediana pelo
# Hmisc::wtd.quantile e pelo tipo 7 interpolado deles bate no centavo (a massa
# pontual sobre o valor mediano e de 0,02% a 0,27% da populacao, pequena
# demais para separar os dois algoritmos). A hipotese que sobra e o CONCEITO
# de renda: nos usamos V4721, o agregado domiciliar do proprio IBGE (soma de
# V4720, todas as fontes), enquanto eles reconstroem a renda somando
# componentes individuais e convertendo ausencia em zero
# (`is.na(.x) ~ 0` em harmoniza_pnad.R), o que rebaixa o meio da distribuicao.
# Isolar isso exigiria replicar a construcao deles componente a componente —
# nao feito. Registrado em 9-vers/plan/2026-08-26_Relatorio_Divergencia_...md.
#
# FONTES: 5-data/pnad_anual_raw/<ano>/ (bruto, via dicionario SAS do IBGE)
#         5-data/pnadc_consolidado_2012_2025_interview1.rds (preserva zeros)
#         output/PNADC_Visita5_2019_2022.parquet (2020-2021)
# CACHE:  output/242_serie_renda_domiciliar.rds (apagar para reextrair)
# VER TAMBEM: 101 (sensibilidade familiar x domiciliar), 238 (universo oficial
#             com zeros),
#             R/deprecated/237 (versao media, deprecada em 2026-09-16 por medir
#             renda familiar sem zeros — ver o cabecalho dela)
# ESTILO: utils/plot_theme.R
# ==============================================================================

PROMOVER <- TRUE
FORCE_REEXTRACAO <- FALSE
ANO_INICIO <- 1992L

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(readr)
  library(stringr)
  library(ggplot2)
  library(ggrepel)
  library(scales)
  library(here)
  library(Hmisc)     # wtd.quantile
  library(deflateBR)
  library(patchwork) # composicao com a faixa de governos (banner_governos)
})
source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())
options(scipen = 999)

BASE_DIR <- here::here("data-raw", "harmonizing-br-data")
OUTPUT_DIR <- file.path(BASE_DIR, "output")
CACHE_PNAD <- here::here("data-raw", "pnad_anual_raw")
CACHE_PNADC <- here::here("data-raw", "pnadc_consolidado_2012_2025_interview1.rds")
if (!file.exists(CACHE_PNADC)) {
  stop("Cache PNADC nao encontrado em data-raw/. Rode antes: ",
       "shared-pipeline/pnadc/000_PNADC_Download_Consolidate.R", call. = FALSE)
}
if (!dir.exists(CACHE_PNAD)) {
  stop("Microdados brutos da PNAD Anual nao encontrados em data-raw/pnad_anual_raw/. Rode antes: ",
       "shared-pipeline/pnadc/050_PNAD_Historica_Manual_Import_1992_1999.R e ",
       "shared-pipeline/pnadc/020_PNAD_Anual_Manual_Import_2001_2015.R", call. = FALSE)
}
PQ_V5 <- file.path(OUTPUT_DIR, "PNADC_Visita5_2019_2022.parquet")
CACHE_SERIE <- file.path(OUTPUT_DIR, "242_serie_renda_domiciliar.rds")
SCRIPT_PATH <- here::here("posts", "renda-mediana-domiciliar-pc", "plot.R")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
FIG_LABEL <- "renda-mediana-domiciliar-pc"

COR_LINHA <- "#0072B2" # azul Okabe-Ito
X_EMENDA <- 2015.5     # PNAD -> PNADC
ANO_SPLICE <- 2015L    # PNAD Anual ate aqui; PNADC a partir de 2016
ANOS_VISITA5 <- c(2020L, 2021L)
FATOR_BASE_SH <- 1.02211 # jan/2024 -> medio/2024, para comparar com a NT 120

# Anos da PNAD com microdados no cache local (sem 1994/2000/2010: sem pesquisa).
ANOS_PNAD <- c(1992, 1993, 1995:1999, 2001:2009, 2011:2015)

# Rotulagem reduzida: inicio, ano-base do capitulo 2, fundo do ciclo e fim.
ANOS_ROTULO <- c(1992, 2002, 2014, 2019, 2025)

# ==============================================================================
# 1. EXTRACAO DA PNAD (1992-2015) — renda DOMICILIAR per capita, com zeros
#    Maquinaria copiada de 101_Sensitivity_Renda_Familiar_vs_Domiciliar.R.
# ==============================================================================
# `CONTROL` e o nome do numero de controle do domicilio em 1992-1993; de 1995
# em diante o IBGE passou a chama-lo `V0102`. Sem isso, 1992/93 caem fora por
# "dicionario sem as variaveis necessarias".
VARS <- c("V0102", "CONTROL", "V0103", "V0401", "V4721", "V4729", "UF")

parse_sas_dict <- function(sas_file, vars_of_interest) {
  linhas <- readLines(sas_file, encoding = "latin1", warn = FALSE)
  linhas <- iconv(linhas, from = "latin1", to = "ASCII", sub = "")
  linhas <- str_squish(linhas)
  out <- data.frame(var = character(), start = integer(), width = integer())
  for (v in vars_of_interest) {
    idx <- grep(paste0("^@\\s*[0-9]+\\s+", v, "(\\s+|$)"), linhas, ignore.case = TRUE)
    if (length(idx) == 0) next
    l <- linhas[idx[1]]
    start <- as.integer(str_extract(l, "(?<=@)[0-9]+"))
    parte <- strsplit(l, v, fixed = TRUE)[[1]]
    width <- as.integer(str_extract(parte[2], "[0-9]+"))
    if (!is.na(start) && !is.na(width)) {
      out <- bind_rows(out, data.frame(var = v, start = start, width = width))
    }
  }
  out %>%
    mutate(end = start + width - 1) %>%
    arrange(start)
}

# O IBGE variou os nomes ao longo da serie e os zips foram extraidos em
# subpastas diferentes. Nomes ja vistos no cache local:
#   dados : PES92.DAT, P96BR.TXT, Pessoas97 (sem extensao), PES2001.TXT, PES2013.txt
#   input : LAYOUT/sas/SAS_PES.TXT, SAS_PE96.TXT, SasPes97.txt, "INPUT PES2007.txt"
# Regra: o input e o arquivo de texto que esta numa pasta LAYOUT ou cujo nome
# comeca por input/sas; o dado e o arquivo de pessoas que sobra.
localiza <- function(ano) {
  todos <- list.files(file.path(CACHE_PNAD, ano),
    recursive = TRUE, full.names = TRUE, ignore.case = TRUE
  )
  todos <- todos[!grepl("\\.(zip|xls|xlsx|doc|docx|pdf)$", todos, ignore.case = TRUE)]
  todos <- todos[!grepl("temp_dados|temp_layout", todos, ignore.case = TRUE)]
  bn <- basename(todos)
  eh_input <- grepl("layout", todos, ignore.case = TRUE) |
    grepl("^(input|sas)", bn, ignore.case = TRUE)
  input <- todos[eh_input & grepl("\\.(txt|sas)$", bn, ignore.case = TRUE)]
  # Varios anos trazem o input de DOMICILIOS ao lado do de PESSOAS (ex.: 2013
  # tem "input DOM2013.txt" e "input PES2013.txt"). O de domicilios nao tem
  # V0401/V4729 e faria o ano cair fora silenciosamente — descartar.
  input <- input[!grepl("dom", basename(input), ignore.case = TRUE)]
  input <- c(
    input[grepl("\\.txt$", input, ignore.case = TRUE)],
    input[grepl("\\.sas$", input, ignore.case = TRUE)]
  )
  dados <- todos[!eh_input & grepl("^(pes|pessoa|p[0-9]{2})", bn, ignore.case = TRUE)]
  # Maior arquivo primeiro: o de pessoas e sempre o maior do ano.
  if (length(dados) > 1) dados <- dados[order(file.size(dados), decreasing = TRUE)]
  list(dados = dados, input = input)
}

extrai_pnad <- function(ano) {
  arq <- localiza(ano)
  if (length(arq$dados) == 0 || length(arq$input) == 0) {
    cat(sprintf("  [%s] microdados nao encontrados — ano pulado\n", ano))
    return(NULL)
  }
  pos <- parse_sas_dict(arq$input[1], VARS)
  tem_controle <- any(c("V0102", "CONTROL") %in% pos$var)
  if (!all(c("V4721", "V0401", "V0103", "V4729") %in% pos$var) || !tem_controle) {
    cat(sprintf("  [%s] dicionario sem as variaveis necessarias (%s) — ano pulado\n",
                ano, paste(setdiff(c("V4721", "V0401", "V0103", "V4729"), pos$var),
                           collapse = ", ")))
    return(NULL)
  }
  df <- read_fwf(arq$dados[1], fwf_positions(pos$start, pos$end, pos$var),
    col_types = cols(.default = col_character()), progress = FALSE
  ) %>%
    mutate(across(everything(), ~ suppressWarnings(as.numeric(.x))))
  if (!"UF" %in% names(df)) df$UF <- NA_real_
  # 1992-1993: o numero de controle chama-se CONTROL
  if (!"V0102" %in% names(df)) df$V0102 <- df$CONTROL

  # ⚠ GOTCHA (ver 101): a chave do domicilio muda de largura no meio da serie.
  # Ate 1999, V0102 tem 6 digitos e a UF vem a parte; de 2001, as duas
  # primeiras posicoes de V0102 JA sao a UF. Chavear sem a UF funde
  # domicilios de estados diferentes nos anos 90.
  df <- df %>% mutate(dom_id = paste(UF, V0102, V0103, sep = "-"))

  # D20 — sentinela de renda nao declarada (doze noves) -> NA
  df <- df %>% mutate(V4721 = ifelse(V4721 >= 999999999999, NA_real_, V4721))

  # Denominador: moradores fora das condicoes ja excluidas do numerador V4721.
  # Peso do domicilio = SOMA dos pesos de pessoa dos moradores contados no
  # denominador — e o que Souza & Hecksher fazem ao colapsar por domicilio
  # (`pesopop = sum(pesopop)`), e equivale a ponderar por PESSOA. Nao usar
  # `first(V4729)`: a mediana passaria a ser a do domicilio tipico, nao a da
  # pessoa tipica, que e a convencao do IBGE e a da NT 120.
  dom <- df %>%
    group_by(dom_id) %>%
    summarise(
      n_dom = sum(!V0401 %in% c(6, 7, 8)),
      renda_dom = first(V4721),
      peso = sum(V4729[!V0401 %in% c(6, 7, 8)], na.rm = TRUE),
      .groups = "drop"
    )

  # Guarda contra chave malformada (ver 101): a PNAD tem 3-4 moradores/domicilio.
  tam_medio <- mean(dom$n_dom)
  if (tam_medio < 2 || tam_medio > 6) {
    stop(sprintf("[%s] tamanho medio de domicilio implausivel (%.2f) — chave dom_id malformada",
                 ano, tam_medio))
  }

  out <- dom %>%
    filter(n_dom > 0, !is.na(renda_dom), !is.na(peso), peso > 0) %>%
    mutate(
      ano = as.integer(ano),
      renda_dom_pcta = renda_dom / n_dom,
      # Conversao cambial (D01): Cruzeiros/Cruzeiros Reais -> Reais
      renda_dom_pcta = case_when(
        ano == 1992L ~ renda_dom_pcta / 2750000,
        ano == 1993L ~ renda_dom_pcta / 2750,
        TRUE ~ renda_dom_pcta
      ),
    ) %>%
    select(ano, renda_dom_pcta, peso)
  cat(sprintf("  [%s] %s domicilios | mediana nominal R$ %.0f\n", ano,
              format(nrow(out), big.mark = "."),
              as.numeric(wtd.quantile(out$renda_dom_pcta, weights = out$peso,
                                      probs = 0.5, normwt = FALSE))))
  out
}

# ==============================================================================
# 2. SERIE (com cache)
# ==============================================================================
if (file.exists(CACHE_SERIE) && !FORCE_REEXTRACAO) {
  cat("Lendo cache da serie (", basename(CACHE_SERIE), ") — apague para reextrair.\n")
  serie <- readRDS(CACHE_SERIE)
} else {
  cat("Extraindo PNAD Anual do bruto (renda domiciliar per capita, com zeros)...\n")
  pnad <- bind_rows(lapply(ANOS_PNAD, extrai_pnad))
  pnad <- pnad %>%
    mutate(
      ref_date = as.Date(sprintf("%d-09-01", ano)),
      renda_real = deflateBR::ipca(renda_dom_pcta, ref_date, "01/2024")
    )

  cat("Lendo PNADC (VD5008 ja e domiciliar per capita; cache preserva zeros)...\n")
  pnadc <- readRDS(CACHE_PNADC) %>%
    filter(!is.na(peso), peso > 0, !is.na(renda_dom_pcta), ano > ANO_SPLICE,
           !(ano %in% ANOS_VISITA5)) %>%
    select(ano, renda_dom_pcta, peso) %>%
    mutate(
      ref_date = as.Date(sprintf("%d-06-15", ano)),
      renda_real = deflateBR::ipca(renda_dom_pcta, ref_date, "01/2024")
    )

  cat("Lendo PNADC visita 5 (2020-2021)...\n")
  v5 <- if (file.exists(PQ_V5)) {
    read_parquet(PQ_V5) %>%
      filter(ano %in% ANOS_VISITA5, !is.na(renda_dom_pcta), !is.na(peso), peso > 0) %>%
      select(ano, renda_dom_pcta, peso) %>%
      mutate(
        ref_date = as.Date(sprintf("%d-06-15", ano)),
        renda_real = deflateBR::ipca(renda_dom_pcta, ref_date, "01/2024")
      )
  } else {
    cat("  AVISO: visita 5 ausente — rode 022_PNADC_Visita5_2020_2021.R\n")
    NULL
  }

  # Sobreposicao 2012-2015 guardada para o diagnostico da emenda (secao 4).
  cat("Lendo PNADC 2012-2015 (so para medir o degrau da emenda)...\n")
  overlap <- readRDS(CACHE_PNADC) %>%
    filter(!is.na(peso), peso > 0, !is.na(renda_dom_pcta), ano %in% 2012:2015) %>%
    select(ano, renda_dom_pcta, peso) %>%
    mutate(
      ref_date = as.Date(sprintf("%d-06-15", ano)),
      renda_real = deflateBR::ipca(renda_dom_pcta, ref_date, "01/2024")
    )

  agrega <- function(d, fonte) {
    d %>%
      group_by(ano) %>%
      summarise(
        mediana = as.numeric(wtd.quantile(renda_real, weights = peso,
                                          probs = 0.5, normwt = FALSE, na.rm = TRUE)),
        media = weighted.mean(renda_real, peso, na.rm = TRUE),
        pct_zero = 100 * weighted.mean(renda_real == 0, peso, na.rm = TRUE),
        n = n(), .groups = "drop"
      ) %>%
      mutate(fonte = fonte)
  }

  # Trava: nenhum ano da PNAD pode ter caido fora em silencio.
  anos_lidos <- sort(unique(pnad$ano))
  faltando <- setdiff(ANOS_PNAD, anos_lidos)
  if (length(faltando) > 0) {
    stop(sprintf("Anos da PNAD nao extraidos: %s — conferir localiza()/dicionario",
                 paste(faltando, collapse = ", ")))
  }

  serie <- list(
    principal = bind_rows(
      agrega(pnad, "PNAD Anual") %>% mutate(visita5 = FALSE),
      agrega(pnadc, "PNAD Contínua") %>% mutate(visita5 = FALSE),
      if (!is.null(v5)) agrega(v5, "PNAD Contínua") %>% mutate(visita5 = TRUE)
    ) %>% arrange(ano),
    overlap_pnadc = agrega(overlap, "PNAD Contínua")
  )
  saveRDS(serie, CACHE_SERIE)
  cat("Cache gravado:", CACHE_SERIE, "\n")
}

df <- serie$principal %>%
  filter(ano >= ANO_INICIO) %>%
  arrange(ano) %>%
  mutate(seg = cumsum(c(1L, as.integer(diff(ano) > 1L))))

max_ano <- max(df$ano)
anos_disp <- df$ano

cat("\nSerie de renda DOMICILIAR per capita (R$ jan/2024), universo oficial:\n")
print(as.data.frame(df %>% transmute(ano, fonte, mediana = round(mediana, 0),
                                     media = round(media, 0),
                                     pct_zero = round(pct_zero, 2))),
      row.names = FALSE)

# ==============================================================================
# 3. FIGURA
# ==============================================================================
y_top <- max(df$mediana)
y_min <- min(df$mediana)
rot <- df %>% filter(ano %in% ANOS_ROTULO)

p <- ggplot(df, aes(x = ano, y = mediana, group = seg)) +
  linhas_transicao_gov() +
  annotate("segment",
    x = X_EMENDA, xend = X_EMENDA,
    y = y_min * 0.93, yend = y_top * 1.02,
    colour = "#9A9A9A", linewidth = 0.4
  ) +
  geom_line(colour = COR_LINHA, linewidth = 0.85) +
  geom_point(data = ~ filter(.x, !visita5), colour = COR_LINHA, size = 1.3) +
  geom_point(
    data = ~ filter(.x, visita5), colour = COR_LINHA,
    fill = "white", shape = 21, size = 1.7, stroke = 0.7
  ) +
  geom_text_repel(
    data = rot,
    aes(label = format(round(mediana), big.mark = ",", trim = TRUE)),
    size = 7.2 / .pt, family = THESIS_FONT, colour = "#333333",
    seed = 42, nudge_y = 0.06 * y_top,
    min.segment.length = 0.08, segment.size = 0.25,
    segment.colour = "#AAAAAA", box.padding = 0.25, point.padding = 0.35,
    max.overlaps = Inf
  ) +
  scale_x_anos_tese(anos = anos_disp, expand = expansion(add = c(1.0, 1.0))) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 6),
    labels = function(x) paste0("R$ ", format(x, big.mark = ",", trim = TRUE))
  ) +
  coord_cartesian(ylim = c(y_min * 0.88, y_top * 1.10), clip = "off") +
  labs(x = NULL, y = "Median real per-capita household income") +
  theme(
    axis.title.y       = element_text(size = 8.5),
    axis.text          = element_text(size = 8),
    panel.grid.major.x = element_blank(),
    plot.margin        = margin(0, 8, 4, 4)
  )

escala_x_gov <- scale_x_anos_tese(
  anos = anos_disp,
  expand = expansion(add = c(1.0, 1.0))
)
p_banner <- banner_governos(
  fim_dados = max_ano, ini_dados = min(anos_disp),
  scale_x = escala_x_gov
)
p <- p_banner / p + patchwork::plot_layout(heights = c(1, 11))

ALTURA_FIG <- 3.90
salvar_grafico(p,
  prefixo = "242_Tese_Renda_Mediana_Domiciliar",
  largura = LARGURA_TEXTO, altura = ALTURA_FIG, formato = "png"
)
salvar_grafico(p,
  prefixo = "242_Tese_Renda_Mediana_Domiciliar",
  largura = LARGURA_TEXTO, altura = ALTURA_FIG, formato = "pdf"
)

# ==============================================================================
# 4. DIAGNOSTICO DA EMENDA — agora com a unidade homogenea nos dois lados
# ==============================================================================
emenda <- df %>%
  filter(fonte == "PNAD Anual", ano %in% 2012:2015) %>%
  select(ano, pnad_mediana = mediana, pnad_media = media) %>%
  inner_join(
    serie$overlap_pnadc %>% select(ano, pnadc_mediana = mediana, pnadc_media = media),
    by = "ano"
  ) %>%
  mutate(
    dif_mediana_pct = 100 * (pnadc_mediana / pnad_mediana - 1),
    dif_media_pct = 100 * (pnadc_media / pnad_media - 1)
  )
cat("\nDEGRAU DA EMENDA (sobreposicao 2012-2015, unidade domiciliar nos dois lados):\n")
print(as.data.frame(emenda %>% mutate(across(where(is.numeric), ~ round(.x, 1)))),
      row.names = FALSE)
cat(sprintf("  Media dos 4 anos — mediana: %+.1f%% | media: %+.1f%%\n",
            mean(emenda$dif_mediana_pct), mean(emenda$dif_media_pct)))
cat("  (Souza & Hecksher medem -2,3% na mediana no mesmo teste; o fator de\n")
cat("   encaixe por estatistica que eles aplicam em 2012 e 0,9619.)\n")

# ==============================================================================
# 4b. VALIDACAO CONTRA SOUZA & HECKSHER (pacote de replicacao versionado)
#     A serie deles esta em precos MEDIOS de 2024; convertida aqui para
#     jan/2024 pelo fator acima, para a comparacao ser numero a numero.
# ==============================================================================
# ODA: o pacote de replicacao de Souza & Hecksher nao e redistribuido aqui; se
# voce o tiver, coloque results/renda.csv em data-raw/Souza-Hecksher/ e a
# comparacao ano a ano e impressa.
SH_RENDA <- here::here("data-raw", "Souza-Hecksher", "renda.csv")
if (file.exists(SH_RENDA)) {
  sh <- read.csv(SH_RENDA, fileEncoding = "UTF-8-BOM") %>%
    filter(fonte == "microdados", renda == "r_tot_hab", survey == "PNAD+PNADC") %>%
    transmute(ano, sh_mediana = p50 / FATOR_BASE_SH, sh_media = media / FATOR_BASE_SH)
  val <- df %>%
    inner_join(sh, by = "ano") %>%
    mutate(dif_mediana = 100 * (mediana / sh_mediana - 1),
           dif_media = 100 * (media / sh_media - 1))
  cat("\nVALIDACAO vs Souza & Hecksher (NT 120), mesma base de precos:\n")
  print(as.data.frame(val %>% transmute(ano,
    nossa_mediana = round(mediana, 0), sh_mediana = round(sh_mediana, 0),
    dif_mediana = round(dif_mediana, 1), dif_media = round(dif_media, 1))),
    row.names = FALSE)
  cat(sprintf("  Desvio medio — mediana: %+.1f%% | media: %+.1f%%\n",
              mean(val$dif_mediana), mean(val$dif_media)))
  cat(sprintf("  So PNAD (<=2015): mediana %+.1f%% | so PNADC (>=2016, sem 2022): %+.1f%%\n",
              mean(val$dif_mediana[val$ano <= 2015]),
              mean(val$dif_mediana[val$ano >= 2016 & val$ano != 2022])))
} else {
  cat("\n(pacote de replicacao de Souza & Hecksher ausente — validacao pulada)\n")
}

cat(sprintf("\nPara comparar com a NT 120 (precos medios de 2024), multiplicar por %.5f:\n",
            FATOR_BASE_SH))
print(as.data.frame(df %>% filter(ano %in% c(1995, 2003, 2014, 2019, 2024)) %>%
  transmute(ano, jan2024 = round(mediana, 0),
            medio2024 = round(mediana * FATOR_BASE_SH, 0))), row.names = FALSE)

# ==============================================================================
# 5. PROMOCAO (so a pedido do autor)
# ==============================================================================
if (PROMOVER) {
  finalizar_figura(
    plot = p,
    fig_label = FIG_LABEL,
    fig_cap = paste0(
      "Median real per-capita household income, Brazil, ",
      ANO_INICIO, "\u2013", max_ano, "."
    ),
    fonte = paste0(
      "IBGE --- PNAD (", ANO_INICIO, "--2015) and PNAD ",
      "Cont\\'{\\i}nua (2016--", max_ano, "), own harmonization"
    ),
    # Texto do autor (capitulo 0202 PT, 2026-09-16): sem a comparacao
    # mediana/media e sem a citacao a Souza-Hecksher, que fica so no apendice.
    nota = paste0(
      "Weighted median of real per-capita household income for the ",
      "population as a whole. Deflated by the IPCA to January 2024 prices. ",
      "Income is measured per capita within the household throughout, and ",
      "households reporting no income are retained, so the series follows ",
      "the convention of the official statistics. The line is interrupted in ",
      "years without a survey (1994, 2000 and 2010); the hollow markers for ",
      "2020 and 2021 come from the fifth visit of PNAD Cont\\'{\\i}nua, ",
      "collected mostly by telephone, which under-enumerates income among the ",
      "poorest. The solid vertical rule marks the change from PNAD to PNAD ",
      "Cont\\'{\\i}nua; the step at that point is measured in the appendix ",
      "and is not rescaled away."
    ),
    apendice = "sec-fignote-renda-mediana-domiciliar-pc",
    script_path = SCRIPT_PATH,
    largura = LARGURA_TEXTO, altura = ALTURA_FIG, unidades = "in"
  )
} else {
  cat("\nPROMOVER = FALSE: rascunho em graphs/.\n")
}

cat("\n===== 242_Tese_Renda_Mediana_Domiciliar concluido =====\n")
