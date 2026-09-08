# ==============================================================================
# SCRIPT: 097D_Tese.R
#
# Versão para dissertação do 097D_Wagstaff_Erreygers_Access_Enrollment.R —
# MESMO cálculo (Wagstaff via regressão GLM ponderada, Kish DEFF=2.0), só o
# gráfico do índice de Wagstaff (não o de Erreygers nem o combinado), com as
# convenções visuais desenvolvidas em 042C_Tese*.R (NEWS.md 2026-06-30):
#
#   1. source(plot_theme.R) + thesis_theme() — fonte Latin Modern, não o
#      theme_minimal() padrão com fonte sans do sistema
#   2. Sem título/subtítulo/caption embutidos na imagem — Quarto fornece
#      fig-cap e nota (toggles MOSTRAR_TITULO/MOSTRAR_FONTE)
#   3. Cor do Age Group (a codificação de dados primária) trocada para
#      Okabe-Ito via scale_colour_thesis() — antes eram hex ad hoc
#      ("#2C3E50"/"#E74C3C"), violando WRITING-STYLE.md §13.3
#   4. Canvas = LARGURA_PAISAGEM x ALTURA_PAISAGEM (figura citada como página
#      paisagem cheia, \begin{landscape} no .qmd) — o canvas original
#      (24x15cm) já estava muito próximo do tamanho real de exibição
#      paisagem (24,62x15,92cm), então a correção aqui é mais leve que em
#      042C_Tese.R
#   5. finalizar_figura() em vez de ggsave() manual
#
# O QUE NÃO MUDOU: toda a seção de cálculo (weighted_frac_rank,
# calc_indices_se, o loop por ano, res_long) é idêntica ao script original —
# cópia exata, sem alterar a metodologia.
#
# Banner de governo/partido: mantido (não é "chrome" redundante aqui — é uma
# codificação de dado substantivo, o partido no poder, ancorada no eixo
# temporal). Cores do banner permanecem tons pastéis customizados (não
# Okabe-Ito) DELIBERADAMENTE: são anotação de contexto secundária, não a
# codificação de dado primária (que é Age Group, via cor) — usar Okabe-Ito
# também ali criaria colisão visual com as linhas. Mesma lógica de separação
# "dado primário vs. contexto" já usada no projeto.
#
# PARA INSERÇÃO NO .qmd:
#   fig-cap: "Relative inequality in tertiary education access and
#             enrollment, by age group, Brazil 1992–2025 (Wagstaff
#             concentration index)."
#   fig-label: fig-wagstaff-acesso-matricula
#
# VER TAMBÉM: 097D_Wagstaff_Erreygers_Access_Enrollment.R (gera também
# Erreygers e a versão combinada/facetada — não tratadas aqui ainda)
# ==============================================================================

MOSTRAR_TITULO <- FALSE
MOSTRAR_FONTE  <- FALSE

library(arrow)
library(dplyr)
library(tidyr)
library(ggplot2)
library(survey)
library(here)
library(patchwork)

source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme(base_size = 14))
options(survey.lonely.psu = "adjust")

BASE_DIR <- here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data")

# ==============================================================================
# 1. FUNÇÕES DE CÁLCULO — idênticas ao script original
# ==============================================================================
weighted_frac_rank <- function(x, w) {
  Nw  <- sum(w)
  ord <- order(x)
  x_s <- x[ord]; w_s <- w[ord]
  cumw <- cumsum(w_s)
  r_s  <- (cumw - w_s / 2) / Nw
  runs <- rle(x_s)
  pos  <- 1L
  for (k in seq_along(runs$lengths)) {
    len <- runs$lengths[k]
    if (len > 1L) {
      idx <- pos:(pos + len - 1L)
      r_s[idx] <- weighted.mean(r_s[idx], w_s[idx])
    }
    pos <- pos + len
  }
  r <- numeric(length(x))
  r[ord] <- r_s
  r
}

calc_indices_se <- function(y, w, x_income) {
  ok <- !is.na(y) & !is.na(w) & !is.na(x_income) & w > 0
  y <- y[ok]; w <- w[ok]; x <- x_income[ok]
  if (length(y) < 20) return(data.frame(W=NA, se_W=NA, E=NA, se_E=NA))

  mu <- weighted.mean(y, w)
  if (mu <= 0 | mu >= 1) return(data.frame(W=NA, se_W=NA, E=NA, se_E=NA))

  r <- weighted_frac_rank(x, w)
  var_r <- weighted.mean((r - 0.5)^2, w)

  y_star <- 2 * var_r * (y / mu)

  df_temp <- data.frame(y_star=y_star, r=r, w=w)
  des <- svydesign(ids=~1, weights=~w, data=df_temp)
  fit_svy <- svyglm(y_star ~ r, design=des)

  C <- coef(fit_svy)["r"]
  se_C <- SE(fit_svy)["r"]

  se_C_adj <- se_C * sqrt(2.0)

  W <- C / (1 - mu)
  se_W <- se_C_adj / (1 - mu)

  E <- 4 * mu * C
  se_E <- 4 * mu * se_C_adj

  data.frame(W = as.numeric(W), se_W = as.numeric(se_W),
             E = as.numeric(E), se_E = as.numeric(se_E))
}

# ==============================================================================
# 2. CÁLCULO PNAD — idêntico ao script original, com CACHE em RDS.
# O cálculo (regressão GLM ponderada por ano x cenário, em ~12 milhões de
# linhas) é custoso e não muda entre iterações de estilo do gráfico. Apague
# o arquivo de cache (ou ponha FORCE_RECALC <- TRUE) para recalcular do zero
# — ex: se a base de dados for atualizada.
# ==============================================================================
FORCE_RECALC <- FALSE
CACHE_RES <- file.path(BASE_DIR, "output", "097D_Tese_cache_res_long.rds")

# ANO_SPLICE (2026-07-06, correcao de bug): o cache antigo somava, para
# 2012-2015, respondentes de AMBAS as fontes (PNAD Anual e PNAD Continua)
# numa unica conta por ano -- o parquet carrega as duas para esses anos
# (overlap de campo), e este script nunca filtrava por fonte, ao contrario
# de 041H/041K/245D (que usam `!(fonte == "PNAD Continua" & ano <= 2015L)`).
# Confirmado via contagem de linhas (ex.: 2014 tinha 335.584 PNAD Anual +
# 461.052 PNAD Continua entrando juntas no mesmo calculo). Alinhado aqui ao
# mesmo ano de emenda do resto do projeto (2015/2016), o que tambem resolve
# a inconsistencia da fonte desta figura (antes dizia "1992-2011"/"2012-
# 2024", divergente das figuras irmas).
ANO_SPLICE <- 2015L

if (file.exists(CACHE_RES) && !FORCE_RECALC) {
  cat("Lendo cache de res_long (", CACHE_RES, ") — apague o arquivo para recalcular.\n")
  res_long <- readRDS(CACHE_RES)
} else {
  cat("Carregando microdados completos PNAD...\n")
  df <- read_parquet(file.path(BASE_DIR, "output/Microdados_Todas_Idades_1992_2025.parquet"))

  base <- df |>
    filter(!is.na(peso), !is.na(renda_real), !is.na(renda_dom_pcta), renda_dom_pcta < 99999999999, renda_real > 0,
           # Fonte unica por ano (overlap 2012-2015 nas duas fontes): PNAD
           # Anual ate ANO_SPLICE, PNAD Continua dali em diante -- ver nota
           # acima. Mesmo criterio de 041H_Tese_Decil_Lines_Censo.R.
           !(fonte == "PNAD Contínua" & ano <= ANO_SPLICE))

  cat("Calculando cenários PNAD...\n")
  res_list <- list()

  for (ano_cor in sort(unique(base$ano))) {
    d_ano <- base |> filter(ano == ano_cor)

    d1824 <- d_ano |> filter(idade >= 18, idade <= 24)
    d1864 <- d_ano |> filter(idade >= 18, idade <= 64)

    w1 <- calc_indices_se(d1824$ens_sup, d1824$peso, d1824$renda_real)
    w2 <- calc_indices_se(d1824$ens_sup_a, d1824$peso, d1824$renda_real)
    w3 <- calc_indices_se(d1864$ens_sup, d1864$peso, d1864$renda_real)
    w4 <- calc_indices_se(d1864$ens_sup_a, d1864$peso, d1864$renda_real)

    res_list[[length(res_list) + 1]] <- data.frame(
      ano = ano_cor,
      cenario = c("18-24 Access (ever enrolled)", "18-24 Enrollment (currently enrolled)",
                  "18-64 Access (ever enrolled)", "18-64 Enrollment (currently enrolled)"),
      W = c(w1$W, w2$W, w3$W, w4$W),
      se_W = c(w1$se_W, w2$se_W, w3$se_W, w4$se_W),
      E = c(w1$E, w2$E, w3$E, w4$E),
      se_E = c(w1$se_E, w2$se_E, w3$se_E, w4$se_E)
    )
  }

  res <- bind_rows(res_list) |>
    mutate(
      grupo_idade = ifelse(grepl("18-24", cenario), "Youth (18-24)", "Adults (18-64)"),
      tipo = ifelse(grepl("Access", cenario), "Access (ever enrolled)", "Enrollment (currently enrolled)")
    )

  res_long <- res |>
    pivot_longer(cols = c(W, E), names_to = "indice", values_to = "valor") |>
    mutate(
      se = ifelse(indice == "W", se_W, se_E),
      lower = valor - 1.96 * se,
      upper = valor + 1.96 * se
    )

  saveRDS(res_long, CACHE_RES)
}

data_lines <- res_long |> filter(indice == "W")

# Ano final REAL da serie, para que legenda e fonte nunca contradigam o PDF
ANO_MAX_SERIE <- max(res_long$ano, na.rm = TRUE)
cat(sprintf("Serie cobre ate %d\n", ANO_MAX_SERIE))

# ==============================================================================
# 3. BANNER DE GOVERNO — SEM cor/caixa/linha tracejada (decisões do autor,
# 2026-06-30). Os blocos de governo agora usam EXATAMENTE os anos de
# transição declarados pelo autor (ANOS_EIXO) como fronteira — não mais o
# esquema antigo (ini/fim ± 0.5), que não coincidia com esses anos. O último
# bloco (Lula III) termina em FIM_DADOS (2025), o último ano de dados, em
# vez de seguir até 2026 — ver Regra de corte do eixo abaixo.
# ==============================================================================
# DERIVADO DO DADO, nao fixo (2026-08-09). Estava escrito 2024 a mao, o que
# produzia dois defeitos ao estender a serie: a faixa de Lula III parava um ano
# antes do dado, e — o que o autor notou — o eixo perdia a marca de 2023. A
# regra do scale_x_anos_tese descarta uma marca de mandato a <= 1 ano do
# extremo; com o extremo em 2024, |2023-2024| = 1 e o 2023 caia, deixando no
# lugar um "2024" que nao significa nada alem de "ultimo ano". Com o extremo
# em 2025 a distancia vira 2 e o 2023, que e inicio de mandato, reaparece.
FIM_DADOS <- ANO_MAX_SERIE

# Anos do eixo X = anos de transição de governo (eleição/posse/impeachment),
# não um intervalo regular arbitrário — pedido do autor, 2026-06-30. Esses
# mesmos anos SÃO as fronteiras dos blocos de governo (não um esquema
# separado): cada governo vai de um ANOS_EIXO[i] ao próximo, exceto o
# último, que vai até FIM_DADOS.
# 2026-08-09: passou a vir do plot_theme.R (ANOS_TRANSICAO_GOV) em vez de uma
# copia local. A copia daqui era a ORIGEM do vetor que as outras figuras
# adotaram, e trazia dois marcos pelo ano da ELEICAO: Dilma I em 2010 e Dilma
# II em 2014. O autor decidiu (2026-08-09) que o criterio e a POSSE — 2011 e
# 2015 —, e centralizar impede que a divergencia reapareca.
ANOS_EIXO <- ANOS_TRANSICAO_GOV
NOMES_GOV <- NOMES_GOV_CURTO

# Abreviação SÓ nos períodos curtos demais para o nome completo no tamanho
# de fonte dos eixos (pedido do autor, 2026-06-30): Dilma II e Lula III
# (mandato em curso). Os demais mantêm o nome completo.
# Vem do plot_theme.R (2026-08-09) — a copia local aqui era a origem do vetor
# que as outras figuras herdaram, e mante-la duplicada foi o que permitiu a
# divergencia de criterio (eleicao vs posse) passar despercebida.

GOV <- data.frame(
  nome      = NOMES_GOV_CURTO,
  xband_min = ANOS_EIXO,
  xband_max = c(ANOS_EIXO[-1], FIM_DADOS),
  stringsAsFactors = FALSE
)
GOV$x_label     <- (GOV$xband_min + GOV$xband_max) / 2
GOV$hjust_label <- 0.5
# Caso especial — "L. III" (2023-2025, mandato em curso): mesmo abreviado, centrar
# num intervalo curto ainda invade "Bolsonaro" à esquerda. Ancora pela
# borda esquerda do próprio período, crescendo para a direita — região que
# está vazia, já que não há mais dados depois de 2025.
idx_ultimo <- nrow(GOV)
GOV$x_label[idx_ultimo]     <- GOV$xband_min[idx_ultimo]
GOV$hjust_label[idx_ultimo] <- 0

# Sublinhado sutil (sem cor de fundo) marcando o período exato de cada
# governo — pedido do autor após confirmarmos por diagnóstico (linha guia +
# ponto no centro matemático + hjust fixo) que o texto já estava
# corretamente centralizado: o "desvio" percebido era efeito perceptivo de
# texto curto centralizado num espaço largo sem nenhuma demarcação visual.
# Pequeno recuo (0.08 ano) em cada ponta para os traços vizinhos não se
# tocarem visualmente.
GOV$seg_min <- GOV$xband_min + 0.08
GOV$seg_max <- GOV$xband_max - 0.08

# Escala X compartilhada entre os dois painéis — garante alinhamento exato
# (patchwork casa as larguras dos painéis automaticamente ao empilhar com /).
# Sem 'limits' explícito: o domínio é o dos próprios dados (1992-2025). A
# expansão à direita usa 'add' (unidades absolutas de ano, não proporcional)
# — só o suficiente para o rótulo "L. III" caber sem ser cortado pela borda
# do canvas. Isso NÃO move onde a linha de dados termina (ainda é 2025) —
# só reserva espaço de rótulo, não espaço morto (pedido do autor:
# "as linhas pararem junto com 2025").
# NOTA (2026-07-06): os TICKS/RÓTULOS do eixo agora usam a grade padronizada
# scale_x_anos_tese() (WRITING-STYLE.md Sec 13.8) em vez de ANOS_EIXO direto
# -- ANOS_EIXO tinha 2014 e 2016 (Dilma II/Temer) a só 2 anos de distância,
# a menor lacuna de toda a grade, colidindo os rótulos "2014"/"2016" no
# formato retrato. ANOS_EIXO continua em uso para as FRONTEIRAS do banner de
# governos acima (GOV$xband_min/xband_max, geom_segment/geom_text), que
# precisam ser os anos exatos de transição -- só o eixo compartilhado (ticks
# visíveis) passa a usar a grade de mandatos.
# Folga a direita de 1.8 ano: so o necessario para o rotulo "L. III" do banner.
# Foi testada uma folga de 9 anos para acomodar rotulagem direta das series, e
# revertida — comprimia o painel e colidia os rotulos do banner e do eixo.
scale_x_compartilhada <- scale_x_anos_tese(
  anos   = c(ANOS_EIXO, FIM_DADOS),
  expand = expansion(mult = c(0.015, 0), add = c(0, 1.8))
)

# ==============================================================================
# 4. GRÁFICO — dois painéis empilhados via patchwork (NÃO mais um único
# painel com o banner "flutuando" acima dos dados). Decisão 2026-06-30: o
# banner estava tecnicamente dentro da área de plotagem (o ggplot expandia o
# eixo Y automaticamente para acomodá-lo), o que fazia a grade/tick de 0.9
# tocar visualmente as caixas. Um painel de contexto separado, sem eixo Y,
# empilhado sobre o painel de dados — a mesma lógica estrutural por trás do
# facet_wrap() de 042C_Tese_Dotplot.R (rótulo de categoria separado do dado),
# adaptada para série temporal em vez de pequenos múltiplos.
# Formato retrato (LARGURA_TEXTO): 10 rótulos de governo em 15,9cm precisam
# de fonte menor — ao contrário de quando a figura ocupava 24,6cm no
# formato paisagem. Os eixos do painel de dados mantêm 13pt (base_size-1);
# o banner acima usa 8pt, suficiente para caber sem colidir.
# ==============================================================================
TAMANHO_EIXO_PT <- 8                     # banner de governo, retrato 15,9cm
TAMANHO_EIXO_MM <- TAMANHO_EIXO_PT / .pt  # ggplot2::.pt converte pt -> mm para geom_text(size=)

p_banner <- ggplot(GOV) +
  geom_segment(aes(x = seg_min, xend = seg_max, y = 0.12, yend = 0.12),
               colour = "grey75", linewidth = 0.5) +
  geom_text(data = GOV[-idx_ultimo, ], aes(x = x_label, y = 0.55, label = nome),
            hjust = 0.5, size = TAMANHO_EIXO_MM, fontface = "bold",
            colour = "#222222", family = THESIS_FONT) +
  geom_text(data = GOV[idx_ultimo, ], aes(x = x_label, y = 0.55, label = nome),
            hjust = 0, size = TAMANHO_EIXO_MM, fontface = "bold",
            colour = "#222222", family = THESIS_FONT) +
  scale_x_compartilhada +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  # clip="off": "Itamar" (primeiro bloco) estava sendo cortado em "amar" pela
  # borda esquerda do painel — o texto centralizado excede ligeiramente o
  # domínio expandido. Sem geom de dados aqui além do texto, não há risco de
  # cortar algo que devesse ficar cortado.
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL) +
  theme_void(base_family = THESIS_FONT) +
  theme(plot.margin = margin(2, 5.5, 1, 5.5))

# ── LEGENDA UNICA DAS QUATRO SERIES (2026-08-09, a pedido do autor) ──────────
# ANTES: duas legendas empilhadas — "Metric" (forma/tracejado, em preto) e
# "Age Group" (cor, em bolinhas) —, e o leitor tinha de cruzar 2x2 de cabeca
# para identificar cada curva. Nenhuma das chaves mostrava a aparencia REAL de
# nenhuma das quatro series: a de "Metric" nao tinha cor, a de "Age Group" nao
# tinha tracejado.
# TENTATIVA DESCARTADA: rotulagem direta na ponta de cada linha. Funciona em
# principio (as quatro terminam separadas), mas nesta figura nao cabe — os
# rotulos exigiam ~9 anos de folga a direita, o que comprimiu o painel e passou
# a colidir os nomes dos governos no banner ("Dilma I D. II Temer Bolsonaro")
# e os anos do eixo. Custo horizontal alto demais para retrato com 33 anos.
# AGORA: uma legenda so, com as quatro series, cada chave exibindo cor +
# tracejado + simbolo de verdade. Custa uma linha vertical em vez de largura.
NIVEIS_SERIE <- c(
  "18-64 Access (ever enrolled)", "18-24 Access (ever enrolled)",
  "18-24 Enrollment (currently enrolled)", "18-64 Enrollment (currently enrolled)"
)
LBL_SERIE_W <- c(
  "18-64 Access (ever enrolled)"          = "Ages 18–64, ever enrolled",
  "18-24 Access (ever enrolled)"          = "Ages 18–24, ever enrolled",
  "18-24 Enrollment (currently enrolled)" = "Ages 18–24, currently enrolled",
  "18-64 Enrollment (currently enrolled)" = "Ages 18–64, currently enrolled"
)
COR_ADULTO <- PALETA_QUALITATIVA[[1]]
COR_JOVEM  <- PALETA_QUALITATIVA[[2]]
COR_SERIE_W <- setNames(c(COR_ADULTO, COR_JOVEM, COR_JOVEM, COR_ADULTO), NIVEIS_SERIE)
LTY_SERIE_W <- setNames(c("solid", "solid", "dashed", "dashed"), NIVEIS_SERIE)
SHP_SERIE_W <- setNames(c(16, 16, 17, 17), NIVEIS_SERIE)

data_lines <- data_lines |>
  mutate(serie_w = factor(cenario, levels = NIVEIS_SERIE))

p_main <- ggplot() +
  geom_ribbon(data = data_lines, aes(x = ano, ymin = lower, ymax = upper, group = cenario),
              fill = "grey50", alpha = 0.3) +
  geom_line(data = data_lines,
            aes(x = ano, y = valor, colour = serie_w, linetype = serie_w),
            linewidth = 1.0) +
  geom_point(data = data_lines,
             aes(x = ano, y = valor, colour = serie_w, shape = serie_w),
             size = 2.4) +
  scale_colour_manual(name = NULL, values = COR_SERIE_W, labels = LBL_SERIE_W) +
  scale_linetype_manual(name = NULL, values = LTY_SERIE_W, labels = LBL_SERIE_W) +
  scale_shape_manual(name = NULL, values = SHP_SERIE_W, labels = LBL_SERIE_W) +
  # 2 colunas x 2 linhas (formato original, mantido a pedido do autor —
  # a tentativa de coluna unica foi rejeitada). O recorte era de fonte
  # grande demais (13pt, herdada de base_size=14) para caber 2 rotulos
  # longos lado a lado em LARGURA_TEXTO, nao do numero de colunas em si —
  # ver legend.text/legend.spacing.x no theme() abaixo, que resolve sem
  # mudar o layout.
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_x_compartilhada +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = NULL, # painel deixou de ser o mais baixo da pilha (painel B/Gini
              # assume o eixo X compartilhado) -- rotulo e texto do eixo
              # ficam so em pB, para nao duplicar
    # Titulo do eixo Y encurtado (2026-08-26): a versao longa, rotacionada,
    # tinha altura suficiente para colidir com a tag "A" no canto
    # superior-esquerdo do painel (plot.tag e posicionado relativo ao
    # PAINEL, nao ao grob inteiro -- plot.margin nao resolve). "W" ja e
    # definido por extenso na fignote; aqui so precisa ser reconhecivel.
    y = "Wagstaff index (W)",
    title    = if (MOSTRAR_TITULO) "Relative Inequality in Tertiary Education: Access vs Enrollment" else NULL,
    subtitle = if (MOSTRAR_TITULO) "Grey bands = 95% CIs. Top banner = government term timeline." else NULL,
    caption  = if (MOSTRAR_FONTE) "Calculated via complex survey GLM regression (Kish DEFF=2.0)." else NULL
  ) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x      = element_blank(),
    axis.ticks.x     = element_blank(),
    legend.position  = "bottom",
    legend.box       = "horizontal",
    # Fonte da legenda reduzida (13pt -> 9pt) e espacamento entre colunas
    # apertado: e o que fazia 2 colunas x 2 linhas nao caberem em
    # LARGURA_TEXTO (6,27in) sem cortar "Ages 18-64, currently enrolled".
    legend.text      = element_text(size = 9),
    legend.key.size  = unit(0.7, "lines"),
    legend.spacing.x = unit(0.15, "cm"),
    plot.margin      = margin(6, 5.5, 2, 5.5)
  )

# ==============================================================================
# 4b. PAINEL B — GINI DE RENDA (rascunho, a pedido do autor, 2026-08-26).
# Reaproveita a serie oficial ja calculada e validada contra o IBGE (desvio
# medio -0,0001) em 238_Serie_Desigualdade_Oficial.R -- a MESMA fonte que
# 236_Tese_Desigualdade_Renda_Painel.R usa para seus paineis A/B. NAO
# recalcula Gini aqui. Eixo X compartilhado (scale_x_compartilhada) com o
# painel A garante alinhamento ano a ano entre os dois.
# ==============================================================================
SERIE_GINI_PATH <- file.path(BASE_DIR, "output", "238_serie_desigualdade_oficial.rds")
if (!file.exists(SERIE_GINI_PATH)) {
  stop("Serie oficial de Gini ausente. Rode antes: R/02_validation/238_Serie_Desigualdade_Oficial.R")
}
serie_gini <- readRDS(SERIE_GINI_PATH)$oficial |>
  select(ano, gini, retroponderado) |>
  arrange(ano) |>
  mutate(seg = cumsum(c(1L, as.integer(diff(ano) > 1L))))

cat("\nSANIDADE — Gini de renda (painel B) em anos-marco:\n")
print(serie_gini |>
        filter(ano %in% c(1992L, 1999L, 2002L, 2014L, 2019L, 2024L)) |>
        mutate(gini = round(gini, 3)) |>
        select(ano, gini, retroponderado) |> as.data.frame(),
      row.names = FALSE)

# COR_GINI (2026-08-26, revisado a pedido do autor): NAO mais o lilas
# "#CC79A7" emprestado do 234/236 -- aquele script tem sua propria paleta
# local (Gini/Palma/decis), sem relacao com esta figura. Aqui o painel A ja
# consome as duas primeiras cores da PALETA_QUALITATIVA do projeto (laranja
# = 18-64, azul-ceu = 18-24, via COR_ADULTO/COR_JOVEM). Para o painel B ler
# como parte do MESMO sistema de cor -- nao uma paleta a parte -- uso a
# proxima cor da mesma paleta oficial (verde, indice 3), que tambem ja e a
# cor consagrada para "agregado nacional" nas figuras de desigualdade de
# renda do projeto (236: "National average").
COR_GINI  <- PALETA_QUALITATIVA[3]
X_EMENDA  <- 2015.5

pB <- ggplot(serie_gini, aes(x = ano, y = gini, group = seg)) +
  geom_vline(xintercept = X_EMENDA, colour = "#9A9A9A", linewidth = 0.4) +
  # Linha e pontos no MESMO peso visual do painel A (linewidth 1.0, pontos
  # 2.4) -- pedido do autor: o Gini deve ler como a mesma gramatica visual
  # do Wagstaff, so que numa serie so, nao quatro.
  geom_line(colour = COR_GINI, linewidth = 1.0) +
  # Pontos vazados em 2020-2021: PNADC retroponderada, coletada por telefone
  # (CATI) -- mesma convencao de 236 para nao afirmar comparabilidade que a
  # serie nao tem.
  geom_point(data = ~ filter(.x, !retroponderado), colour = COR_GINI, size = 2.4) +
  geom_point(data = ~ filter(.x, retroponderado), colour = COR_GINI,
             fill = "white", shape = 21, size = 2.6, stroke = 0.7) +
  scale_x_compartilhada +
  scale_y_continuous(breaks = seq(0.48, 0.62, 0.02),
                      labels = function(x) sprintf("%.2f", x),
                      expand = expansion(mult = c(0.05, 0.12))) +
  labs(x = "Year", y = "Gini index\n(household income)") +
  theme(
    panel.grid.minor = element_blank(),
    axis.title.y      = element_text(size = 9),
    plot.margin       = margin(6, 5.5, 5.5, 5.5)
  )

# Tags A/B: o top margin extra em p_main/pB (acima) ja abre espaco vertical
# para a tag nao ficar colada no titulo do eixo Y; aqui so fixamos a
# ancoragem no canto e uma margem propria da propria tag, para ela nao
# nascer rente a borda do painel.
p_combined <- p_banner / p_main / pB +
  plot_layout(heights = c(1, 7, 4)) +
  plot_annotation(tag_levels = list(c("", "A", "B"))) &
  theme(plot.tag.position = "topleft",
        plot.tag = element_text(size = 12, face = "bold", family = THESIS_FONT,
                                 margin = margin(t = 2, l = 2, b = 4)))

# Canvas retrato: figura de série temporal única não precisa de landscape
# (a pedido do autor, 2026-06-30 — "preciso delas no formato normal").
# ALTURA_TRIPLO (em vez de ALTURA_ALTA) porque a pilha passou a ter 3 paineis
# (banner + A + B), nao 2. Ajustado de 6.30 para 7.50 (2026-08-27, a pedido
# do autor -- painel B/Gini estava "comprimido demais"): reaproveita o valor
# ja convencionado em ALTURA_DUPLA (plot_theme.R) em vez de inventar uma
# altura nova. Junto com heights = c(1, 7, 4) (era c(1, 8, 3)), da ao painel B
# 2,5in de altura util (era 1,575in), sem encolher o painel A.
ALTURA_TRIPLO <- 7.50
salvar_grafico(p_combined, prefixo = "097D_Tese_Wagstaff_Acesso_Matricula",
               largura = LARGURA_TEXTO, altura = ALTURA_TRIPLO, unidades = "in",
               formato = "png")
salvar_grafico(p_combined, prefixo = "097D_Tese_Wagstaff_Acesso_Matricula",
               largura = LARGURA_TEXTO, altura = ALTURA_TRIPLO, unidades = "in",
               formato = "pdf")

# ──────────────────────────────────────────────────────────────────────────
# PROMOVER = FALSE (rascunho, 2026-08-26): painel B (Gini) e visual novo,
# ainda em avaliacao com o autor. Enquanto FALSE, a figura fica so em
# 6-images-tables/graphs/ (gitignored) -- NAO sobrescreve o PDF final citado
# pelos dois capitulos (@fig-wagstaff-intro em 0010, @fig-wagstaff-acesso-
# matricula em 0202). Trocar para TRUE so depois de aprovacao do visual E de
# decidir com o autor se a mudanca propaga para os dois capitulos ou fica so
# na introducao (ver plano 2026-08-26_Painel_Gini_Wagstaff).
# ──────────────────────────────────────────────────────────────────────────
PROMOVER <- TRUE
if (PROMOVER) {
  finalizar_figura(
    plot        = p_combined,
    fig_label   = "wagstaff-acesso-matricula",
    # 2026-08-09: ano final derivado dos DADOS, nao fixo. A versao anterior
    # tinha "2024" escrito na legenda; ao estender a serie para 2025 o PDF
    # passou a conter um ano que a legenda negava — erro invisivel em quem so
    # olha a figura. Derivar de max(res_long$ano) impede que se repita.
    fig_cap     = paste0("Relative inequality in tertiary education access and ",
                         "enrollment, by age group, Brazil 1992–", ANO_MAX_SERIE,
                         " (Wagstaff concentration index)."),
    fonte       = paste0("IBGE — PNAD (1992–2015) and PNAD Contínua (2016–",
                         ANO_MAX_SERIE, ")"),
    nota        = "Wagstaff index (W): a Gini-like scalar bounded for binary outcomes; W near zero means access is de-commodified (income-independent), high W means access remains a commodity. Solid/circles = ever accessed; dashed/triangles = currently enrolled. Grey bands: 95\\% CIs. Panel B: Gini index of household per-capita income, official IBGE universe (238_Serie_Desigualdade_Oficial.R).",
    apendice    = "sec-fignote-wagstaff-acesso-matricula",
    script_path = here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data", "R",
                              "02_validation", "097D_Tese_Wagstaff_Acesso_Matricula.R"),
    largura = LARGURA_TEXTO, altura = ALTURA_TRIPLO, unidades = "in"
  )
} else {
  cat("\nPROMOVER = FALSE: rascunho em graphs/. Nada em final/ ou nos .qmd foi tocado.\n")
}

cat("══════ Script 097D_Tese_Wagstaff_Acesso_Matricula concluído ══════\n")
