# 222_Tese_Intro_Composicao_Setor_Privado
# ==============================================================================
# VARIANTE da figura 220 com a COMPOSIÇÃO INTERNA DO SETOR PRIVADO.
#
# Painel A: matrículas por setor (idêntico ao da 220).
# Painel B (REDESENHADO 2026-08-27, a pedido do autor — rascunho em
#           222E_Tese_Expansao_Painel_B_Indexado_DRAFT.R): deixou de mostrar
#           composição do setor privado (% do privado) e passou a mostrar
#           CRESCIMENTO INDEXADO (1980 = 100) de Total/Público/Privado. Razão:
#           no Painel A, a expansão da década de 1990 vira uma tira fina perto
#           do eixo x (a escala vai de ~1,6M em 1994 a >10M em 2024); indexado
#           ao próprio ano-base de cada série, cada década ocupa a mesma faixa
#           vertical proporcional, tornando o ritmo de crescimento dos anos 90
#           tão legível quanto o dos anos 2000-2010. A composição do setor
#           privado (Particular/for-profit/EaD, dois blocos não-emendáveis)
#           NÃO aparece mais nesta figura — ver histórico abaixo.
#
# HISTÓRICO (design anterior do Painel B, até 2026-08-27): composição do
# setor privado em DOIS BLOCOS que o INEP não permite emendar:
#             1999–2009 · Particular      vs. Comunitária/Confessional/Filantrópica
#             2010–2024 · Com fins lucr.  vs. Sem fins lucrativos
#
# 🚨 POR QUE OS DOIS BLOCOS NÃO VIRAM UMA LINHA SÓ:
#   São taxonomias diferentes, não a mesma variável renomeada. "Particular"
#   significava "privada NÃO comunitária/confessional/filantrópica" — um
#   guarda-chuva que incluía muita instituição juridicamente SEM fins
#   lucrativos. "Com fins lucrativos" (TP_CATEGORIA_ADMINISTRATIVA = 4, a
#   partir de 2010) é uma categoria jurídica estrita. Emendá-las produziria um
#   degrau de 77% (2009, Particular) para 43% (2010, com fins lucrativos) que
#   NÃO é um evento do mundo — é a troca de régua. Daí o vão branco entre os
#   blocos e os dois rótulos separados.
#
#   Substantivamente, o contraste entre as duas réguas é ele próprio o achado:
#   o setor privado já era largamente NÃO-filantrópico em 2009 (77% particular)
#   antes de se tornar majoritariamente for-profit no sentido jurídico — a
#   conversão dos anos 2010 foi de estatuto legal, sobre uma base que já era
#   comercial na prática.
#
# ⚠️ OUTRAS RESSALVAS:
#   • 1999 usa os agregados (todas as modalidades); 2000–2009, a base
#     PRESENCIAL (o CENSUP só separa EAD por categoria a partir de 2008).
#   • 2002 não possui Tabela 5.1 publicada pelo INEP; a linha do geom_line() realiza interpolação visual contínua entre 2001 e 2003 no Painel B.
#   • O salto de 64%→77% em 2009 coincide com a troca de tabela agregada para
#     microdados; parte dele pode ser reclassificação, não movimento real.
#   • 2012–2024: a categoria "Especial" (≤1,6% do privado) fica de fora; as
#     participações são calculadas DENTRO do par com/sem fins lucrativos.
#   • Em 1995–1998 os campos `Private_Particular_*` do v6 somam exatamente o
#     TOTAL privado: ali "Particular" desagrega o setor inteiro por tipo de
#     organização, não é a subcategoria. NÃO usar aqueles anos aqui.
#
# FONTE: data_tertiary_v6_clean.xlsx (educabr2).
# Fase 1 (desenvolvimento): salva em 6-images-tables/graphs/ (gitignored).
# ==============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)

source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())

CAMINHO_V6 <- file.path(
  dirname(here::here()), "educabr2",
  "data-raw", "sources", "tertiary_multisource", "data_tertiary_v6_clean.xlsx"
)
stopifnot(file.exists(CAMINHO_V6))

source_priority <- function(src) {
  case_when(
    str_starts(src, "CENSUP") ~ 1,
    str_starts(src, "Sinopse_CENSUP") ~ 2,
    str_starts(src, "educacao") ~ 3,
    str_detect(src, "Kang-Paese-Felix") ~ 4,
    str_detect(src, "MaduroJunior") ~ 5,
    str_detect(src, "Durham") ~ 6,
    TRUE ~ 7
  )
}

SERIES <- c(
  "Total", "Public", "Private", "Total_Presencial", "Private_Presencial",
  "Total_EAD",
  "Private_For_Profit", "Private_Non_Profit",
  "Private_Particular", "Private_Particular_Presencial",
  "Private_Community_Confessional_Philanthropic",
  "Private_Community_Confessional_Philanthropic_Presencial"
)

wide <- read_excel(CAMINHO_V6) |>
  filter(enrollment_type %in% SERIES, year_key >= 1980) |>
  mutate(src_p = source_priority(data_source)) |>
  arrange(src_p) |>
  distinct(year_key, enrollment_type, .keep_all = TRUE) |>
  select(year_key, enrollment_type, numbahs) |>
  pivot_wider(names_from = enrollment_type, values_from = numbahs) |>
  arrange(year_key) |>
  complete(year_key = full_seq(year_key, 1)) |>
  mutate(
    Total = if_else(is.na(Total), Total_Presencial, Total),
    prop_priv_presencial = Private_Presencial / Total_Presencial,
    Private = if_else(is.na(Private) & !is.na(prop_priv_presencial),
      Total * prop_priv_presencial, Private
    ),
    Public = if_else(is.na(Public), Total - Private, Public)
  )

stopifnot(all(abs(wide$Public + wide$Private - wide$Total) < 1, na.rm = TRUE))

# ==============================================================================
# PAINEL A — NÍVEIS (idêntico à 220)
# ==============================================================================

COR_PUBLICO <- PALETA_QUALITATIVA[4]
COR_PRIVADO <- PALETA_QUALITATIVA[1]

dados_area <- wide |>
  select(year_key, Public, Private) |>
  pivot_longer(-year_key, names_to = "setor", values_to = "matriculas") |>
  filter(!is.na(matriculas)) |>
  mutate(
    setor = factor(setor, levels = c("Private", "Public")),
    milhoes = matriculas / 1e6
  )

rotulo_final <- wide |> filter(year_key == max(year_key))

# Rótulos DENTRO das faixas coloridas (decisão do autor, 2026-08-03). A versão
# anterior os punha à direita do painel, o que custava 16% da largura em
# expansão do eixo mais 30pt de margem — cerca de um quinto da figura gasto com
# legenda. Texto branco sobre o preenchimento: as duas faixas são escuras o
# bastante para carregá-lo, e a faixa pública, ainda que fina, comporta uma
# linha de texto a partir dos anos 2000.
# 2024 removido (2026-08-27, a pedido do autor): "2023" e "2024" adjacentes
# no fim da grade colidiam visualmente -- fora do padrao do projeto. O dado
# ainda vai ate 2024 (nao mudou o dominio, so o rotulo do eixo).
ANOS_EIXO_FIG2 <- c(1980, 1984, 1988, 1992, 1995, 1999, 2003, 2007, 2011, 2015, 2019, 2023)

# ESCALA X UNICA (2026-08-27): mesmo objeto scale_x_continuous() -- mesmos
# breaks/limits/expand -- nos dois paineis, para garantir alinhamento exato
# dos anos entre Painel A e Painel B empilhados (mesma logica do
# `scale_x_compartilhada` de 097D_Tese_Wagstaff_Acesso_Matricula.R).
ESCALA_X_COMPARTILHADA <- scale_x_continuous(
  breaks = ANOS_EIXO_FIG2,
  labels = as.character(ANOS_EIXO_FIG2),
  limits = range(dados_area$year_key),
  expand = expansion(mult = c(0.01, 0.02))
)

p_niveis <- ggplot(dados_area, aes(year_key, milhoes, fill = setor)) +
  geom_area(colour = "white", linewidth = 0.15) +
  scale_fill_manual(values = c(Private = COR_PRIVADO, Public = COR_PUBLICO)) +
  ESCALA_X_COMPARTILHADA +
  scale_y_continuous(
    breaks = seq(0, 10, 2), labels = function(x) sprintf("%.0f", x),
    expand = expansion(mult = c(0, 0.06))
  ) +
  # Ano do rotulo explicito no proprio texto (2026-08-27, a pedido do autor):
  # sem isso, "80%"/"20%" parecem valores gerais da figura, nao especificos
  # do ultimo ano -- usa rotulo_final$year_key (nao 2024 fixo) para nao ficar
  # desatualizado se a serie for estendida.
  annotate("text",
    x = 2023, y = 4.5,
    label = sprintf(
      "Private — %.1fm (%.0f%%, %d)", rotulo_final$Private / 1e6,
      100 * rotulo_final$Private / rotulo_final$Total, rotulo_final$year_key
    ),
    hjust = 1, size = 2.7, colour = "white", fontface = "bold"
  ) +
  # Alinhado com o rotulo do Privado (2026-08-27, a pedido do autor): mesmo
  # x=2023 e hjust=1 do rotulo acima, em vez de x=2012/hjust=0.5 -- os dois
  # rotulos formam uma coluna com a mesma borda direita. A faixa publica em
  # 2023 (~2,0-2,1 milhoes) ja tem altura suficiente pra uma linha de texto.
  annotate("text",
    x = 2023, y = 0.9,
    label = sprintf(
      "Public — %.1fm (%.0f%%, %d)", rotulo_final$Public / 1e6,
      100 * rotulo_final$Public / rotulo_final$Total, rotulo_final$year_key
    ),
    hjust = 1, size = 2.7, colour = "white", fontface = "bold"
  ) +
  coord_cartesian(clip = "off") +
  labs(
    subtitle = "A. Undergraduate enrolment, by sector (millions)",
    x = NULL, y = NULL
  ) +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(face = "bold", size = 9, hjust = 0)
  )

# ==============================================================================
# PAINEL B — CRESCIMENTO INDEXADO (1980 = 100), Total/Público/Privado
# (REDESENHADO 2026-08-27 — ver histórico da composição do setor privado no
# cabeçalho do arquivo e no script 222E_..._DRAFT.R que originou este design).
#
# Cada série indexada ao seu PRÓPRIO primeiro ano disponível (as três têm
# dado desde 1980, então na prática é 1980 = 100 para as três). Escala linear
# — sem EaD/for-profit, a faixa de valores (100-930) não tem salto de ordem
# de grandeza que justificasse log.
# ==============================================================================

indexar_desde_inicio <- function(anos, valores) {
  ok <- !is.na(valores)
  anos <- anos[ok]; valores <- valores[ok]
  ano_base <- min(anos)
  valor_base <- valores[which.min(anos)]
  tibble::tibble(year_key = anos, indice = 100 * valores / valor_base, ano_base = ano_base)
}

NIVEIS_INDEXADO <- c("Total", "Private", "Public")

dados_indexado <- bind_rows(
  indexar_desde_inicio(wide$year_key, wide$Total)   |> mutate(serie = "Total"),
  indexar_desde_inicio(wide$year_key, wide$Private) |> mutate(serie = "Private"),
  indexar_desde_inicio(wide$year_key, wide$Public)  |> mutate(serie = "Public")
) |>
  mutate(serie = factor(serie, levels = NIVEIS_INDEXADO))

CORES_INDEXADO <- c(Total = "grey35", Private = COR_PRIVADO, Public = COR_PUBLICO)
TIPOS_INDEXADO <- c(Total = "22", Private = "solid", Public = "solid")

# Identificação (Total/Private/Public) vem de uma LEGENDA EXTERNA embaixo do
# painel (decisão do autor, 2026-08-27) — o texto perto de cada linha traz só
# o VALOR de 2024, ancorado no próprio ponto final (x = year_key, sem
# deslocamento), sem caixa de fundo (geom_text simples — o autor pediu
# "transparente mesmo").
fim_indexado <- dados_indexado |>
  group_by(serie) |>
  filter(year_key == max(year_key)) |>
  ungroup() |>
  mutate(
    rotulo = sprintf("%.0f", indice),
    y_rotulo = indice - 0.045 * max(dados_indexado$indice)
  )

p_indexado <- ggplot(dados_indexado, aes(year_key, indice, colour = serie, linetype = serie)) +
  geom_hline(yintercept = 100, linetype = "dotted", colour = COR_MEDIANA, linewidth = 0.3) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.2, data = fim_indexado, show.legend = FALSE) +
  geom_text(
    data = fim_indexado,
    aes(x = year_key, y = y_rotulo, label = rotulo),
    hjust = 0.5, size = 3.0, fontface = "bold", show.legend = FALSE
  ) +
  scale_colour_manual(name = NULL, values = CORES_INDEXADO) +
  scale_linetype_manual(name = NULL, values = TIPOS_INDEXADO) +
  ESCALA_X_COMPARTILHADA +
  scale_y_continuous(
    breaks = seq(0, 900, 100),
    labels = function(x) as.character(x),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  coord_cartesian(clip = "off") +
  guides(colour = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1)) +
  labs(
    subtitle = "B. Enrolment growth, indexed (1980 = 100)",
    x = "Year", y = NULL
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.8, "lines"),
    plot.subtitle = element_text(face = "bold", size = 9, hjust = 0)
  )

p_final <- (p_niveis / p_indexado) +
  plot_layout(heights = c(1, 1)) &
  theme(plot.margin = margin(t = 4, r = 5, b = 2, l = 2))

salvar_grafico(
  p_final,
  prefixo = "intro-composicao-setor-privado-1980-2024",
  largura = LARGURA_TEXTO,
  altura  = 5.4
)

# ==============================================================================
# PROMOÇÃO PARA O TEXTO (Fase 2) — autorizada pelo autor em 2026-08-02.
# A entrada estendida `sec-fignote-expansao-composicao-matriculas` já existe no
# apêndice B (Data Codebook § Extended Figure Notes); o hyperref abaixo aponta
# para ela.
# ==============================================================================

finalizar_figura(
  plot = p_final,
  fig_label = "expansao-composicao-matriculas",
  fig_cap = paste(
    "Expansion and composition of undergraduate enrolment, Brazil, 1980–2024."
  ),
  nota = paste(
    "Panel A: undergraduate enrolment by sector, in millions. Panel B:",
    "enrolment growth for Total, Private and Public sectors, indexed to each",
    "series' own 1980 level (1980 = 100). The shared linear axis gives each",
    "decade the same proportional vertical space, making the pace of growth",
    "in the 1990s legible against Panel A's absolute scale, where it is",
    "compressed near the axis by the much larger post-2000 expansion."
  ),
  fonte = paste(
    "INEP --- Censo da Educação Superior: microdata (2009--2024) and published",
    "Sinopse tables (1995--2008); FGV/IBRE historical series (Kang, Paese and",
    "Felix 2021) for total enrolment 1980--1994, and Maduro Júnior (2007) for",
    "the public/private split in those years"
  ),
  apendice = "sec-fignote-expansao-composicao-matriculas",
  script_path = here::here(
    "4-DA-Code", "2026-02_CENSUP_Public",
    "222_Tese_Expansao_Composicao_Matriculas.R"
  ),
  largura = LARGURA_TEXTO,
  altura = 5.4
)

cat("\n--- Índice (ano-base da própria série = 100) em anos-marco ---\n")
print(as.data.frame(
  dados_indexado |>
    select(year_key, serie, indice) |>
    pivot_wider(names_from = serie, values_from = indice) |>
    filter(year_key %in% c(1980, 1990, 1994, 1999, 2001, 2003, 2010, 2015, 2024)) |>
    mutate(across(-year_key, ~ round(.x, 0)))
))
