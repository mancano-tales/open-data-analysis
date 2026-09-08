# ==============================================================================
# 040_Fig_Descolamento_EaD_Politicas.R
#
# Figura nova, construida INTEIRAMENTE sobre a base do SEDAP+ (2009-2024), em
# nivel de aluno e com definicao unica em todos os anos.
#
# O QUE ELA MOSTRA, e por que nao e a 028 estendida:
#
# A 028 (@fig-matriculas-turno-via-acesso) empilha o sistema inteiro em 15
# combinacoes por ano. E uma boa figura de composicao, mas o achado central
# desta rodada nao aparece nela: o descolamento entre o VOLUME do EaD e o
# ALCANCE das politicas de descomodificacao. Numa barra empilhada de 10
# milhoes, uma linha de ProUni travada em 120 mil e invisivel.
#
# Aqui o descolamento e o assunto, em dois paineis com escalas proprias:
#
#   Painel A  matricula privada por modalidade e via de acesso, em milhoes.
#             Mostra o EaD saindo de ~10% para 60% do setor privado.
#
#   Painel B  a MESMA politica vista como cobertura: % da matricula privada
#             com ProUni ou FIES, separada por modalidade. Mostra que o
#             presencial teve cobertura de ~20-25% e o EaD nunca passou de
#             ~7%. O sistema nao desmontou as politicas; ele cresceu para
#             fora do alcance delas.
#
# A leitura conjunta e o argumento: a descomodificacao nao recuou por
# decisao de suspende-la, e sim porque a oferta migrou para uma modalidade
# que os instrumentos nunca alcancaram. Isso separa empiricamente o canal de
# acessibilidade do canal de descomodificacao, que e o que a Parte II
# precisa.
#
# ALTERNATIVAS DE DESENHO CONSIDERADAS (registradas para o autor avaliar):
#
#   (i)   Estender a 028 ate 2024 e nao criar figura nova. Rejeitada porque a
#         serie do SEDAP+ e conceitualmente distinta da local (supressao de
#         celulas pequenas) e porque o achado ficaria invisivel, como acima.
#         Continua sendo opcao: os dados estao prontos para isso.
#   (ii)  Painel unico com area de EaD e linha de ProUni no mesmo eixo.
#         Rejeitada: 120 mil contra 4,9 milhoes nao coexistem num eixo so
#         sem eixo secundario, e eixo secundario e proibido pelo
#         WRITING-STYLE (sec. 13).
#   (iii) Painel B em numeros absolutos em vez de cobertura. Rejeitada: em
#         absoluto o ProUni parece apenas estavel; e a razao contra a
#         matricula que revela o encolhimento relativo, que e o ponto.
#   (iv)  Incluir a rede publica nos dois paineis. Rejeitada para o painel B
#         (a rede publica nao tem preco, logo "cobertura de bolsa" nao se
#         aplica) e mantida no A apenas como referencia de contexto — ver
#         INCLUIR_PUBLICO abaixo, que o autor pode ligar.
#
# Fonte de dados: 020_Extrair_Nicho_2020_2024.R (que cobre 2009-2024).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(data.table)
  library(here)
  library(patchwork)
})

source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())

INCLUIR_PUBLICO <- FALSE # ver alternativa (iv) no cabecalho

BASE <- file.path(
  dirname(here::here()), "5-data", "INEP", "derived",
  "nicho_agregado_sedap_2010_2024.csv"
)
stopifnot(file.exists(BASE))
d <- fread(BASE)

# ------------------------------------------------------------------------------
# Preparo. `turno` vem como Day / Evening / Distance; a figura precisa da
# distincao presencial x EaD e, dentro do presencial, do turno.
# ------------------------------------------------------------------------------
d <- d |>
  mutate(
    modalidade = ifelse(turno == "Distance", "Distance", "In person"),
    bolsa = via %in% c("ProUni", "FIES")
  )

anos <- sort(unique(d$ano))

# --- Painel A: composicao da matricula privada --------------------------------
pa_dados <- d |>
  filter(setor == "Privado") |>
  mutate(
    grupo = case_when(
      turno == "Distance" & bolsa ~ "Distance, with ProUni or FIES",
      turno == "Distance" ~ "Distance, full fee",
      bolsa ~ "In person, with ProUni or FIES",
      TRUE ~ "In person, full fee"
    )
  ) |>
  group_by(ano, grupo) |>
  summarise(QT = sum(QT_MATRICULAS), .groups = "drop")

NIVEIS_A <- c(
  "In person, full fee", "In person, with ProUni or FIES",
  "Distance, full fee", "Distance, with ProUni or FIES"
)
CORES_A <- c(
  "In person, full fee" = "#9ECAE1",
  "In person, with ProUni or FIES" = "#0072B2",
  "Distance, full fee" = "#F5C57A",
  "Distance, with ProUni or FIES" = "#D55E00"
)

p_a <- ggplot(
  pa_dados |> mutate(grupo = factor(grupo, levels = NIVEIS_A)),
  aes(x = ano, y = QT / 1e6, fill = grupo)
) +
  geom_area(colour = "white", linewidth = 0.25) +
  scale_fill_manual(values = CORES_A, name = NULL) +
  scale_x_continuous(
    breaks = c(2010, 2013, 2016, 2019, 2022, 2024),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "M"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    subtitle = "A. Private enrolment by mode of delivery and access route",
    x = NULL, y = "Enrolment"
  ) +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 7),
    legend.key.size = unit(0.32, "cm")
  )

# --- Painel B: cobertura das politicas ----------------------------------------
pb_dados <- d |>
  filter(setor == "Privado") |>
  group_by(ano, modalidade) |>
  summarise(
    cobertura = 100 * sum(QT_MATRICULAS[bolsa]) / sum(QT_MATRICULAS),
    .groups = "drop"
  )

# Rotulos diretos no lugar de legenda (WRITING-STYLE sec. 13): com duas series
# so, a legenda custa uma viagem de ida e volta do olho por nada.
pico <- pb_dados |>
  filter(modalidade == "In person") |>
  slice_max(cobertura, n = 1)
fim <- pb_dados |> filter(ano == max(ano))

p_b <- ggplot(pb_dados, aes(x = ano, y = cobertura, colour = modalidade)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.5) +
  geom_text(
    data = fim, aes(label = sprintf("%s: %.1f%%", modalidade, cobertura)),
    hjust = 1, vjust = -1.1, size = 2.7, fontface = "bold", show.legend = FALSE
  ) +
  annotate("text",
    x = pico$ano, y = pico$cobertura + 2.6,
    label = sprintf("peak %.1f%% (%d)", pico$cobertura, pico$ano),
    size = 2.6, colour = "#0072B2"
  ) +
  scale_colour_manual(
    values = c("In person" = "#0072B2", "Distance" = "#D55E00"), guide = "none"
  ) +
  scale_x_continuous(
    breaks = c(2010, 2013, 2016, 2019, 2022, 2024),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"), limits = c(0, NA),
    expand = expansion(mult = c(0, 0.14))
  ) +
  labs(
    subtitle = "B. Share of private enrolment holding a ProUni scholarship or a FIES loan",
    x = NULL, y = "Coverage"
  )

# DECISAO (autor, 2026-08-06): a figura promovida e SO o painel de cobertura.
#
# O painel de composicao (p_a, mantido acima e ainda salvo em graphs/ para
# consulta) e redundante com @fig-matriculas-turno-via-acesso, que mostra a
# mesma composicao com muito mais detalhe e ja ocupa uma pagina do capitulo.
# Publicar os dois faria o leitor ver a mesma historia duas vezes.
#
# O painel de cobertura, ao contrario, nao e redundante com nada: a taxa de
# cobertura existe matematicamente na figura de quinze anos, mas nao e
# VISIVEL nela — o leitor teria de estimar a olho razoes entre faixas finas.
# E o unico grafico do capitulo que mostra o descolamento diretamente.
p_composicao <- p_a / p_b + plot_layout(heights = c(1.25, 1))
salvar_grafico(p_composicao,
  prefixo = "040_Descolamento_Dois_Paineis",
  largura = LARGURA_TEXTO, altura = 6.4, unidades = "in"
)

# A versao enxuta e a que vai para o capitulo: meia pagina em vez de pagina
# inteira, sustentando os numeros de cobertura que a prosa afirma.
p <- p_b + labs(subtitle = NULL)

salvar_grafico(p,
  prefixo = "040_Descolamento_EaD_Politicas",
  largura = LARGURA_TEXTO, altura = 3.1, unidades = "in"
)

cat("\n=== NUMEROS PARA A PROSA ===\n")
print(as.data.frame(pb_dados |> pivot_wider(
  names_from = modalidade,
  values_from = cobertura
) |>
  mutate(across(where(is.numeric), ~ round(.x, 1)))))
cat("\nEaD como % do setor privado:\n")
print(as.data.frame(
  d |> filter(setor == "Privado") |> group_by(ano) |>
    summarise(pct_ead = round(100 * sum(QT_MATRICULAS[turno == "Distance"]) /
      sum(QT_MATRICULAS), 1), .groups = "drop")
))

# ──────────────────────────────────────────────────────────────────────────
# Promocao. Decisao tomada em sessao autonoma (2026-08-05, autor ausente);
# reversivel — basta nao citar o pacote no capitulo, e a figura fica em
# final/ sem efeito sobre o texto.
# ──────────────────────────────────────────────────────────────────────────
finalizar_figura(
  plot = p,
  fig_label = "descolamento-ead-politicas",
  fig_cap = "Distance learning and the reach of student aid, private sector, Brazil, 2010--2024.",
  fonte = "INEP — Censo da Educação Superior, student-level microdata via SEDAP+ (2010–2024)",
  nota = paste(
    "Each line is the share of private enrolment in that mode of delivery held by a student with a",
    "ProUni scholarship or a FIES loan --- the reach of the two instruments, rather than their size.",
    "Coverage of in-person study rose to 35.6\\% in 2015 and fell to 13.8\\% by 2024; in distance study",
    "it never exceeded 7.3\\% and ended at 2.7\\%, over the same years in which distance enrolment grew",
    "from 15.7\\% to 60.5\\% of the private sector, as the preceding figure shows. The instruments",
    "were not dismantled so much as outgrown. Counts come from the student-level census records",
    "queried through SEDAP+, which suppresses cells holding few individuals; the ratios plotted here",
    "match the published microdata for 2010--2019 to within 0.1 percentage point."
  ),
  apendice = "sec-fignote-descolamento-ead-politicas",
  script_path = here::here(
    "4-DA-Code", "2026-08_SEDAP",
    "040_Fig_Descolamento_EaD_Politicas.R"
  ),
  largura = LARGURA_TEXTO, altura = 3.1
)
