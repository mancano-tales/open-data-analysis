# ==============================================================================
# SCRIPT: 245D_Tese_Composicao_Decis.R
#
# Composição por decil de renda dos estudantes de ES (versão dissertação).
# Refaz 245C_v3_Composicao_Todos_Anos.R seguindo os padrões visuais da tese:
#   - thesis_theme() + LM Roman (plot_theme.R)
#   - Paleta divergente calibrada: RColorBrewer RdBu[-6] (D1=vermelho, D10=navy)
#   - Anotações de governo: underline cinza + texto incumbente (sem caixas coloridas)
#   - Labels nas barras: todos os decis com share ≥6% ou ≥3% (com COR_TEXTO)
#   - salvar_grafico() → graphs/ (PDF, iteração)
#   - finalizar_figura() → final/<fig_label>/ (.pdf + .R + .qmd)
#
# MODO controla a variável e a faixa etária (ver abaixo).
#
# FONTE DE DADOS: harmonização própria (Mancano 2026), PNAD/PNADC (IBGE).
# Parquets gerados por 035_Splice_Microdados.R.
#
# VER TAMBÉM: 245C_v3_Composicao_Todos_Anos.R (original, 2026-02_SALATA_PNAD/)
#             070_Plot_Composicao_Decis.R (versão anterior nesta pasta)
# ⚠ SCRIPT DERIVADO DO 245D (separado em 2026-08-09, decisao do autor).
#   Antes, as quatro variantes de composicao saiam de um unico script com a
#   variavel MODO. O efeito colateral era grave: o finalizar_figura arquiva
#   uma copia do script ao lado de cada PDF, e as quatro copias eram o MESMO
#   arquivo, declarando MODO <- "ens_sup_18_24". O .R ao lado do PDF de
#   18-64 afirmava ter sido gerado pela configuracao de 18-24 — o snapshot,
#   que existe justamente para dizer o que produziu cada figura, mentia.
#   Alem disso o stem (timestamp + nome do script) colidia entre as quatro.
#   Esta copia fixa MODO = "ens_sup_a_18_64" e aponta SCRIPT_PATH para si mesma.
#   Manter a logica em sincronia com as irmas 245D/E/F/G ao editar.
# =============================================================================
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(ggplot2)
  library(scales); library(Hmisc); library(RColorBrewer); library(here)
  library(patchwork) # composicao com a faixa de governos (banner_governos)
})
source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())
options(scipen = 999)

# ── CONFIGURAÇÃO ───────────────────────────────────────────────────────────────
# Opções de MODO (variável × faixa etária):
#   "ens_sup_18_24"   → ever accessed tertiary,  18–24 anos  [default/dissertação]
#   "ens_sup_18_64"   → ever accessed tertiary,  18–64 anos
#   "ens_sup_a_18_24" → currently enrolled,      18–24 anos
#   "ens_sup_a_18_64" → currently enrolled,      18–64 anos
MODO <- "ens_sup_a_18_64"   # fixo: um script por figura

# ── CAMINHOS ──────────────────────────────────────────────────────────────────
BASE_HARM   <- here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data", "output")
SCRIPT_PATH <- here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data",
                           "R", "03_analysis_plots", "245G_Tese_Composicao_ens_sup_a_18_64.R")

# ── LÓGICA DE CONFIGURAÇÃO POR MODO ──────────────────────────────────────────
cfg <- switch(MODO,
  ens_sup_18_24   = list(
    parquet   = "Microdados_Jovens_18_24_1992_2025.parquet",
    var       = "ens_sup",
    idade_max = 24L,
    var_lbl   = "ens_sup (ever entered tertiary)"
  ),
  ens_sup_18_64   = list(
    parquet   = "Microdados_Todas_Idades_1992_2025.parquet",
    var       = "ens_sup",
    idade_max = 64L,
    var_lbl   = "ens_sup (ever entered tertiary)"
  ),
  ens_sup_a_18_24 = list(
    parquet   = "Microdados_Jovens_18_24_1992_2025.parquet",
    var       = "ens_sup_a",
    idade_max = 24L,
    var_lbl   = "ens_sup_a (currently enrolled)"
  ),
  ens_sup_a_18_64 = list(
    parquet   = "Microdados_Todas_Idades_1992_2025.parquet",
    var       = "ens_sup_a",
    idade_max = 64L,
    var_lbl   = "ens_sup_a (currently enrolled)"
  ),
  stop("MODO desconhecido: ", MODO)
)

FIG_LABEL   <- paste0("composicao-decil-", gsub("_", "-", MODO))
PREFIXO_DEV <- paste0("245D_Composicao_Decis_", MODO)
idade_label <- if (cfg$idade_max == 24L) "ages 18–24" else "ages 18–64"

cat(sprintf("MODO: %s | Parquet: %s | Var: %s | Idades: 18-%d\n",
            MODO, cfg$parquet, cfg$var, cfg$idade_max))

parquet_path <- file.path(BASE_HARM, cfg$parquet)
if (!file.exists(parquet_path))
  stop("Parquet nao encontrado: ", parquet_path)

# ── PALETA ────────────────────────────────────────────────────────────────────
# Divergente RdBu calibrada: D1=vermelho-escuro (#67001F), D10=navy (#053061)
# [-6] remove o branco central (#F7F7F7), invisivel em fundo branco
# Healy (cap. 8, "Three Palette Types"): paleta divergente quando os dois
# extremos tem identidade semantica distinta (aqui: baixa vs. alta renda).
CORES_DECIL <- setNames(
  RColorBrewer::brewer.pal(11, "RdBu")[-6],
  paste0("D", 1:10)
)

# Texto sobre cada segmento — branco sobre escuro (D1-D3, D7-D10), escuro sobre claro (D4-D6)
COR_TEXTO <- c(
  "D1"  = "white",   "D2"  = "white",   "D3"  = "white",
  "D4"  = "#333333", "D5"  = "#333333", "D6"  = "#333333",
  "D7"  = "white",   "D8"  = "white",   "D9"  = "white",  "D10" = "white"
)

# GOVERNOS: tabela local e `gov_trans` REMOVIDOS em 2026-08-09 — a faixa vem
# inteira de banner_governos() (plot_theme.R), composta por cima com patchwork.
# A copia local rotulava blocos vizinhos com o MESMO nome ("FHC", "FHC",
# "Lula", "Lula"), separava Dilma II de Temer e datava as transicoes pela
# eleicao; as tres convencoes foram aposentadas pelo autor nesta data.

# ==============================================================================
# 1. CARGA E FILTRO
# ==============================================================================
cat("Carregando parquet...\n")
dados <- arrow::read_parquet(
  parquet_path,
  col_select = c("ano", "fonte", "idade", "peso", "renda_dom_pcta", cfg$var)
) |>
  filter(
    idade >= 18L, idade <= cfg$idade_max,
    # Overlap PNAD Anual / PNADC (2012-2015 existem nas duas fontes):
    # manter PNAD Anual para 1992-2015; PNADC a partir de 2016.
    !(fonte == "PNAD Contínua" & ano <= 2015L),
    !is.na(renda_dom_pcta), renda_dom_pcta > 0,
    !is.na(peso), peso > 0
  )

cat(sprintf("  %s obs em %d anos\n",
            format(nrow(dados), big.mark = "."),
            length(unique(dados$ano))))

# ==============================================================================
# 2. CALCULO DE DECIS E COMPOSICAO
# ==============================================================================
base <- dados |>
  mutate(sup_total = coalesce(as.integer(.data[[cfg$var]]), 0L)) |>
  group_by(ano) |>
  mutate(decil = as.integer(as.character(cut(
    renda_dom_pcta,
    breaks = Hmisc::wtd.quantile(renda_dom_pcta, weights = peso,
                                  probs = seq(0, 1, 0.1), na.rm = TRUE),
    labels = 1:10, include_lowest = TRUE
  )))) |>
  ungroup() |>
  filter(!is.na(decil))

comp <- base |>
  group_by(ano, decil) |>
  summarise(mat_pond = sum(peso * sup_total), .groups = "drop") |>
  group_by(ano) |>
  mutate(share_mat = mat_pond / sum(mat_pond) * 100) |>
  ungroup() |>
  mutate(
    decil_f    = factor(paste0("D", decil), levels = paste0("D", 10:1)),
    # Revertido para pagina paisagem dedicada em 2026-07-06 (segunda decisao
    # do autor: rotulos ficavam espremidos/sobrepostos em 15,92cm de coluna
    # de texto -- ver NEWS.md -- o autor optou por uma pagina so para esta
    # figura em vez de reduzir mais rotulos ou barras). Limiares de volta ao
    # valor original (28 barras em 24,62cm, ~0,88cm/barra: mais espaco que
    # em 15,92cm/retrato).
    label_full = case_when(
      share_mat >= 6 ~ paste0("D", decil, "\n", sprintf("%.0f%%", share_mat)),
      share_mat >= 3 ~ paste0("D", decil),
      TRUE           ~ ""
    )
  )

anos_disp <- sort(unique(comp$ano))
max_ano   <- max(anos_disp)
cat(sprintf("Anos: %d a %d (%d anos disponíveis)\n",
            min(anos_disp), max_ano, length(anos_disp)))

# O recorte da faixa ao intervalo de dados e feito por banner_governos().

# ==============================================================================
# 3. GRAFICO
# ==============================================================================
anos_str  <- paste0(min(anos_disp), "–", max_ano)

p <- ggplot(comp, aes(x = ano, y = share_mat, fill = decil_f)) +

  # Barras empilhadas — eixo x continuo, lacunas em anos sem pesquisa
  geom_bar(stat = "identity", width = 0.9, colour = "white", linewidth = 0.2) +

  # Labels internos: todos os decis com share suficiente (fonte de volta a
  # 1.9mm em 2026-07-06 -- pagina paisagem dedicada restaura o espaco por
  # barra que o formato retrato nao tinha)
  geom_text(
    aes(label = label_full, colour = decil_f),
    position    = position_stack(vjust = 0.5),
    size        = 1.9, fontface = "bold", lineheight = 0.8,
    show.legend = FALSE
  ) +

  # Linhas horizontais de referencia de equidade (cada decil = 10% se perfeito)
  geom_hline(yintercept = seq(10, 90, 10),
             colour = "white", linewidth = 0.25, linetype = "dashed", alpha = 0.5) +

  # Linhas verticais de transicao de governo
  # Transicoes de governo NAO recebem vertical tracejada (padrao do 097D,
  # adotado em toda a tese em 2026-08-09, a pedido do autor): a faixa
  # superior ja delimita os mandatos, e as verticais cortavam os rotulos.

  # A faixa de governos saiu daqui em 2026-08-09: era desenhada dentro do
  # painel (underline em y=103.5, rotulo em y=105.5, com o ylim esticado ate
  # 109 para abrir espaco). Agora vem de banner_governos() e e composta por
  # cima com patchwork, no bloco 4.

  # Escalas
  scale_fill_manual(
    name   = "Income\ndecile",
    values = CORES_DECIL,
    breaks = paste0("D", 10:1),
    labels = c(
      "D10: top 10%", "D9: 80–90%", "D8: 70–80%", "D7: 60–70%",
      "D6: 50–60%", "D5: 40–50%", "D4: 30–40%", "D3: 20–30%",
      "D2: 10–20%", "D1: bottom 10%"
    ),
    guide = guide_legend(ncol = 1, keywidth = unit(0.5, "cm"), keyheight = unit(0.5, "cm"))
  ) +
  scale_colour_manual(values = COR_TEXTO, guide = "none") +
  scale_x_continuous(
    breaks = anos_disp,
    expand = expansion(add = c(0.5, 0.5))
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0))
  ) +
  # ylim volta a 0-100: o headroom ate 109 existia so para caber a faixa.
  coord_cartesian(ylim = c(0, 100), clip = "off") +

  # Titulo/subtitulo/caption REMOVIDOS da imagem em 2026-07-06 (plano
  # fignotes/apendice, item 4.3c; WRITING-STYLE.md Sec 13.1): titulo
  # descritivo vai so no caption do Quarto, fonte so na fignote -- como
  # todas as demais figuras da tese. Antes desta correcao esta era a unica
  # figura com titulo/"Source:" embutidos na propria imagem.
  labs(
    x       = NULL,
    y       = "Share of students (%)"
  ) +

  theme(
    legend.position = "right",
    legend.title    = element_text(size = 8, face = "bold", family = THESIS_FONT),
    legend.text     = element_text(size = 7.5, family = THESIS_FONT),
    legend.key.size = unit(0.5, "cm"),
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 7, family = THESIS_FONT),
    panel.grid      = element_blank(),
    plot.margin     = margin(8, 10, 8, 8)
  )

# ==============================================================================
# 4. SALVAR
# ==============================================================================

# 4a. Desenvolvimento — PDF datado em graphs/ (para iterar)
# Pagina paisagem dedicada (decisao do autor, 2026-07-06): 28 barras nao
# cabiam legiveis em LARGURA_TEXTO (retrato). Largura = LARGURA_PAISAGEM
# (usa toda a largura disponivel da pagina girada), mas ALTURA proposital-
# mente MENOR que ALTURA_PAISAGEM (13cm em vez dos 15,92cm cheios): o
# \begin{figure} do Quarto poe a legenda da figura E a fignote ABAIXO da
# imagem, na mesma pagina landscape -- usar a altura paisagem cheia so para
# a imagem nao deixa espaco para esse texto, e a fignote transborda para
# uma segunda pagina landscape quase em branco (visto no render de teste de
# 2026-07-06). 13cm deixa ~2,9cm de sobra para legenda + fignote.
ALTURA_PAISAGEM_FIG <- 13

# Composicao com a faixa de governos por cima (padrao 097D, 2026-08-09).
# desloc = -0.5: sao BARRAS, e cada uma ocupa [ano-0.5; ano+0.5] — uma
# fronteira no ano de posse cortaria ao meio a barra do primeiro ano do
# mandato. fim_dados leva o mesmo +0.5, que e a borda direita da ultima barra.
escala_x_gov <- scale_x_continuous(breaks = anos_disp,
                                   expand = expansion(add = c(0.5, 0.5)))
p_banner <- banner_governos(fim_dados = max_ano + 0.5, desloc = -0.5,
                            scale_x = escala_x_gov)
p <- p_banner / p + patchwork::plot_layout(heights = c(1, 13))

salvar_grafico(p,
  prefixo  = PREFIXO_DEV,
  largura  = LARGURA_PAISAGEM, altura = ALTURA_PAISAGEM_FIG, unidades = "cm",
  formato  = "pdf")

# 4b. Versao final — PDF + R + QMD em final/<fig_label>/
# Fignote enxuta (WRITING-STYLE.md Sec 13.7): detalhe de decisoes de
# harmonizacao migrou para a entrada da figura em "Extended Figure Notes".
var_lbl_curto <- if (cfg$var == "ens_sup") "ever enrolled" else "currently enrolled"
nota <- paste0(
  "Each bar is the share of students aged 18--", cfg$idade_max,
  " (", var_lbl_curto, ") from each income decile. Dashed lines: perfect equity (10\\% each). ",
  "Government labels abbreviated for short periods (D.~= Dilma II; L.~= Lula III)."
)

finalizar_figura(
  plot        = p,
  fig_label   = FIG_LABEL,
  fig_cap     = paste0(
    "Income composition of tertiary education students, Brazil ",
    min(anos_disp), "--", max_ano, "."
  ),
  nota        = nota,
  apendice    = paste0("sec-fignote-", FIG_LABEL),
  fonte       = paste0(
    "IBGE --- PNAD (1992--2015) and PNAD Cont\\'{\\i}nua (2016--", max_ano, "), ",
    "harmonized series"
  ),
  script_path = SCRIPT_PATH,
  largura     = LARGURA_PAISAGEM,
  altura      = ALTURA_PAISAGEM_FIG,
  unidades    = "cm"
)

cat(sprintf("\n═════ Script 245D concluído | MODO: %s ═════\n", MODO))
