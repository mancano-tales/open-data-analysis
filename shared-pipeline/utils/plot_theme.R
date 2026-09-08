# ==============================================================================
# plot_theme.R — Sistema de estilo visual compartilhado da dissertação
#
# USO: source(here::here("4-DA-Code", "utils", "plot_theme.R"))
#      Depois: theme_set(thesis_theme())
#
# CONTEÚDO:
#   1. Tipografia: Latin Modern Roman instalada como fonte real do Windows
#      (texto selecionável em PDF); fallback para "serif"
#   2. Paletas Okabe-Ito (colorblind-safe, aprovado pela Nature) + sequencial HCL
#   3. Dimensões padrão para A4 (texto = 15,92 cm = 6,27 in; + paisagem)
#   3b. scale_x_anos_tese() — grade de anos padronizada (WRITING-STYLE 13.8)
#   4. thesis_theme() — tema ggplot baseado em theme_minimal()
#   4b. tema_grade_densa() — aditivo p/ facet_wrap com muitos painéis
#   5. Helpers de escala: thesis/decil/delta (fill e colour) + diverging (mapas)
#   6. camada_destaque() — padrão "Layer, Highlight, Repeat" (Healy Cap. 8)
#   7. salvar_grafico() — wrapper padronizado para ggsave() (PNG ou PDF)
#   8. finalizar_figura() — pacote .pdf+.R+.qmd em 6-images-tables/final/
#   9. Modelo de bloco de comentário para inserção de figuras no .qmd
#
# REFERÊNCIA: Healy, K. (2026). Data Visualization: A Practical Introduction
#   (2nd ed.). Princeton University Press. https://socviz.co/ [@Healy2026]
#   Cópia local dos capítulos: 4-DA-Code/utils/socviz/
#
# TIPOGRAFIA: A dissertação usa Latin Modern (lmodern no LaTeX/KOMA-Script).
#   Desde 2026-07-12, os arquivos OTF do TinyTeX (lmroman10-*.otf) são
#   instalados como fonte REAL do Windows (per-user, sem admin) em vez de
#   renderizados via showtext — isso é o que torna o texto dos PDFs
#   selecionável/pesquisável. Ver seção 1 abaixo para o gotcha dos dois nomes
#   de fonte (GDI vs. systemfonts) e 4-DA-Code/utils/README.md para o script
#   de instalação (necessário rodar de novo numa máquina nova).
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(here)
})

# ------------------------------------------------------------------------------
# 1. TIPOGRAFIA
# Fonte: Latin Modern Roman (OTF do TinyTeX), instalada como fonte REAL do
# Windows (per-user, sem admin — ver 4-DA-Code/utils/README.md secao "Fonte
# selecionavel em PDF" para o script de instalacao). Isso substitui o uso de
# showtext (2026-07-08 -> 2026-07-12): showtext desenha cada caractere como
# curva vetorial, o que produzia PDFs com texto NAO selecionavel/pesquisavel
# (confirmado com pdffonts/pdftotext em toda figura ja promovida) e mais
# pesados em bytes (cada letra vira um caminho proprio em vez de referencia
# a um glifo embutido). Com a fonte instalada de verdade, cairo_pdf() e os
# devices raster baseados em systemfonts (ragg::agg_png, usado por padrao
# pelo ggsave() quando 'ragg' esta instalado) embutem o glifo real da fonte
# — texto genuinamente selecionavel no PDF, arquivo menor.
#
# GOTCHA — dois motores de fonte, dois nomes para a MESMA fonte: o OTF da
# Latin Modern Roman grava o nome em dois campos da tabela 'name' que
# motores diferentes leem preferencialmente:
#   - GDI/Cairo (usado por grDevices::cairo_pdf, nosso device de PDF) le o
#     nameID 1 ("Family"): "LM Roman 10"
#   - DirectWrite/systemfonts (usado por ragg, portanto pelo PNG default do
#     ggsave) le o nameID 16 ("Typographic Family"): "Latin Modern Roman"
# Passar o nome errado para o device errado NAO da erro — ele silenciosamente
# substitui por Arial, e a figura sai com a fonte trocada sem aviso. Por
# isso salvar_grafico()/finalizar_figura() sobrescrevem o 'family' do plot
# na hora de salvar, usando FONTE_PDF para formato="pdf" e FONTE_PNG para
# formato="png" — nenhum script precisa saber dessa distincao.
#
# PORTABILIDADE: a instalacao da fonte e por-usuario e por-maquina (grava em
# %LOCALAPPDATA%\Microsoft\Windows\Fonts + registro HKCU). Numa maquina nova
# (ou apos reinstalar o Windows), rode o instalador de novo — ver README.md.
# Sem a fonte instalada, cai em "serif" (fallback abaixo), como antes.
# ------------------------------------------------------------------------------
.font_installed <- function(nome_familia) {
  if (!requireNamespace("systemfonts", quietly = TRUE)) {
    return(FALSE)
  }
  m <- tryCatch(systemfonts::match_fonts(nome_familia), error = function(e) NULL)
  !is.null(m) && isTRUE(grepl("lmroman10", m$path, ignore.case = TRUE))
}

.lm_ok <- .font_installed("Latin Modern Roman")

FONTE_PDF <- if (.lm_ok) "LM Roman 10" else "serif"
FONTE_PNG <- if (.lm_ok) "Latin Modern Roman" else "serif"

if (!.lm_ok) {
  message(
    "[plot_theme] Latin Modern Roman nao encontrada como fonte instalada do ",
    "Windows — fallback para 'serif' (texto ainda assim sera selecionavel ",
    "no PDF, so que sem o tipo Latin Modern). Ver README.md p/ instalar."
  )
}

THESIS_FONT <- FONTE_PNG # default para uso interativo/preview (RStudio, etc.)

# ------------------------------------------------------------------------------
# 2. PALETAS — Okabe-Ito (colorblind-safe)
# Ref.: Okabe & Ito (2008); adotado por Nature, cowplot, ggthemes.
# Testado para protanopia, deuteranopia e tritanopia.
# ------------------------------------------------------------------------------

# Qualitativa — categorias sem ordem (governos, fontes, regiões, partidos)
PALETA_QUALITATIVA <- c(
  "#E69F00", # laranja
  "#56B4E9", # azul-céu
  "#009E73", # verde
  "#0072B2", # azul-escuro
  "#D55E00", # vermelho-tijolo
  "#CC79A7", # lilás
  "#F0E442", # amarelo (usar por último — baixo contraste em fundo branco)
  "#000000" # preto
)

# Sequencial — dados ordenados, baixo → alto (ex: decis D1→D10)
# HCL (colorspace) é perceptualmente uniforme (Healy Cap. 1/8); fallback para
# interpolação RGB se o pacote 'colorspace' não estiver instalado.
.paleta_sequencial_hcl <- function(n = 10) {
  if (requireNamespace("colorspace", quietly = TRUE)) {
    colorspace::sequential_hcl(n, palette = "Blues 3")
  } else {
    message(
      "[plot_theme] Pacote 'colorspace' nao instalado — paleta sequencial ",
      "usara interpolacao RGB (menos uniforme). Execute install.packages('colorspace')."
    )
    colorRampPalette(c("#FFFFFF", "#0072B2"))(n)
  }
}
PALETA_SEQUENCIAL <- .paleta_sequencial_hcl(10)

# Divergente — delta positivo/negativo (ex: variação por governo)
COR_POSITIVO <- "#0072B2" # azul-escuro Okabe — ganho / aumento
COR_NEGATIVO <- "#D55E00" # vermelho-tijolo Okabe — perda / queda
COR_NACIONAL <- "#000000" # linha de referência nacional
COR_MEDIANA <- "#888888" # linha mediana / referência neutra

# ------------------------------------------------------------------------------
# 3. DIMENSÕES PADRÃO (A4, margens 2,54 cm)
# Largura do texto = 21 - 2 × 2,54 = 15,92 cm = 6,27 in
# ------------------------------------------------------------------------------
LARGURA_TEXTO <- 6.27 # figura full-width (largura do bloco de texto)
LARGURA_MEIA <- 3.10 # figura half-width (duas colunas, float ao lado)
LARGURA_2_3 <- 4.20 # figura dois-terços

ALTURA_PADRAO <- 3.80 # razão áurea ~1,65:1 — adequada para maioria
ALTURA_ALTA <- 5.00 # muitas categorias no eixo y (ex: 042B decil)
ALTURA_DUPLA <- 7.50 # painéis 2×2 ou barras longas (ex: vintil)

# Página paisagem cheia (rotacionada via \usepackage{pdflscape} +
# \begin{landscape} no .qmd) — para figuras com grades densas de múltiplos
# painéis que não cabem legíveis na largura normal do texto. Área útil A4
# paisagem com margens de 2,54cm: 29,7 - 2×2,54 = 24,62cm (largura),
# 21 - 2×2,54 = 15,92cm (altura). Usar com uma folga (ex: 24×15) em vez do
# valor exato. Ver WRITING-STYLE.md §13.6 e NEWS.md 2026-06-30.
LARGURA_PAISAGEM <- 24.62
ALTURA_PAISAGEM <- 15.92

DPI_IMPRESSAO <- 300

# ------------------------------------------------------------------------------
# 3b. EIXO DE ANOS PADRONIZADO — scale_x_anos_tese()
# Politica 2026-07-06 (WRITING-STYLE.md Sec. 13.8): toda figura com anos no
# eixo X usa esta grade em vez de um scale_x_continuous(breaks=...) proprio,
# para que todos os graficos da tese herdem o mesmo padrao. Grade fixa de 4
# em 4 anos alinhada a inicio de mandato presidencial, sempre incluindo o
# primeiro e o ultimo ano efetivamente presentes na serie (mesmo que caiam
# fora da grade regular) para que os extremos nunca fiquem sem rotulo.
# ------------------------------------------------------------------------------
ANOS_MANDATO <- c(1995, 1999, 2003, 2007, 2011, 2015, 2019, 2023)

scale_x_anos_tese <- function(anos, limits = NULL, ...) {
  anos <- anos[!is.na(anos)]
  min_ano <- min(anos)
  max_ano <- max(anos)
  grade <- ANOS_MANDATO[ANOS_MANDATO >= min_ano & ANOS_MANDATO <= max_ano]

  # REGRA DE DESEMPATE NO FIM DA SERIE (autor, 2026-08-09): quando uma marca de
  # mandato cai perto do ultimo ano, quem fica e o MANDATO, e o extremo nao e
  # rotulado. Antes era o contrario — o extremo vencia e a marca de mandato
  # caia —, e o resultado com dados ate 2024 era um eixo terminando em "2024",
  # que nao significa nada alem de "ultimo ano", tendo descartado o 2023, que e
  # inicio de mandato. Com dados ate 2025 a regra antiga mantinha os dois e os
  # rotulos colidiam na largura da 097D.
  #   O ano final da serie nao se perde: ele consta da legenda da figura
  # ("Brazil 1992-2025") e da fonte. O inicio continua sempre rotulado — ali
  # nao ha conflito, porque 1992 nao e ano de mandato.
  JANELA_FIM <- 2
  grade <- grade[abs(grade - min_ano) > 1]
  mandato_no_fim <- any(abs(grade - max_ano) <= JANELA_FIM)
  breaks <- if (mandato_no_fim) {
    sort(unique(c(grade, min_ano)))
  } else {
    sort(unique(c(grade, min_ano, max_ano)))
  }
  limits_val <- if (is.null(limits)) c(min_ano, max_ano) else limits
  ggplot2::scale_x_continuous(
    breaks = breaks,
    labels = function(x) sprintf("%d", as.integer(x)),
    limits = limits_val,
    ...
  )
}

# Variante para series mensais/diarias (eixo X e Date, nao ano inteiro) --
# mesma grade de mandatos, mas os extremos sao a data exata dos dados (nao
# 1o de janeiro do ano), para que o tick fique sobre o inicio/fim real da
# serie em vez de cair fora do painel quando a serie nao comeca em janeiro
# (ex.: 010_IPCA_Curso_Superior_Series.R, serie mensal iniciada em ago/1999).
scale_x_anos_tese_date <- function(datas, ...) {
  datas <- datas[!is.na(datas)]
  min_d <- min(datas)
  max_d <- max(datas)
  min_ano <- as.integer(format(min_d, "%Y"))
  max_ano <- as.integer(format(max_d, "%Y"))
  grade <- ANOS_MANDATO[ANOS_MANDATO >= min_ano & ANOS_MANDATO <= max_ano]
  grade <- grade[abs(grade - min_ano) > 1 & abs(grade - max_ano) > 1]
  breaks <- sort(unique(c(as.Date(paste0(grade, "-01-01")), min_d, max_d)))
  ggplot2::scale_x_date(
    breaks = breaks,
    labels = function(x) format(x, "%Y"),
    ...
  )
}

# ------------------------------------------------------------------------------
# 3c. FAIXA DE GOVERNOS — banner_governos()
#
# Extraída do 097D_Tese.R em 2026-08-09, a pedido do autor ("por que você não
# copiou o código do rótulo do Wagstaff?"). Antes, cada figura desenhava a
# faixa DENTRO do próprio painel, via expansão do ylim, o que trazia dois
# defeitos que só a 097D não tinha:
#
#   1. Verticais de transição cortavam os rótulos. Como a faixa vivia dentro do
#      painel, qualquer geom_vline atravessava o texto dos mandatos. A 097D
#      escapava porque compõe a faixa como GRÁFICO SEPARADO (patchwork), fora
#      do alcance de qualquer camada do painel principal.
#   2. Os traços se encostavam. As demais desenhavam de `ini - 0.5` a
#      `fim + 0.5`, então segmentos vizinhos terminavam e começavam na MESMA
#      coordenada: o resultado era uma linha quase contínua, com falhas de
#      antialiasing nas junções. A 097D recua 0.08 ano de cada lado, o que
#      produz um respiro deliberado e limpo entre mandatos.
#
# Uso: componha com patchwork por cima do painel principal, passando a MESMA
# escala x (é ela que garante o alinhamento das colunas):
#
#   escala_x <- scale_x_anos_tese(anos = anos_disp)
#   p_banner <- banner_governos(fim_dados = max(anos_disp), scale_x = escala_x)
#   p_final  <- p_banner / p_main + patchwork::plot_layout(heights = c(1, 11))
#
# O painel principal NÃO deve mais reservar headroom no ylim para a faixa.
# ------------------------------------------------------------------------------
# Anos de transição — fronteiras exatas dos blocos de governo, que não são a
# mesma coisa que a grade de ticks do eixo (ANOS_MANDATO).
#
# CRITÉRIO: ANO DE POSSE (decisão do autor, 2026-08-09). O vetor herdado do
# 097D dizia "eleição/posse/impeachment" e de fato misturava os três, com dois
# marcos um ano adiantados: Dilma I aparecia em 2010 (ano da ELEIÇÃO; a posse
# foi em 1/1/2011) e Dilma II em 2014 (idem; posse em 1/1/2015). Corrigidos.
# O 2016 permanece porque ali o critério é outro e está certo: Temer assume em
# 31/8/2016, pelo impeachment, não por eleição.
#   Efeito colateral bem-vindo: o vetor passa a coincidir com ANOS_MANDATO
# (grade do eixo) em todos os anos comuns, então tick, vertical de transição e
# emenda do banner caem exatamente no mesmo lugar.
#
# DILMA II + TEMER COLAPSADOS (decisão do autor, 2026-08-09). Com o critério de
# posse, o bloco de Dilma II encolheu de dois anos (2014–2016) para um
# (2015–2016), e nessa largura nenhum rótulo cabe — "D. II" colidia com
# "Temer". Os dois viram um bloco de 2015 a 2019, como o autor já havia
# decidido para os dotplots 042C, pela mesma razão substantiva: um mandato de
# 1 ano e outro de 3 não permitem isolar efeitos de política.
#   CUSTO A DECLARAR: a fronteira de 2016 desaparece da faixa, e com ela a
# marca visual do impeachment — que é objeto de análise na Parte III. A ruptura
# continua no texto e nas figuras da Parte III; só não aparece mais como
# divisória nesta faixa.
ANOS_TRANSICAO_GOV <- c(1992, 1995, 1999, 2003, 2007, 2011, 2015, 2019, 2023)
# Conjunto PADRÃO: nome por extenso, abreviando só onde o período é curto
# demais para caber (Dilma II, 2 anos; Lula III, em curso).
# Abreviações onde o bloco é estreito demais para o nome por extenso, medidas
# em renderização real e não por precaução (autor, 2026-08-09):
#   "D.II/Tem." — o bloco colapsado tem 4 anos; "Dilma II/Temer" (14 car.) não
#                 cabe, e mesmo "D. II/Temer" encostava em Bolsonaro.
#   "Bolson."   — vizinho imediato do bloco acima; encurtar só um dos dois não
#                 abria espaço suficiente entre eles.
# Os demais mantêm o nome por extenso: cabem, e abreviar sem necessidade custa
# legibilidade. Houve, entre 2026-08-09 14h e 15h, um segundo vetor
# (NOMES_GOV_MINI) para painéis estreitos; foi eliminado ao se constatar que a
# colisão ocorre já na largura de texto padrão — manter dois conjuntos só
# criaria a chance de figuras irmãs divergirem no rótulo.
NOMES_GOV_CURTO <- c(
  "Itamar", "FHC I", "FHC II", "Lula I", "Lula II",
  "Dilma I", "D.II/Tem.", "Bolson.", "L. III"
)

# Verticais de transição, para o painel principal. DEVEM usar exatamente os
# mesmos anos das fronteiras do banner — caso contrário a linha cai ao lado da
# emenda entre dois mandatos, e não sobre ela. Era o que acontecia antes de
# 2026-08-09: o banner vinha de ANOS_TRANSICAO_GOV (anos inteiros) e as
# verticais de um vetor `gov_trans` em MEIOS-ANOS (1994.5, 1998.5, …), herdado
# de uma convenção diferente. O deslocamento de meio ano era pequeno mas
# visível, e foi o autor quem apontou.
linhas_transicao_gov <- function(anos_transicao = ANOS_TRANSICAO_GOV,
                                 colour = "#CCCCCC",
                                 linewidth = 0.35,
                                 linetype = "dashed") {
  # O primeiro elemento é o início da série, não uma transição.
  ggplot2::geom_vline(
    xintercept = anos_transicao[-1],
    colour = colour, linewidth = linewidth,
    linetype = linetype
  )
}

banner_governos <- function(fim_dados,
                            scale_x,
                            ini_dados = NULL,
                            desloc = 0,
                            anos_transicao = ANOS_TRANSICAO_GOV,
                            nomes = NOMES_GOV_CURTO,
                            tamanho_pt = 8,
                            recuo = 0.08) {
  # `desloc` (2026-08-09): deslocamento das FRONTEIRAS, em anos. Nas figuras de
  # linha o dado está NO ano, então a fronteira de um mandato coincide com o
  # ano de posse e `desloc = 0`. Nas de BARRAS (245D–G) cada barra ocupa o
  # intervalo [ano − 0,5; ano + 0,5], e uma fronteira em 1995 cortaria a barra
  # de 1995 ao meio — o primeiro ano do mandato apareceria metade num bloco e
  # metade no outro. Passar `desloc = -0.5` põe a emenda no vão entre as
  # barras. Era o que a tabela local do 245D fazia com `ini - 0.5`.
  #   `desloc` NÃO se aplica a `fim_dados`/`ini_dados`: esses dois são a
  # coordenada x da borda dos dados, que o chamador já informa na escala certa
  # (para barras, `max(anos) + 0.5`, a borda direita da última barra).
  anos_transicao <- anos_transicao + desloc

  gov <- data.frame(
    nome = nomes,
    xband_min = anos_transicao,
    xband_max = c(anos_transicao[-1], fim_dados),
    stringsAsFactors = FALSE
  )
  gov <- gov[gov$xband_min < fim_dados, , drop = FALSE]
  gov$xband_max[nrow(gov)] <- fim_dados

  # ini_dados (2026-08-09): nem toda figura começa em 1992. A 232 (fardo da
  # mensalidade) começa em 2000 e a 237 (renda média) em 2002, porque suas
  # fontes não vão mais longe. Sem este recorte, a faixa desenharia mandatos
  # inteiros fora do domínio do painel: os segmentos seriam cortados pela
  # escala, mas o RÓTULO, centrado no bloco completo, apareceria deslocado —
  # "FHC II" centrado em 2001 num painel que começa em 2000. Recortando o
  # primeiro bloco, o rótulo se recentra sobre a parte visível.
  if (!is.null(ini_dados)) {
    gov <- gov[gov$xband_max > ini_dados, , drop = FALSE]
    gov$xband_min[1] <- max(gov$xband_min[1], ini_dados)
  }

  gov$x_label <- (gov$xband_min + gov$xband_max) / 2
  gov$hjust_label <- 0.5
  # Último bloco: centrar num período de 1-2 anos joga o texto por cima do
  # vizinho. Ancora pela borda esquerda e cresce para a direita, região vazia
  # por definição (não há dado depois do fim da série).
  ult <- nrow(gov)
  gov$x_label[ult] <- gov$xband_min[ult]
  gov$hjust_label[ult] <- 0

  gov$seg_min <- gov$xband_min + recuo
  gov$seg_max <- gov$xband_max - recuo

  ggplot2::ggplot(gov) +
    ggplot2::geom_segment(
      ggplot2::aes(x = seg_min, xend = seg_max, y = 0.12, yend = 0.12),
      colour = "grey75", linewidth = 0.5
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = x_label, y = 0.55, label = nome, hjust = hjust_label),
      size = tamanho_pt / .pt, fontface = "bold",
      colour = "#222222", family = THESIS_FONT
    ) +
    scale_x +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    # clip="off": o texto centralizado do primeiro bloco excede levemente o
    # domínio e era cortado ("Itamar" virava "amar").
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_void(base_family = THESIS_FONT) +
    ggplot2::theme(plot.margin = ggplot2::margin(2, 5.5, 1, 5.5))
}

# ------------------------------------------------------------------------------
# 4. TEMA BASE — thesis_theme()
# Construído sobre theme_minimal() (Healy Ch. 8: build on existing themes).
# Chamar theme_set(thesis_theme()) UMA VEZ no topo de cada script.
# ------------------------------------------------------------------------------
thesis_theme <- function(base_size = 9.5, base_family = THESIS_FONT) {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      # Título e subtítulo
      plot.title = element_text(
        face = "bold",
        size = base_size + 1.5,
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        size = base_size - 0.5,
        colour = "#555555",
        margin = margin(b = 6)
      ),
      # Caption: 1 linha curta de fonte (notas longas vão no .qmd)
      plot.caption = element_text(
        size = base_size - 2,
        colour = "#666666",
        hjust = 0,
        margin = margin(t = 10)
      ),
      # Eixos
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 1),
      axis.ticks = element_line(colour = "#CCCCCC"),
      # Grid
      panel.grid.major = element_line(colour = "#EEEEEE", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      # Facet strips
      strip.text = element_text(face = "bold", size = base_size),
      strip.background = element_rect(fill = "#F0F0F0", colour = NA),
      # Legenda
      legend.position = "bottom",
      legend.text = element_text(size = base_size - 1),
      legend.key.size = unit(0.9, "lines"),
      legend.title = element_text(size = base_size - 1, face = "bold"),
      # Margem da figura
      plot.margin = margin(8, 8, 8, 8)
    )
}

# ------------------------------------------------------------------------------
# 4b. TEMA ADITIVO — tema_grade_densa()
# Para grades com muitos painéis (facet_wrap com ≥6-9 painéis): soma-se a
# thesis_theme(), não substitui. Codifica as decisões tomadas em 042C_Tese.R
# (NEWS.md 2026-06-30) depois de uma figura real ter saído ilegível por
# excesso de "chrome" (caixa colorida + negrito + cabeçalho de 2 linhas)
# competindo por espaço com os dados:
#   - cabeçalho do painel em 1 linha, sem negrito, sem fundo colorido —
#     texto mais estreito e mais alto-contraste com o conteúdo
#   - fonte de eixo um pouco maior que o padrão de thesis_theme(), porque
#     o espaço liberado pelo cabeçalho compacto é reinvestido ali
# Regra geral mais importante (não é específica desta função): salve a
# figura no TAMANHO FÍSICO REAL de exibição (ex: LARGURA_PAISAGEM se for
# figura paisagem de página cheia), nunca um canvas arbitrariamente maior —
# texto em ggplot é dimensionado em pontos absolutos, não escala com
# `width=100%` no Quarto.
# Uso: thesis_theme() + tema_grade_densa() na cadeia do ggplot.
# ------------------------------------------------------------------------------
tema_grade_densa <- function(
  eixo_size = 12,
  eixo_title_size = 14,
  strip_size = 11.5
) {
  ggplot2::theme(
    axis.text.y = element_text(size = eixo_size),
    axis.text.x = element_text(size = eixo_size),
    axis.title.x = element_text(size = eixo_title_size),
    axis.title.y = element_text(size = eixo_title_size),
    strip.text = element_text(
      size = strip_size, face = "plain",
      margin = margin(t = 3, b = 3)
    ),
    strip.background = element_blank()
  )
}

# ------------------------------------------------------------------------------
# 5. HELPERS DE ESCALA
# ------------------------------------------------------------------------------

# Escala de cor qualitativa (categorias) — mapeamento automático para Okabe-Ito
scale_colour_thesis <- function(...) {
  scale_colour_manual(values = PALETA_QUALITATIVA, ...)
}
scale_fill_thesis <- function(...) {
  scale_fill_manual(values = PALETA_QUALITATIVA, ...)
}

# Escala sequencial (ex: decis coloridos por ranking) — fill e colour
scale_fill_decil <- function(n = 10, ...) {
  scale_fill_manual(values = .paleta_sequencial_hcl(n), ...)
}
scale_colour_decil <- function(n = 10, ...) {
  scale_colour_manual(values = .paleta_sequencial_hcl(n), ...)
}

# Escala divergente discreta (ex: delta +/- por governo) — fill e colour
# 'labels'/'name' são argumentos nomeados (não em '...') para que o chamador
# possa sobrescrevê-los (ex: rótulos em inglês) sem erro de "argumento duplicado".
scale_fill_delta <- function(
  labels = c("TRUE" = "Ganho (+pp)", "FALSE" = "Perda (–pp)"),
  name = NULL,
  ...
) {
  scale_fill_manual(
    values = c("TRUE" = COR_POSITIVO, "FALSE" = COR_NEGATIVO),
    labels = labels,
    name   = name,
    ...
  )
}
scale_colour_delta <- function(
  labels = c("TRUE" = "Ganho (+pp)", "FALSE" = "Perda (–pp)"),
  name = NULL,
  ...
) {
  scale_colour_manual(
    values = c("TRUE" = COR_POSITIVO, "FALSE" = COR_NEGATIVO),
    labels = labels,
    name   = name,
    ...
  )
}

# Escala divergente contínua com ponto médio (ex: mapas coropléticos de
# desigualdade ou delta — Healy Cap. 7: scale_fill_gradient2(midpoint = ...))
scale_fill_diverging_thesis <- function(midpoint = 0, ...) {
  scale_fill_gradient2(
    low = COR_NEGATIVO, mid = "white", high = COR_POSITIVO,
    midpoint = midpoint, ...
  )
}
scale_colour_diverging_thesis <- function(midpoint = 0, ...) {
  scale_colour_gradient2(
    low = COR_NEGATIVO, mid = "white", high = COR_POSITIVO,
    midpoint = midpoint, ...
  )
}

# ------------------------------------------------------------------------------
# 6. CAMADA DE DESTAQUE — "Layer, Highlight, Repeat" (Healy Cap. 8)
# Plota o conjunto completo em cinza claro (contexto) e destaca um subconjunto
# em cor Okabe-Ito por cima. Útil para séries com muitas linhas (decis,
# governos) quando se quer chamar atenção para 1-2 séries sem perder o
# contexto das demais.
#
# Exemplo:
#   camada_destaque(
#     data_fundo    = df,
#     data_destaque = dplyr::filter(df, decil == "D10"),
#     mapping       = aes(x = ano, y = taxa, group = decil),
#     linewidth     = 1.2
#   )
# ------------------------------------------------------------------------------
camada_destaque <- function(
  data_fundo, data_destaque, mapping,
  geom = "line",
  cor_fundo = "grey70",
  alpha_fundo = 0.5,
  cor_destaque = COR_POSITIVO,
  ...
) {
  geom_fn <- get(paste0("geom_", geom), envir = asNamespace("ggplot2"))
  list(
    geom_fn(
      data = data_fundo, mapping = mapping,
      colour = cor_fundo, alpha = alpha_fundo
    ),
    geom_fn(
      data = data_destaque, mapping = mapping,
      colour = cor_destaque, ...
    )
  )
}

# ------------------------------------------------------------------------------
# 6b. FORÇAR FONTE NO MOMENTO DE SALVAR
# Dezenas de scripts já existentes (ex.: 042C_Tese.R, a série 041/042/220/231-
# 234/245D em 2026-06_Harmonizing-BR-Data) fixam `family = THESIS_FONT`
# diretamente em element_text() específicos (plot.title, legend.text, ...) ou
# em geom_text()/annotate() — não apenas no tema herdado. Um valor explícito
# NÃO herda do elemento pai "text", então `plot + theme(text = element_text(
# family = ...))` sozinho não alcança esses casos: seria preciso reescrever
# cada um dos ~30 scripts. Em vez disso, .forcar_fonte_plot() varre o objeto
# ggplot já construído (plot$theme e plot$layers) e sobrescreve todo `family`
# explícito para o nome correto do device de destino — nenhum script precisa
# mudar, incluindo os que já existem.
#
# GOTCHA — patchwork esconde sub-figuras fora de $theme/$layers: quando dois
# ggplots são combinados via `p1 / p2` (ou `+`), o objeto resultante tem
# classe "patchwork" mas SÓ o último plot da cadeia fica acessível como
# $theme/$layers do próprio objeto — os demais ficam em plot$patches$plots
# (uma lista de ggplots completos). Sem tratar isso, sub-figuras combinadas
# (ex.: salvar_grafico(p_principal / p_legenda, ...)) saem com a fonte
# forçada só no painel "ativo"; qualquer painel resolvido separadamente
# (como uma legenda-mapa construída com seu próprio theme_minimal()) mantém
# o nome de fonte errado para o device — no PDF isso não dá erro, cai pra
# Arial silenciosamente, e como o PNG usa outro motor de fonte (systemfonts/
# ragg), pode resolver esse MESMO nome errado corretamente — resultado:
# PNG e PDF saem com fontes diferentes um do outro no mesmo painel (achado
# ao vivo, 2026-07-13, relatado pelo autor comparando as duas saídas).
# Corrigido varrendo plot$patches$plots recursivamente (cobre patchwork
# aninhado, ex.: (p1/p2)/p3).
# ------------------------------------------------------------------------------
.forcar_fonte_plot <- function(plot, fonte) {
  plot <- .forcar_fonte_um_nivel(plot, fonte)
  if (inherits(plot, "patchwork") && !is.null(plot$patches) && length(plot$patches$plots) > 0) {
    plot$patches$plots <- lapply(plot$patches$plots, .forcar_fonte_plot, fonte = fonte)
  }
  plot
}

.forcar_fonte_um_nivel <- function(plot, fonte) {
  if (!is.null(plot$theme) && length(plot$theme) > 0) {
    for (nm in names(plot$theme)) {
      el <- plot$theme[[nm]]
      if (inherits(el, "element_text")) plot$theme[[nm]]$family <- fonte
    }
  }
  if (length(plot$layers) > 0) {
    for (i in seq_along(plot$layers)) {
      geom <- plot$layers[[i]]$geom
      # geom_text()/geom_label() NAO herdam base_family do tema — 'family' e
      # um parametro proprio da camada, default "" (fonte generica do
      # device) se nunca foi setado. Por isso forcamos sempre que o geom
      # aceitar esse aesthetic, nao so quando ja havia algo em aes_params
      # (script novo sem family= explicito) ou ja tinha um valor (padrao
      # antigo com family = THESIS_FONT hardcoded).
      mapping_layer <- plot$layers[[i]]$mapping
      tem_family_mapeado <- !is.null(mapping_layer) && !is.null(mapping_layer$family)
      if (!is.null(geom) && "family" %in% geom$aesthetics() && !tem_family_mapeado) {
        plot$layers[[i]]$aes_params$family <- fonte
      }
    }
  }
  plot + ggplot2::theme(text = ggplot2::element_text(family = fonte))
}

# ------------------------------------------------------------------------------
# 7. SALVAMENTO PADRONIZADO
# ------------------------------------------------------------------------------
salvar_grafico <- function(
  plot,
  prefixo,
  largura = LARGURA_TEXTO,
  altura = ALTURA_PADRAO,
  dir = NULL,
  unidades = "in",
  dpi = DPI_IMPRESSAO,
  formato = "png" # "png" (padrão, compatível com scripts existentes) ou "pdf"
) {
  if (is.null(dir)) dir <- here::here("6-images-tables", "graphs")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  nome <- file.path(
    dir,
    paste0(format(Sys.time(), "%Y-%m-%d_%H%M"), "_", prefixo, ".", formato)
  )
  # Sobrescreve a familia da fonte no MOMENTO de salvar, conforme o device de
  # destino (ver gotcha na secao 1 — GDI/cairo_pdf e systemfonts/ragg leem
  # nomes diferentes para a mesma fonte). device=NULL deixa o ggsave() achar
  # o device raster padrao (ragg::agg_png, se instalado) pela extensao ".png".
  fonte_alvo <- if (formato == "pdf") FONTE_PDF else FONTE_PNG
  plot <- .forcar_fonte_plot(plot, fonte_alvo)
  device <- if (formato == "pdf") {
    function(..., family) grDevices::cairo_pdf(..., family = fonte_alvo)
  } else {
    NULL
  }
  ggplot2::ggsave(nome,
    plot = plot, width = largura, height = altura,
    units = unidades, dpi = dpi, device = device
  )
  cat(sprintf("Salvo: %s\n", nome))
  invisible(nome)
}

# ------------------------------------------------------------------------------
# 8. FINALIZAR FIGURA — pacote PDF + snapshot do .R + snippet .qmd
# "Promoção manual" para o texto da dissertação (chamar UMA VEZ, quando a
# figura estiver pronta — continue iterando com salvar_grafico() normalmente
# até esse momento). Gera, em 6-images-tables/final/<fig_label>/, três
# arquivos com o MESMO nome-base (timestamp + nome do script gerador):
#   <stem>.pdf  — figura vetorial (cairo_pdf, embute a fonte showtext)
#   <stem>.R    — cópia exata do script que gerou esta versão da figura
#   <stem>.qmd  — trecho pronto para copiar e colar no capítulo: imagem +
#                 legenda curta + bloco {=latex} com \begin{fignote}...\end{fignote}
# Os três arquivos compartilharem o nome-base é o que permite, anos depois,
# saber exatamente qual execução de qual script produziu qual PDF citado.
#
# Exemplo:
#   finalizar_figura(
#     plot        = p_combined,
#     fig_label   = "delta-matriculados-decil-governo",
#     fig_cap     = "Change in enrollment rate by decile and term, Brazil, 1992–2023.",
#     fonte       = "IBGE --- PNAD (1992--2011) and PNAD Contínua (2012--2023).",
#     nota        = "Each bar shows the pp-change in 18--24-year-old enrollment rate... 95\\% CI...",
#     script_path = here::here("4-DA-Code", "...", "042C_Tese.R"),
#     largura = 44, altura = 24, unidades = "cm"
#   )
# ------------------------------------------------------------------------------
finalizar_figura <- function(
  plot, fig_label, fig_cap, script_path,
  nota = NULL, fonte = NULL, apendice = NULL,
  largura = LARGURA_TEXTO, altura = ALTURA_PADRAO, unidades = "in"
) {
  nome_script <- tools::file_path_sans_ext(basename(script_path))
  stem <- paste0(format(Sys.time(), "%Y-%m-%d_%H%M"), "_", nome_script)
  dir_fig <- here::here("6-images-tables", "final", fig_label)
  if (!dir.exists(dir_fig)) dir.create(dir_fig, recursive = TRUE)

  # Mesma sobrescrita de familia de fonte que salvar_grafico() aplica para PDF
  # (ver .forcar_fonte_plot() e o gotcha na secao 1 de tipografia) — garante
  # texto selecionavel e a fonte correta no pacote final promovido.
  plot_pdf <- .forcar_fonte_plot(plot, FONTE_PDF)
  caminho_pdf <- file.path(dir_fig, paste0(stem, ".pdf"))
  ggplot2::ggsave(caminho_pdf,
    plot = plot_pdf, width = largura, height = altura,
    units = unidades,
    device = function(..., family) grDevices::cairo_pdf(..., family = FONTE_PDF)
  )

  # PNG irmao, mesmo stem, para HTML/EPUB/DOCX.
  #
  # PDF nao e' imagem para esses formatos: o site publicado renderizava a figura
  # como um LINK para o .pdf, sem <img> nenhum, e o EPUB ficava sem a figura
  # (verificado em 2026-07-26). O filtro filters/figuras-png-fora-do-latex.lua
  # troca a extensao no render e so' age se o PNG existir, de modo que figuras
  # ainda nao regeradas continuam com o comportamento antigo.
  #
  # Fonte DIFERENTE da do PDF, e nao e' descuido: cairo_pdf le o nameID 1 da
  # fonte ("LM Roman 10") e os devices raster de systemfonts leem o nameID 16
  # ("Latin Modern Roman"). Passar o nome errado nao da erro — troca a fonte em
  # silencio. Ver a secao de tipografia no topo deste arquivo.
  plot_png <- .forcar_fonte_plot(plot, FONTE_PNG)
  caminho_png <- file.path(dir_fig, paste0(stem, ".png"))
  ggplot2::ggsave(caminho_png,
    plot = plot_png, width = largura, height = altura,
    units = unidades, dpi = DPI_IMPRESSAO, device = NULL
  )

  # ── Cópia plana em graphs/ (decisão do autor, 2026-08-09) ──────────────────
  # O PNG acima fica em final/<fig_label>/, uma pasta por figura. Com dezenas
  # de rótulos, achar "a figura que acabei de gerar" exige abrir uma pasta por
  # vez — na rodada de extensão para 2025 foram doze pastas diferentes. Uma
  # cópia com o MESMO stem na raiz de graphs/ resolve: a pasta de rascunho
  # passa a listar, em ordem cronológica, tudo o que foi gerado, promovido ou
  # não. graphs/ é gitignored, então isto não versiona nada a mais.
  # O nome carrega o FIG_LABEL, não só o stem: um mesmo script pode promover
  # várias figuras no mesmo minuto (o 245D promove quatro variantes de
  # composição), e nesse caso o stem — timestamp + nome do script — é idêntico
  # entre elas. Sem o rótulo, as três últimas sobrescrevem a primeira e a pasta
  # passa a mostrar a figura errada sob um nome plausível, que é pior do que
  # não ter cópia nenhuma.
  caminho_png_flat <- here::here(
    "6-images-tables", "graphs",
    paste0(stem, "__", fig_label, ".png")
  )
  dir.create(dirname(caminho_png_flat), showWarnings = FALSE, recursive = TRUE)
  file.copy(caminho_png, caminho_png_flat, overwrite = TRUE)

  caminho_r <- file.path(dir_fig, paste0(stem, ".R"))
  file.copy(script_path, caminho_r, overwrite = TRUE)

  # `nota` deve ser texto LaTeX-pronto e enxuto (Sec 13.7 WRITING-STYLE.md:
  # ate ~5 linhas impressas; escapa %, usa \texttt{}, etc.). `apendice`, se
  # informado (ex.: "sec-fignote-meu-slug"), apenda automaticamente o link
  # hyperref para a entrada da figura em "Extended Figure Notes". `fonte` vai
  # para o final do fignote (nao para o caption).
  fignote_content <- {
    nota_txt <- if (!is.null(nota)) nota else ""
    apendice_txt <- if (!is.null(apendice)) {
      paste0(
        " \\hyperref[", apendice, "]{Appendix~\\ref*{", apendice, "}}."
      )
    } else {
      ""
    }
    fonte_txt <- if (!is.null(fonte)) paste0(" Source: ", fonte, ".") else ""
    paste0(nota_txt, apendice_txt, fonte_txt)
  }
  fignote_block <- if (nchar(fignote_content) > 0) {
    paste0(
      "\n\n```{=latex}\n\\begin{fignote}\n",
      fignote_content,
      "\n\\end{fignote}\n```"
    )
  } else {
    ""
  }
  qmd_txt <- paste0(
    "![", fig_cap, "](../../6-images-tables/final/", fig_label, "/",
    stem, ".pdf){#fig-", fig_label, " width=100%}", fignote_block, "\n"
  )
  caminho_qmd <- file.path(dir_fig, paste0(stem, ".qmd"))
  writeLines(qmd_txt, caminho_qmd, useBytes = TRUE)

  cat(sprintf(
    "[finalizar_figura] Pacote salvo em %s/ com stem '%s' (.pdf/.R/.qmd)\n",
    dir_fig, stem
  ))
  invisible(list(pdf = caminho_pdf, r = caminho_r, qmd = caminho_qmd))
}

# ------------------------------------------------------------------------------
# 9. MODELO DE BLOCO DE COMENTÁRIO PARA FIGURAS NO .QMD
# Copiar e colar ANTES de cada ggsave() / salvar_grafico().
# Preencher os campos entre colchetes. Nota metodologica vai em bloco
# {=latex} com \begin{fignote}...\end{fignote} apos a figura no .qmd.
# ------------------------------------------------------------------------------
#
# ── LEGENDA / DRAFT PARA O QUARTO ────────────────────────────────────────────
# fig-cap:   "[Medida] by [estratificacao], Pais, YYYY--YYYY."  (sem "Source:";
#            intervalo = anos efetivamente plotados, nunca uma decada nominal)
# fig-label: fig-[slug-unico, ex: delta-decil-lula-i]
# nota (ENXUTA, ate ~5 linhas impressas — WRITING-STYLE.md Sec 13.7):
#   "[(i) o que cada elemento visual mostra; (ii) variavel/recorte em 1 frase;
#    (iii) metodo do IC em 1 oracao — NADA de codigos de decisao (D01, D02...),
#    variaveis de survey (V6002...) ou minucias de bootstrap: isso vai na
#    entrada da figura em Extended Figure Notes, nao aqui]"
# apendice: "sec-fignote-[slug-unico]"  (cria o hyperref automaticamente —
#           criar a entrada correspondente no apendice ANTES de rodar)
# fonte: "IBGE --- PNAD (anos) and PNAD Continua (anos)."
#        (aparece no final do fignote, nao no caption)
# Variaveis-chave: [ex: ens_sup_a (D19), decil (D02, D03), peso]
# Referencia cruzada no texto: @fig-[slug-unico]
# Dimensoes usadas: LARGURA_TEXTO × ALTURA_ALTA  (6.27 × 5.00 in, 300 dpi)
# Paleta usada: [PALETA_QUALITATIVA / COR_POSITIVO+COR_NEGATIVO / PALETA_SEQUENCIAL]
# Eixo de anos: scale_x_anos_tese(anos = df$ano)  (WRITING-STYLE.md Sec 13.8)
# ─────────────────────────────────────────────────────────────────────────────

message(
  "[plot_theme] Carregado. Fonte PNG: '", FONTE_PNG, "' | Fonte PDF: '", FONTE_PDF, "'. ",
  "Chamar theme_set(thesis_theme()) no topo do script."
)
