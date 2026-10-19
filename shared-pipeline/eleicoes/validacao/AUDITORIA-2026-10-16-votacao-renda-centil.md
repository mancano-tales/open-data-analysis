# Auditoria metodológica — post "Votação presidencial por centil de renda dos setores censitários"

- **Data:** 2026-10-16
- **Auditor:** Claude Code / Claude Opus 5.5 / desktop
- **Implementador designado:** Codex (quem escreve não revisa: as correções voltam ao Claude para revisão)
- **Escopo:** `posts/votacao-renda-centil/index.qmd`, `posts-pt/votacao-renda-centil/index.qmd`,
  `posts/votacao-renda-centil/plot.R`, `shared-pipeline/eleicoes/010_Download_Censo_TSE.R`,
  `020_Cruzar_Setores_Votos.R`, `030_Verificar_Resultados.R` e os registros de cobertura em
  `data-raw/eleicoes-renda/` (`auditoria-ligacao.csv`, `cobertura-locais.csv`,
  `locais-conflitantes.csv`, `secoes-sem-local.csv`, `cobertura-setores.csv`, `sensibilidade.csv`).
- **Fora do escopo:** o explorador eleitoral (`agregacao/`, `historico/`), que tem pipeline e
  auditorias próprias. Alguns achados abaixo (A3, A4) podem valer também para ele; verificar
  antes de estender.
- **Método:** leitura integral do código e do texto publicado (rascunho, `draft: true`),
  conferência dos números do texto contra os CSVs de cobertura. Nenhum script foi reexecutado
  nesta auditoria; os achados de execução são de leitura de código.

---

## 1. Síntese

O post reconstrói, com dados públicos, a curva da Folha de S.Paulo (12/10/2026) que relaciona a
renda dos setores censitários do Censo 2022 à votação de Lula (PT) e Flávio Bolsonaro (PL) no 1º
turno presidencial de 2026. A concepção é cuidadosa: não confunde seção eleitoral com setor
censitário, declara a natureza ecológica e descritiva da associação, privilegia o código de local
registrado na apuração sobre o cadastro, documenta exclusões e publica uma análise de
sensibilidade.

Os problemas estão em três pontos:

1. **A regra de ligação espacial** (local mais próximo do setor, até 5 km) mede proximidade, não o
   local onde os moradores votam, e a exclusão por distância é seletiva por renda e ruralidade.
2. **A agregação não é ponderada** em dois níveis (setores e locais), e o post não informa que
   fração dos votos válidos está efetivamente representada na curva.
3. **A verificação independente (`030`) cobre a aritmética, mas não a etapa espacial**, que é a
   mais arriscada do pipeline.

Nenhum achado invalida o post como exercício descritivo. Os itens A1–A4 devem ser tratados antes
de tirar o rascunho do ar (`draft: false`).

---

## 2. Descrição técnica do que foi desenvolvido

### 2.1 Insumos (`010_Download_Censo_TSE.R`)

| Insumo | Fonte | Observação |
|---|---|---|
| Renda setorial | `censobr::read_tracts(year = 2022, dataset = "ResponsavelRenda")` | Schema conferido (`code_tract`, `code_muni`, `V06001`, `V06004`); cache `renda2022.rds` |
| Votos por seção | ZIP `votacao_secao_2026_BR.zip` via API CKAN do TSE (`resultados-2026`) | Regex precisa casar exatamente um recurso; download atômico (`.part` → rename); ZIP validado |
| Cadastro de locais | ZIP `eleitorado_local_votacao_2026.zip` (`eleitorado-2026`) | Usado só para auditoria |
| Proveniência | `manifest.csv` (URL, MD5, data), `session-download.txt` | Boa prática |

- `V06004` = rendimento nominal médio mensal das pessoas responsáveis **com rendimentos** por
  domicílios particulares permanentes ocupados.
- `V06001` = número de pessoas responsáveis.

### 2.2 Votos por local (`020`, primeira parte)

1. Filtro: `ANO_ELEICAO == 2026`, `NR_TURNO == 1`, `CD_CARGO == 1`, `SG_UF != "ZZ"`.
2. Votos válidos: `0 < NR_VOTAVEL < 95` (exclui brancos 95 e nulos 96+). Denominador = todos os
   candidatos; numeradores = 13 (Lula) e 22 (Flávio).
3. Chave de seção `(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_SECAO)`; falha se houver duplicata.
4. Junção com o cadastro só para auditoria: 497.897 seções, 0 sem cadastro, **6.588 com local
   divergente**. Prevalece `NR_LOCAL_VOTACAO` do arquivo de resultados.
5. Soma por local `(UF, município, zona, local)` e cálculo do percentual do local como **razão das
   somas** (correto dentro do local).

### 2.3 Georreferenciamento dos locais

1. `geobr::read_polling_places(year = 2026)`, filtrado pela data do 1º turno (`dt_eleicao`).
2. Chave `place_key = UF:código TSE do município:zona:local`.
3. Locais com coordenadas conflitantes descartados (0 casos); duplicatas idênticas consolidadas.
4. Excluídos: sentinela (-1,-1) e pontos fora do retângulo do Brasil.
5. Ponto precisa estar dentro do polígono do próprio município (malha 2022, EPSG:5880).
   Resultado: 91.087 locais com votos e coordenadas; **818 excluídos**; 90.269 elegíveis.

### 2.4 Ligação setor → local

1. Setor elegível: `V06001 > 0` e `V06004 > 0` finito.
2. Geometria do setor (`geobr::read_census_tract(2022)`), ponto interno `st_point_on_surface`
   calculado em EPSG:5880 e reprojetado para 4326 (cache com chave MD5 + versões do sf/GEOS).
3. Para cada município, `st_nearest_feature(setor, locais do mesmo município)`, com locais
   ordenados pela chave para desempate determinístico.
4. Distância geodésica (`st_distance`, s2) por elemento; o setor entra se `distância ≤ 5000 m`
   (`VOTOS_MAX_DIST_M`).
5. Cobertura: 458.772 setores na tabela; 9.241 sem renda elegível; 39.483 além da distância;
   38 sem local/malha; **410.010 incluídos (89,37%)**.

### 2.5 Centis

1. Ordenação por `V06004`, desempate por `code_tract`.
2. `centil = floor((i - 1) * 100 / N) + 1` → 100 grupos com 4.100 ou 4.101 setores.
3. Por centil: média simples de `p_lula`, `p_flavio` e de `V06004 / 1212` (salário mínimo de 2022).
4. `sensibilidade.csv`: mesma rotina para 1, 2 e 5 km, **recalculando amostra e cortes** em cada limite.

### 2.6 Verificação (`030`)

- Recalcula percentuais por local a partir do CSV bruto com `tapply`, sem passar pela junção
  com o cadastro.
- Refaz os 100 grupos e as médias com R base; confere [0,100], soma ≤ 100, contagens iguais
  (diferença ≤ 1), unidades de renda.
- Grava `versoes.csv` e `checksums.csv`.

### 2.7 Resultados publicados

- Centil 46: Lula 42,35%, renda 1,68 SM (referência da Folha lida na imagem: 41,12% e 1,69).
- Centil 100: Lula 38,90%, Flávio 49,53%, renda 16,52 SM.
- Flávio supera Lula a partir do centil 35.
- Maior diferença entre as curvas de 1 km e 5 km: 6,32 p.p. (Lula), 5,51 p.p. (Flávio).

---

## 3. Achados

Severidade: **Alta** = pode mudar a leitura do gráfico; **Média** = afeta a robustez ou a
transparência; **Baixa** = aprimoramento.

### A1 — Cobertura de votos não é informada (Alta)

**Problema.** O post informa a cobertura de setores (89,37%), mas não a de votos. Um local entra na
curva só se for o mais próximo de ao menos um setor incluído. Locais que não são o mais próximo de
nenhum setor (comuns em áreas densas com várias escolas próximas) **desaparecem**, com todos os
seus votos. Não se sabe que fração dos ~votos válidos nacionais está representada.

**Evidência.** `020`, laço por município: `nearest <- st_nearest_feature(tr, pp)`; nenhum registro
de quantos locais/votos foram usados. Os CSVs de cobertura tratam só de setores e locais
geocodificados.

**Correção.**
- Em `020`, após `links`, calcular e gravar `cobertura-votos.csv` com: locais com votos; locais
  geocodificados válidos; locais ligados a ≥ 1 setor incluído; votos válidos, votos de Lula e de
  Flávio em cada um desses universos, e as respectivas frações do total nacional (sem ZZ).
- Mesmo cálculo por UF (`cobertura-votos-uf.csv`).
- Comparar o percentual nacional de Lula/Flávio nos locais usados com o resultado oficial
  nacional; registrar a diferença.
- Citar no post a cobertura de votos ao lado da de setores.

**Aceite.** Arquivos gerados; `030` confere que as somas batem com o CSV bruto; o texto do post
(PT e EN) traz a cobertura de votos e a diferença em relação ao resultado oficial.

### A2 — Exclusão por distância é seletiva e o efeito não está isolado (Alta)

**Problema.** Os 39.483 setores além de 5 km são, em grande parte, rurais e de baixa renda. Sua
exclusão altera sobretudo a cauda inferior. A sensibilidade atual mistura dois efeitos, porque
em cada limite a amostra e os cortes dos centis são recalculados: não se separa "mudou o conjunto
de setores" de "mudou a votação atribuída aos mesmos setores".

**Correção.**
- Gravar no `tracts` a renda e a situação (urbana/rural, se disponível em `geobr`/`censobr`) e
  tabular os excluídos por distância por decil de renda nacional e por UF
  (`exclusoes-distancia.csv`).
- Nova sensibilidade com **cortes fixos**: definir os centis no universo de todos os setores com
  renda elegível e ligação a qualquer distância; para cada limite (1, 2, 5, 10 km e sem limite),
  calcular a média só dos setores incluídos, mantendo o centil de origem. Gravar
  `sensibilidade-cortes-fixos.csv` com `n_setores` por centil e limite.
- Testar o cruzamento das curvas (centil em que Flávio supera Lula) em cada limite e registrar.

**Aceite.** O post informa o perfil de renda dos excluídos e mostra (tabela ou figura de apoio) a
curva com cortes fixos; a afirmação "Flávio supera Lula no centil 35" passa a dizer em que limites
vale.

### A3 — Médias não ponderadas em dois níveis (Alta)

**Problema.**
- **Setores:** cada setor pesa 1, independentemente de `V06001` (domicílios). O texto avisa que
  os centis não são centésimos da população, mas o leitor tende a lê-los assim, e a comparação
  com a Folha pode depender disso.
- **Locais:** o percentual de um local se repete em todos os setores ligados a ele. Um local
  cercado de muitos setores pequenos pesa muito; a média de razões não é a razão das somas.

**Correção.** Produzir, em paralelo à série atual (que deve continuar existindo para comparação):
1. **Centis ponderados por domicílios:** cortar os centis pela soma acumulada de `V06001`
   (cada centil ≈ 1% dos domicílios com responsável com renda) e usar `V06001` como peso nas
   médias de votação e de renda.
2. **Razão das somas por centil:** distribuir os votos de cada local entre os setores ligados a
   ele proporcionalmente a `V06001` e calcular, por centil, `soma(votos do candidato) /
   soma(votos válidos)`. Isso evita que um local conte várias vezes.
- Gravar `centis-ponderados.csv` e `centis-razao.csv`; incluir comparação com a série atual.

**Aceite.** `030` reproduz as duas novas séries com R base; o post explica qual série a figura
principal mostra e por quê, e informa a diferença máxima entre as séries.

### A4 — Proximidade não é o local de votação real (Média)

**Problema.** O eleitor vota onde está inscrito, não necessariamente no local mais próximo de
casa. Em metrópoles e para quem mudou de endereço, o local mais próximo pode não ter relação
com os moradores do setor. O post fala em "vizinhança eleitoral", mas não discute esse viés nem
testa uma regra alternativa.

**Correção.**
- Implementar ao menos uma ligação alternativa e comparar com a atual:
  (a) k vizinhos mais próximos (k = 3) com pesos inversos à distância, ou
  (b) polígonos de Voronoi dos locais recortados pelo município, com interseção de área entre
  setor e polígono.
- Gravar `sensibilidade-ligacao.csv` (centil × regra) e reportar a diferença máxima.
- No texto, um parágrafo curto sobre o viés de inscrição (domicílio eleitoral ≠ residência atual).

**Aceite.** Série alternativa gerada e verificada; o post cita a diferença entre as regras.

### A5 — A verificação não cobre a etapa espacial (Média)

**Problema.** O `030` parte de `setores-votos.rds` e confere a aritmética, mas aceita sem conferir
`place_key` e `distancia_m`. Erros de CRS, de filtro por município ou de desempate não seriam
detectados.

**Correção.** Em `030` (ou em `validacao/`):
- Sorteio fixo (semente registrada) de, por exemplo, 2.000 setores incluídos, estratificado por UF.
- Para cada um, recalcular o local mais próximo por outro caminho: distância euclidiana em
  EPSG:5880 contra todos os locais elegíveis do município (força bruta, sem `st_nearest_feature`).
- Exigir mesmo `place_key` (salvo empate documentado) e diferença de distância < 1% (geodésica ×
  projetada).
- Conferir também que todo `place_key` incluído existe no CSV bruto e na lista de locais elegíveis.

**Aceite.** Teste falha se alguma divergência não explicada aparecer; resultado registrado em
`verificacao-espacial.csv`.

### A6 — Safras diferentes de renda, malha e votos (Baixa)

**Problema.** Renda e malha são de 2022; votos e locais, de 2026. Novos loteamentos, setores
alterados e mudanças de município não são capturados. 818 locais caíram fora da malha municipal
de 2022.

**Correção.** Uma frase no post; tabular os 818 locais excluídos por UF e votos válidos
(`locais-fora-municipio.csv`) para saber quanto pesam.

### A7 — Interpretação de `V06004` na base da distribuição (Baixa)

**Problema.** `V06004` considera só responsáveis com rendimento. Setores pobres com muitos
responsáveis sem renda parecem mais ricos do que são, o que comprime a cauda inferior.

**Correção.** Se o dataset `ResponsavelRenda` trouxer o número de responsáveis com rendimento,
calcular a fração sem rendimento por setor e reportar a média por centil; uma frase no post.
Verificar no dicionário do IBGE qual variável traz essa contagem; não supor o nome.

### A8 — Validação externa fraca (Baixa)

**Problema.** A comparação com a Folha é um único ponto (centil 46), lido de uma imagem.

**Correção.** Se a reportagem ou o material de apoio da Folha publicar mais pontos ou os dados,
comparar a curva inteira. Caso contrário, ler na imagem 5 a 10 pontos (centis 1, 10, 25, 46, 75,
90, 100), registrar em `referencia-folha.csv` com a origem e reportar o erro médio. Deixar claro que
são leituras visuais.

### A9 — Ausência de incerteza (Baixa)

**Problema.** Não há bandas. O ponto de cruzamento das curvas pode ser sensível ao acaso da
composição dos centis.

**Correção.** Bootstrap por município (reamostrar municípios com reposição, 500 réplicas, semente
fixa), recalculando os centis; reportar intervalo de 95% por centil e para o centil de cruzamento.
Opcional na figura (faixa clara).

### A10 — Detalhes de código (Baixa)

- `030`: o filtro sobre o CSV bruto não inclui `ANO_ELEICAO == 2026`. Hoje é inofensivo (arquivo
  só de 2026), mas deve espelhar o `020`.
- `020`: os valores 13 e 22 e o rótulo "Flávio Bolsonaro (PL)" estão embutidos no código.
  Conferir com `NM_VOTAVEL` no próprio arquivo e falhar se o nome não bater.
- `020`: o salário mínimo 1.212 aparece como número solto em duas funções; usar uma constante
  nomeada com comentário (referência: julho de 2022).

---

## 4. Plano de implementação sugerido para o Codex

Ordem recomendada (cada passo em commit próprio, Conventional Commits, trailer `Agent:`):

1. **A10** (preparação): constantes, filtro de ano no `030`, checagem de `NM_VOTAVEL`.
2. **A1**: cobertura de votos nacional e por UF.
3. **A5**: verificação espacial por amostra fixa.
4. **A3**: séries ponderada e de razão das somas, com reprodução no `030`.
5. **A2**: perfil dos excluídos e sensibilidade com cortes fixos.
6. **A4**: regra de ligação alternativa.
7. **A6–A9**: tabelas auxiliares, referência da Folha, bootstrap.
8. Atualizar `posts/votacao-renda-centil/index.qmd` e `posts-pt/...` (mesmo conteúdo nas duas
   línguas) e `data/README.md`. **Não** mudar `draft: true` sem pedido do autor.

Restrições:

- Não editar `NEWS.md`.
- Caminhos relativos; nada de dados brutos novos versionados fora de `data-raw/` e `output/`.
- Tempo de execução: o cruzamento nacional é longo; usar o cache existente e rodar em segundo plano.
- Abrir PR para revisão do Claude, citando este relatório; não mergear sem revisão.
- Toda nova saída numérica precisa ser reproduzida por um cálculo independente no `030`.

## 5. Critérios de encerramento

- Todos os achados A1–A5 resolvidos ou com justificativa registrada no PR.
- `030` passa e cobre as novas saídas.
- O post informa cobertura de votos, série escolhida e sensibilidade com cortes fixos.
- O aviso temporário no `AGENTS.md` (seção "Específico deste repositório") é **removido** no mesmo
  PR que concluir a implementação.
