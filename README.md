# Open Data Analysis

Catálogo aberto de análises de dados sobre educação superior, mercado de
trabalho e desigualdade no Brasil. Cada post é uma figura publicada,
acompanhada do pipeline completo que a produz — do dado bruto até o gráfico
final. O objetivo é reprodutibilidade de ponta a ponta: qualquer pessoa deve
conseguir baixar os dados públicos, rodar os scripts na ordem indicada, e
obter a mesma figura.

Este repositório também serve como pacote de replicação para a dissertação
de mestrado do autor (educação superior e estratificação social no Brasil).
Um subconjunto das análises aqui publicadas é citado diretamente no texto da
dissertação; esse subconjunto será congelado numa tag/release Git antes da
defesa, para que a versão citada permaneça estável mesmo que o repositório
continue crescendo depois.

## Estrutura

```
open-data-analysis/
├── shared-pipeline/     # scripts upstream compartilhados por várias figuras
│   ├── pnadc/           # download, harmonização e splicing de PNAD/PNAD Contínua
│   ├── censo/           # pontos de validação via Censo Demográfico
│   ├── censup/          # download e organização de microdados do Censo da Educação Superior
│   ├── sedap/           # cliente e extração via SEDAP+ (INEP, dado restrito)
│   └── ipca/            # série de mensalidades via IPCA/SIDRA
├── posts/
│   └── <fig-label>/
│       ├── index.qmd    # página do post (frontmatter + pipeline + reprodutibilidade)
│       ├── plot.R       # script canônico que gera a figura
│       └── thumbnail.png  # versão publicada da figura
├── data-raw/            # (gitignored) dado bruto/intermediário escrito por shared-pipeline/
└── output/              # (gitignored) figuras geradas ao rodar um plot.R
```

## Como rodar

1. Instale os pacotes R usados pelos scripts — entre os mais comuns:
   `tidyverse`, `arrow`, `here`, `survey`, `PNADcIBGE`, `censobr`, `sidrar`,
   `ipeadatar`, `httr2`, `archive`, `data.table`, `ggh4x`, `patchwork`. Cada
   script declara seus próprios `library()` no topo — instale conforme os
   erros de pacote ausente aparecerem.
2. Abra o projeto pela raiz (o `.Rproj` ou `here::here()` apontando para esta
   pasta). Todos os caminhos são relativos à raiz: os scripts de
   `shared-pipeline/` criam `data-raw/` e escrevem os dados brutos e
   intermediários ali; os `plot.R` leem de `data-raw/` e gravam a figura em
   `output/figures/<fig-label>/`. Nenhum script depende de caminho fora do
   repositório.
3. Rode os scripts upstream **na ordem listada na seção "Pipeline" do post**
   que você quer reproduzir (cada post lista a cadeia completa, do download ao
   dado intermediário, e o que cada script grava). Para reconstruir de uma vez
   todos os dados públicos (Tier A), rode
   `shared-pipeline/build_public_data.R`.
4. Só então rode o `plot.R` do post em `posts/<fig-label>/`. Compare o PNG
   gerado em `output/figures/<fig-label>/` com o `thumbnail.png` do post — é a
   versão publicada.

Rodar a cadeia PNAD/PNADC completa baixa alguns GB de microdados do IBGE e
leva horas; o script `000` sozinho leva 30–90 minutos. Os caches em
`data-raw/` evitam repetir downloads.

## Tiers de reprodutibilidade

Cada post é classificado num tier que descreve o grau de acesso necessário
para reproduzi-lo do zero:

| Tier | Significado |
|---|---|
| **A** | Usa apenas dados públicos, acessíveis via pacote/API sem necessidade de credencial (PNADcIBGE, censobr, sidrar, ipeadatar). Rodar os scripts de `shared-pipeline/` na ordem numérica reconstrói os dados intermediários. |
| **A (cross-repo)** | Depende do pacote R público `educabr2` (`remotes::install_github("mancano-tales/educabr2")`), que já embute os dados necessários — nada para baixar manualmente. |
| **B** | Depende de dado restrito do INEP acessível via credencial SEDAP+ (variável de ambiente `SEDAP_TOKEN`). Sem uma credencial aprovada pelo INEP, os scripts em `shared-pipeline/sedap/` não rodam — a lógica fica disponível para auditoria, mas a reprodução completa exige solicitar acesso. |

## Projeto vivo

Este catálogo cresce ao longo do tempo com novas análises — não é um
snapshot estático de uma única dissertação. Novos posts são adicionados
conforme novas figuras são produzidas. Um post é removido quando a figura é
**superada** por uma versão mais recente da mesma análise (por exemplo, as
variações de renda por mandato em termos relativos e absolutos, de julho de
2026, foram substituídas pela versão anualizada, que exclui o mandato Itamar
e inclui Lula III); o histórico fica no Git. Posts também podem ser
reclassificados de tier se uma fonte de dado mudar de status de acesso.

## Como citar

Cada post traz uma seção "How to Cite" / "Como Citar" em APA 7ª edição, com
a referência da dissertação, a referência da figura específica (com a URL da
página) e entradas BibTeX com chave única por figura
(`Mancano2026_<fig-label>`). Até a defesa, cite a URL da página e a data de
acesso; depois, uma tag do Git e um DOI no Zenodo serão linkados aqui.

## Site bilíngue

O catálogo é publicado em dois idiomas a partir do mesmo repositório, via perfis do Quarto:

- **Inglês** (padrão): `quarto render --profile en` (ou simplesmente `quarto render`) publica em `docs/`.
- **Português**: `quarto render --profile pt` publica em `docs/pt/`.

Os dois perfis (`_quarto-en.yml`, `_quarto-pt.yml`) herdam a configuração comum de `_quarto.yml` e definem apenas o que é específico de idioma (título/descrição do site, `lang`, `output-dir` e o link de troca de idioma na navbar).

Os posts em português vivem em `posts-pt/<fig-label>/index.qmd` e referenciam os mesmos `plot.R` e `thumbnail.png` já publicados em `posts/<fig-label>/` — os arquivos de script e imagem **não são duplicados**, apenas linkados com caminho relativo (`../../posts/<fig-label>/...`). Para atualizar uma figura, basta editar o script/imagem uma única vez em `posts/<fig-label>/`; os dois posts (EN e PT) passam a apontar para a versão atualizada automaticamente. Ao criar uma nova figura em `posts/`, replique a página em `posts-pt/` traduzindo Overview/Metodologia e ajustando os caminhos relativos.

## Licença

MIT — ver [`LICENSE`](LICENSE).
