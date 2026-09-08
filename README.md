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
└── posts/
    └── <fig-label>/
        ├── index.qmd    # página do post (frontmatter + pipeline + reprodutibilidade)
        ├── plot.R       # script canônico que gera a figura
        └── thumbnail.png
```

## Como rodar

1. Instale os pacotes R usados pelos scripts — entre os mais comuns:
   `tidyverse`, `arrow`, `here`, `survey`, `PNADcIBGE`, `censobr`, `sidrar`,
   `ipeadatar`, `httr2`, `archive`, `data.table`, `ggh4x`, `patchwork`. Cada
   script declara seus próprios `library()` no topo — instale conforme os
   erros de pacote ausente aparecerem.
2. Crie uma pasta `data-raw/` na raiz do projeto. Os scripts de
   `shared-pipeline/` escrevem e leem os dados brutos/intermediários ali.
3. Rode os scripts de `shared-pipeline/` na ordem numérica indicada pelo
   prefixo do nome do arquivo (ex.: `pnadc/000_...` antes de
   `pnadc/035_...`), dentro de cada subpasta de fonte. Isso baixa e
   harmoniza os microdados necessários em `data-raw/`.
4. Só então rode o `plot.R` de um post específico em `posts/<fig-label>/`.

Cada post documenta, na sua própria página (`index.qmd`), exatamente quais
scripts de `shared-pipeline/` ele depende — não é preciso rodar tudo para
reproduzir uma figura isolada.

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
conforme novas figuras são produzidas; posts antigos não são removidos, só
eventualmente reclassificados de tier se uma fonte de dado mudar de status
de acesso.

## Licença

MIT — ver [`LICENSE`](LICENSE).
