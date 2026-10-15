# Agregação em centis nacionais fixos

`centis.R` não baixa dados nem modifica insumos ao ser carregado.

```r
source("shared-pipeline/eleicoes/agregacao/centis.R")
universe <- eleicao_income_universe(readRDS("data-raw/eleicoes-renda/renda2022.rds"))
dataset <- eleicao_aggregate_contest(
  votes = votos_normalizados,
  universe = universe,
  links = ligacoes_espaciais,
  year = 2026, round = 1, office = 1, uf = "BR",
  sources = list(list(name = "TSE", url = "https://dadosabertos.tse.jus.br/"))
)
item <- eleicao_write_dataset(dataset, "output/eleicoes/2026-1-1-BR.json")
```

`votes` segue o contrato de `historico/tse.R`. Pode conter linhas de seção
ou linhas já somadas por local. Precisa de `year`, `round`, `uf`,
`municipality_tse`, `zone`, `place`, `office`, `candidate_id`,
`candidate_name`, `votes` e `vote_type`. Se existir, `place_key` é utilizado;
caso contrário a chave é construída como `UF:municipality_tse:zone:place`.
Os tipos de voto aceitos são `candidate`, `party`, `special` e `invalid_code`.
Votos positivos sem classificação são recusados.

Por padrão, a agregação exige `attr(votes, "vote_audit")` com `status`
igual a `reconciled`, método e tabela `checks` por ano/turno/cargo/UF.
Essa tabela contém `official_valid_votes`, `computed_valid_votes` e
`difference`, e deve cobrir todas as UFs da disputa, com diferenças zero.
As somas do cache recebido também são comparadas à auditoria anexada.
Nenhuma curva, inclusive presidencial, pode ser exportada pelo orquestrador
sem essa reconciliação. A opção `require_reconciled = FALSE` existe somente
para fixtures e grava status `unreconciled` nos metadados; não é usada no
orquestrador de publicação.

`links` contém `code_tract`, `place_key`, `distancia_m`, com uma linha por
setor; os códigos são normalizados para texto. Ligações que cruzam UFs são
recusadas. A construção espacial continua sendo responsabilidade de outro
componente, inclusive a validade do município e do ano das coordenadas.

O universo é criado uma vez a partir de **todos** os setores de renda
elegível do Censo 2022. Os centis são nacionais, com grupos iguais de setores
e desempate pelo código. Filtros de UF/cargo/ano não recalculam o ranking.
A renda de cada ponto considera o grupo completo da UF ou do Brasil,
conforme a série, antes das exclusões por distância e disponibilidade de votos.

O argumento `income_reference = "state"` de `eleicao_aggregate_contest()`
reordena o universo completo da UF por renda e código do setor, formando
cem grupos dentro daquela UF, também antes das exclusões. O padrão do núcleo,
`"national"`, preserva os resultados anteriores. Referência estadual exige UF
explícita; não existe uma série estadual para Brasil. A votação continua sendo
uma média de proporções, e não uma contagem de votos nas faixas de renda.
`percentile_scope` identifica Brasil (`BR`) ou a UF e `income_reference`
identifica a classificação. Séries estaduais têm sufixo `-state` no ID.

Para exportar ambas as referências de 2026 sem repetir o processamento nacional:

```r
targets <- eleicao_targets(years = 2026L, rounds = 1L,
                          income_references = "state")
eleicao_build_explorer(targets = targets, incremental = TRUE,
                      flush_progress = TRUE)
```

Os seletores usam a referência estadual como padrão para cargos estaduais,
mantendo a nacional para presidência e para comparação entre territórios.

A agregação primeiro soma os votos por local/candidato, calcula o
denominador completo do cargo/local e conta setores por local/centil.
Depois multiplica percentuais por essa contagem. Assim evita materializar
uma tabela candidato × setor. O custo ainda depende da quantidade de
candidatos e locais; para deputados deve-se processar uma UF/cargo por vez.

Presidência admite exportação nacional e por UF. Os cargos estaduais exigem
UF explícita. Locais sem votos válidos positivos para o cargo são excluídos;
um candidato ausente num local com denominador válido recebe zero.
Grupos sem cobertura recebem `votacao: null`, mantendo `n_setores: 0` e o
total de setores do universo. Percentuais de senadores são participações nos
votos válidos, sem conversão para percentual de eleitores.

Nos cargos proporcionais, legenda integra o denominador. O padrão exporta
apenas candidatos nominais; `include_party_series = TRUE` também exporta
séries dos votos dados diretamente à legenda. Essas séries não são o total
partidário, que exigiria somar votos nominais usando a filiação histórica do
cadastro de candidatos.

O JSON contém `metadata`, `candidates` e `points`, em formato de linhas.
`eleicao_write_dataset()` retorna um item de manifesto com URL, ano, turno,
código e rótulo do cargo, UF e estado `available`. O chamador reúne os itens
no manifesto completo. Nenhum item indisponível é inventado pelo exportador.

Execute a verificação com fixtures da raiz do repositório:

```sh
Rscript shared-pipeline/eleicoes/agregacao/tests.R
```

O teste compara a agregação por pesos com uma implementação independente
que cruza cada setor na fixture. Verifica também ausência versus zero,
legenda, senador, ranking fixo, preservação da renda do universo e JSON.

## Exportação das séries disponíveis

```sh
Rscript shared-pipeline/eleicoes/agregacao/build-explorer.R
```

O orquestrador lê caches `historico/places-ANO-UF-CARGOS.rds` produzidos pelo
normalizador versão 2 e prefere `places-ANO-UF-CARGOS-legal.rds`, com votos
legalmente válidos e auditoria oficial. Lê mapas completos
`espacial/links-ANO-TURNO.rds`.
Seu padrão considera presidência nacional e por UF em 2010, 2014, 2018,
2022 e 2026, nos dois turnos, além dos cargos estaduais desses mesmos anos.
Somente o DF recebe deputado distrital e as demais UFs recebem deputado
estadual; segundo turno estadual é considerado somente para governador.
Somente disputas com votos e mapa efetivamente disponíveis são exportadas.
Um turno não encontrado recebe razão explícita, sem fabricar resultado.

O destino padrão é `posts/explorador-eleitoral/data/`. Contém `manifest.json`
com `schema_version: 1`, `series`, `availability` e `metadata`, uma tabela
`availability.csv` e os JSONs em `series/ANO-TURNO-CARGO-UF.json`.
Os argumentos opcionais são pasta de saída, pasta dos caches históricos e
pasta dos mapas. A função `eleicao_build_explorer()` permite fornecer um
subconjunto de combinações para execução incremental ou testes.

Para acrescentar estados sem recalcular as séries já publicadas, use
`eleicao_build_explorer(targets = alvos, incremental = TRUE,
flush_progress = TRUE)`. O manifesto preserva as outras combinações e é
atualizado após cada grupo de insumos concluído. Uma impressão digital dos
arquivos de renda, vínculos espaciais, votos certificados e parâmetros
permite reutilizar resultados idênticos. Mudanças nesses insumos obrigam
nova agregação; uma disputa que falha na conferência sai do manifesto.

O manifesto é refeito apenas com o universo fixo; não incorpora curvas
anteriores que usavam ranking após exclusões. Arquivos antigos que não
constam do manifesto ficam sem referência. Mapas parciais de uma UF não
são promovidos silenciosamente a mapa nacional.

Antes de exportar, prepare os caches de votos estaduais sequencialmente:

```sh
Rscript shared-pipeline/eleicoes/agregacao/prepare-2026-all-ufs.R --dry-run
Rscript shared-pipeline/eleicoes/agregacao/prepare-2026-all-ufs.R
Rscript shared-pipeline/eleicoes/agregacao/prepare-2026-all-ufs.R SP,DF
```

Esse CLI invoca `historico/legal-validity.R` uma UF por vez, para cargos 3,
5, 6, 7 e 8, incluindo reconciliação oficial. O helper reaproveita agregados
brutos por local com hash e versão compatíveis para reduzir a memória.
Cada processo filho termina antes do seguinte, liberando sua memória. Falhas
são registradas e a lista continua; um relatório final permite retomar as
UFs necessárias. `--dry-run` não baixa nem normaliza dados.

Verifique o orquestrador sem usar dados nacionais:

```sh
Rscript shared-pipeline/eleicoes/agregacao/tests-build.R
```
