# Diagnóstico das legendas PB e SE — 2026

`diagnosticar-legenda-PB-SE.R` usa somente os caches já preparados, uma UF por
vez. Não baixa arquivos, não altera caches legais e não modifica o helper de
reconciliação em execução.

```sh
Rscript shared-pipeline/eleicoes/agregacao/validacao/diagnosticar-legenda-PB-SE.R
```

A divergência observada no helper `legal-validity.R` ocorria porque os
boletins por seção contêm números de legenda que não têm registro na tabela
oficial de partidos daquela disputa. Os números abaixo não podem entrar no
denominador de votos válidos.

| UF | Cargo | Número | Votos | Chaves município/zona/partido |
|---|---|---:|---:|---:|
| PB | Deputado federal | 70 | 304 | 128 |
| SE | Deputado federal | 25 | 309 | 68 |
| SE | Deputado federal | 77 | 126 | 48 |

A verificação independente contra `QT_VOTOS_NULOS_TECNICOS` passou em **cada
município/zona**, sem distribuir nenhum voto entre seções ou locais:

| UF/cargo | Técnicos nominais | Técnicos de legenda | Técnicos oficiais | Chaves verificadas | Divergências |
|---|---:|---:|---:|---:|---:|
| PB / deputado federal | 569 | 304 | 873 | 230 | 0 |
| PB / deputado estadual | 0 | 0 | 0 | 230 | 0 |
| SE / deputado federal | 917 | 435 | 1.352 | 77 | 0 |
| SE / deputado estadual | 273 | 0 | 273 | 77 | 0 |

Todas as legendas com registro oficial apresentaram correspondência exata.
Não foram encontrados casos de validade parcial ou conversão nominal que
explicassem essas diferenças.

A correção proposta é separar, logo após a junção com a tabela de partidos,
as linhas sem correspondência oficial como técnicas; somar seus votos aos
técnicos nominais nas chaves completas de eleição/turno/UF/município/zona/cargo;
excluí-las dos votos válidos; e manter todas as verificações oficiais por
município/zona. A ausência de um partido não autoriza aceitação silenciosa:
a igualdade com os nulos técnicos oficiais e a reconciliação integral do
cargo continuam obrigatórias. Qualquer diferença entre legendas reconhecidas
ou residual nos técnicos deve continuar bloqueando a disputa.

Os produtos completos são gravados fora do Git em
`data-raw/eleicoes-renda/historico/diagnostico-legenda/`:

- `UF-party-mismatches.csv`: chaves e contagens que causam a divergência.
- `UF-technical-null-by-zone.csv`: comprovação independente em cada zona.
- `UF-summary.csv`: classificação agregada da comparação.
- `UF-party-columns.txt`: campos da tabela oficial efetivamente utilizada.

## Correção isolada enquanto o batch histórico está ativo

`legal-party-technical-wrapper.R` carrega o helper original em um ambiente
privado e insere a separação das legendas técnicas imediatamente após a
junção de partidos. O arquivo original permanece intacto. A transformação
recusa executar se não encontrar exatamente uma junção esperada. Todas as
verificações originais de partidos conhecidos e totais por município/zona
continuam obrigatórias.

```sh
Rscript shared-pipeline/eleicoes/agregacao/validacao/test-legal-party-wrapper.R
Rscript shared-pipeline/eleicoes/agregacao/validacao/legal-party-technical-wrapper.R PB,SE
```

O teste usa arquivos sintéticos pequenos e exercita a função de reconciliação
completa: aceita a legenda ausente quando os nulos técnicos conferem,
recusa diferença em partido reconhecido e também recusa diferença nos
técnicos. A execução real produz os caches legais completos PB/SE e os CSVs
de auditoria usuais, com os mesmos nomes esperados pelo exportador.

A execução real concluiu com sucesso os quatro cargos aplicáveis de cada
UF (governador, senador e deputados federal/estadual): 230 chaves de
município/zona por cargo na PB e 77 na SE. Todos os totais nominais,
de legenda, válidos e técnicos conferiram exatamente. Não há cargo de
deputado distrital nesses estados; a ausência não foi preenchida com zero.

## Agregação de candidatos com chaves já únicas

`candidate-group-fastpath.R` fornece `legal_group_candidates_fast(cand, keys)`
como substituição exata da agregação de candidatos do helper legal. Chaves
únicas passam por operações vetoriais; somente chaves repetidas executam
os callbacks originais de soma, partido e destino. A ordem da primeira
aparição dos grupos é preservada. O arquivo não altera o helper ativo.

O adaptador opcional `legal_candidate_fast_environment()` carrega uma cópia
privada do helper e substitui somente a expressão de agrupamento esperada,
recusando executar se a estrutura do código tiver mudado.

```sh
Rscript shared-pipeline/eleicoes/agregacao/validacao/test-candidate-fastpath.R
```

O teste compara valores, tipos e ordem com a expressão original, incluindo
chaves duplicadas ou ausentes, contagens ausentes, destinos múltiplos,
partidos ambíguos e casos aleatórios pequenos. Um destino único ausente
vira string vazia, reproduzindo `paste(sort(unique(NA_character_)), collapse
= "|")`; não permanece `NA` por conveniência do caminho rápido.

No benchmark local pequeno de 20 mil chaves únicas, a referência levou
14,07 s e o caminho rápido 0,05 s, com resultados iguais. Esses tempos
servem para identificar o custo dos callbacks; não são previsão do tempo
de SP ou garantia de aceleração do pipeline completo, que inclui leitura,
junções e outros controles. Nenhum benchmark nacional foi iniciado.

## Retomada sequencial com as duas correções

O CLI `../resume-2026-optimized.R` combina os dois adaptadores no mesmo
ambiente privado: separação exata das legendas técnicas e fastpath de chaves
de candidatos. O helper histórico original permanece intacto. O padrão
retoma MG, ES, RJ, SP, PR, SC, RS, MS, MT, GO e DF, uma UF por vez. Um argumento
com lista de UFs restringe a retomada. Cada estado concluído atualiza o
relatório `historico/resume-2026-optimized-status.csv`.

```sh
Rscript shared-pipeline/eleicoes/agregacao/validacao/test-optimized-resume.R
Rscript shared-pipeline/eleicoes/agregacao/resume-2026-optimized.R --dry-run
Rscript shared-pipeline/eleicoes/agregacao/resume-2026-optimized.R ES,RJ,SP
```

O teste composto exercita a reconciliação legal completa com as duas
substituições ativas, incluindo rejeição das divergências reconhecidas e
dos técnicos, além de verificar a lista e ordem do plano de retomada.
O modo `--dry-run` não processa dados nacionais.
