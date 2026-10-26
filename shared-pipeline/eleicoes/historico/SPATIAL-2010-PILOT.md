# Piloto espacial Acre: pleito e Censo 2010

Este piloto tem status `prepared_not_integrated`. Não agrega candidatos nem
altera as séries públicas, o manifesto, o aplicativo ou a referência atual de
renda do Censo 2022.

Usa `V007` (renda nominal média dos responsáveis com rendimento) e `V001`
do arquivo Basico preparado por `prepare-income-2010.R`. Os centis nacionais
e estaduais são calculados antes das exclusões espaciais, com desempate pelo
código setorial. A unidade permanece BRL nominal de 2010: não há conversão
para salários mínimos. Ver a conferência de variáveis em `CENSO-2010.md`.

## Reprodução

São necessários os pacotes já usados pelo projeto: data.table, sf, s2,
jsonlite, arrow, duckdb, DBI, geobr e digest. Não há instalação automática.
Na raiz do repositório:

```powershell
New-Item -ItemType Directory -Force data-raw/eleicoes-renda/censo2010/spatial-pilot
curl.exe --fail --silent --show-error --max-time 60 --max-filesize 1048576 --output data-raw/eleicoes-renda/censo2010/spatial-pilot/geobr-release-v2.0.0.json https://api.github.com/repos/ipea/geobr_prep_data/releases/tags/v2.0.0
Rscript --vanilla shared-pipeline/eleicoes/historico/test-spatial-2010-pilot.R
Rscript --vanilla shared-pipeline/eleicoes/historico/prepare-spatial-2010-pilot.R --prepare-ac
```

O catálogo oficial da [release geobr v2.0.0](https://github.com/ipea/geobr_prep_data/releases/tag/v2.0.0)
identifica `censustracts_2010.parquet` (416.981.938 bytes) e
`municipalities_2010.parquet` (115.861.487 bytes), com SHA256. O produtor
confere URL, tamanho e SHA256; usa TLS verificado; limita cada download a
2GiB e exige reserva de 20GiB livres. Arquivos existentes são reutilizados
apenas se tamanho e SHA256 coincidirem. DuckDB filtra `code_state=12` antes
de coletar as geometrias, com limite de memória de 1GB e duas threads. A
materialização é recusada acima de 128MiB de WKB selecionado. Somente o
cache nacional de arquivos existe em disco; nenhum sf nacional é carregado.

## Ligação e auditoria

O piloto preserva as chaves TSE originais e a primeira data do pleito 2010,
03/10/2010. Exige correspondência TSE–IBGE2010 bijetiva, código IBGE exato
e ponto estritamente dentro de um único município de 2010, igual ao declarado.
Não aplica aliases nem corrige silenciosamente município por coordenadas.
Duplicatas idênticas de local são colapsadas; chaves com coordenadas
conflitantes são excluídas.

Os pontos interiores dos setores são obtidos por `point_on_surface` em
EPSG5880 e conferidos dentro do próprio setor e município de 2010. A
distância usa S2 esférico, em metros, após transformação para EPSG4326.
O vizinho mais próximo é procurado somente entre locais validados no mesmo
município; empates até 1e-6 metro ficam sem atribuição. A flag de cobertura
exige distância até 5.000 metros. Setores excluídos continuam no arquivo
com seu centil original e motivo de exclusão.

Probe real em 17/10/2026: 308.993 setores elegíveis no universo nacional;
871 no Acre. A malha possui 900 geometrias setoriais AC, enquanto o Basico
possui 874 registros AC; apenas os setores elegíveis de renda participam
do piloto. Dos 1.505 registros de locais, 805 duplicatas idênticas foram
colapsadas, 141 pontos ficaram fora da malha municipal e 10 em município
diferente do declarado. Restaram 549 locais validados nos 22 municípios.
Dos 871 setores, 586 têm ligação única até 5km, 279 ficam além de 5km e
6 têm empate sem atribuição. Esses números são cobertura espacial, não
cobertura de votos nem certificação da localização original dos locais.

As saídas ficam apenas em `data-raw/eleicoes-renda/censo2010/spatial-pilot`:
RDS dos vínculos, locais e pontos; CSV de correspondência municipal,
duplicidades e auditoria linha a linha; `metadata.json` com fontes, versões,
CRS, hashes, universos e exclusões. O cache legal AC2010 aparece somente
como referência de proveniência por MD5; nenhum voto é lido ou agregado.
