# Vínculos geográficos por eleição

`050_Mapear_Locais_Historicos.R` liga os pontos internos dos setores do Censo
2022 aos locais de votação georreferenciados do **próprio ano**, por
meio de `geobr::read_polling_places()`. O vínculo é independente dos candidatos
e da existência de votos no local. A disponibilidade de coordenadas consultada
começa em 2010; não se extrapolam coordenadas recentes para anos anteriores.

Quando a fonte oferece somente o cadastro do primeiro turno, o segundo turno
usa as coordenadas desse cadastro do mesmo ano e cruza as chaves dos locais
com os votos efetivos do segundo turno. Isso é uma aproximação documentada:
locais novos ou sem chave correspondente não recebem votos estimados, e uma
mudança de endereço com a mesma chave não pode ser detectada. Os metadados
registram `polling_reference_date`, `polling_reference_round` e
`within_year_register_reused`; a interface deve mostrar essa limitação.

Execute a partir da raiz, depois de preparar renda e pontos com os scripts
`010_Download_Censo_TSE.R` e `020_Cruzar_Setores_Votos.R`:

```sh
Rscript shared-pipeline/eleicoes/espacial/050_Mapear_Locais_Historicos.R 2026 1
```

Os arquivos ficam em `data-raw/eleicoes-renda/espacial/`, fora do Git. O processo
salva cada UF para permitir retomada. Mudanças nos arquivos de renda ou de
locais invalidam os vínculos em cache. Para reconstruir explicitamente, use
`map_election_places(2026, 1, rebuild = TRUE)` após carregar o script.

O universo de renda é nacional e fixo: responsáveis com renda média positiva
e número de responsáveis positivo. Ordena-se por V06004 e, em empates, código
do setor. Os 100 grupos têm aproximadamente o mesmo número de setores; não
são centis de pessoas ou eleitores. A renda em salários mínimos usa R$ 1.212,
o salário mínimo de 2022. O ranking é calculado antes de qualquer exclusão
eleitoral ou geográfica.

Chaves de local com coordenadas conflitantes são excluídas. Também são
excluídos pontos fora dos limites do Brasil ou incompatíveis com o município
declarado, usando a malha municipal de 2022. Cada setor recebe o local mais
próximo dentro do mesmo município de 2022, com distância métrica em EPSG:5880.
Setores sem local permanecem no universo com vínculo ausente. O limite de
5 km é aplicado posteriormente na agregação, permitindo documentar a cobertura
sem alterar os centis. Mudanças municipais entre eleições podem reduzir essa
cobertura e devem ser consideradas ao comparar anos.

Os CSVs `coverage-places-ANO-TURNO.csv` registram conflitos e incompatibilidades
dos locais. Os RDS finais incluem os 449.531 setores elegíveis e metadados de
ano, turno, data e identidade dos insumos; a quantidade depende da versão do
arquivo do Censo. A votação de cada setor continua sendo uma aproximação
territorial, sem identificação da renda individual ou do local onde cada
morador votou. O método não certifica reprodução exata da análise da Folha.
