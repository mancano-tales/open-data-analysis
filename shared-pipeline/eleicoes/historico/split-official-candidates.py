"""Stream the official national candidate ZIP into bounded per-state catalogs.

Preserves source counts and Latin-1 encoding; does not infer legal validity.
Usage: python split-official-candidates.py YEAR CACHE_DIR
"""
import csv
import hashlib
import io
import json
from collections import Counter
from pathlib import Path
import sys
import zipfile


def split_candidates(year: int, cache: Path) -> None:
    archive = cache / f"votacao_candidato_munzona_{year}.zip"
    destination = cache / f"candidate-catalog-{year}"
    destination.mkdir(parents=True, exist_ok=True)
    fields = ["ANO_ELEICAO", "CD_TIPO_ELEICAO", "NR_TURNO", "CD_ELEICAO", "SG_UF",
              "CD_MUNICIPIO", "NR_ZONA", "CD_CARGO", "SQ_CANDIDATO", "NR_CANDIDATO",
              "NM_URNA_CANDIDATO", "NR_PARTIDO", "SG_PARTIDO", "NM_TIPO_DESTINACAO_VOTOS",
              "QT_VOTOS_NOMINAIS", "QT_VOTOS_NOMINAIS_VALIDOS"]
    counts = Counter()
    handles, writers = {}, {}
    try:
        with zipfile.ZipFile(archive) as zipped:
            members = [n for n in zipped.namelist()
                       if n.lower().endswith(f"votacao_candidato_munzona_{year}_brasil.csv")]
            if len(members) != 1:
                raise ValueError("No unique official BRASIL candidate table")
            with io.TextIOWrapper(zipped.open(members[0]), encoding="latin-1", newline="") as stream:
                rows = csv.DictReader(stream, delimiter=";")
                selected = [f for f in fields if f in rows.fieldnames]
                for row in rows:
                    state = row["SG_UF"]
                    if state == "ZZ" or row.get("CD_TIPO_ELEICAO", "2") != "2":
                        continue
                    if len(state) != 2 or not state.isalpha():
                        raise ValueError(f"Invalid state in official source: {state!r}")
                    if state not in writers:
                        handles[state] = (destination / f"{state}.csv.part").open("w", encoding="latin-1", newline="")
                        writers[state] = csv.DictWriter(handles[state], selected, delimiter=";", quoting=csv.QUOTE_ALL)
                        writers[state].writeheader()
                    writers[state].writerow({f: row[f] for f in selected})
                    counts[(state, row["CD_CARGO"])] += 1
    finally:
        for handle in handles.values():
            handle.close()
    for state in writers:
        (destination / f"{state}.csv.part").replace(destination / f"{state}.csv")
    with archive.open("rb") as source:
        digest = hashlib.file_digest(source, "md5").hexdigest()
    report = {"source": archive.name, "source_md5": digest,
              "rows_by_state_office": [{"uf": k[0], "office": k[1], "rows": n}
                                       for k, n in sorted(counts.items())]}
    (destination / "provenance.json").write_text(json.dumps(report, indent=2), encoding="utf-8")
    print(json.dumps(report, ensure_ascii=True))


if __name__ == "__main__":
    split_candidates(int(sys.argv[1]), Path(sys.argv[2]))
