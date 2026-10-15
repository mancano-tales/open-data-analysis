# Shared, compressed publication of the electoral explorer

The raw aggregate JSON under `posts/explorador-eleitoral/data/` remains the
reproducible source used by the R exporter and the source-data tests. It is
not the deployment format. Publishing uses one canonical data directory,
`docs/posts/explorador-eleitoral/data/`, shared by both languages. The PT
page resolves `../../../posts/explorador-eleitoral/data/` relative to its
published URL, preserving a GitHub Pages project prefix.

The PT QMD does not declare the source data directory as a resource. Its
small JS/CSS/thumbnail and documentation copies remain valid; removing them
would save little and unnecessarily break existing asset URLs.

## Build order

1. Finish electoral exports and stop concurrent writers.
2. Render EN and PT with Quarto. Quarto may copy the raw EN JSON again.
3. Commit an exact Git backup of the original aggregate JSON and any
   previously published files that will be removed. The publisher checks
   content against `HEAD`; a staged-only backup is insufficient.
4. Inspect the default dry run:

   ```sh
   node posts/explorador-eleitoral/publish-static.cjs
   ```

5. Publish compressed files and remove verified redundant copies:

   ```sh
   node posts/explorador-eleitoral/publish-static.cjs --apply --prune-json --prune-pt
   ```

6. Validate the output and commit the resulting static publication. Do not
   render again after this step without rerunning the publisher.

Without pruning flags, `--apply` generates gzip files and switches the
published manifest, but leaves the original published JSON in place. This
is useful to inspect the result, but does not achieve the intended size
reduction. Only the explicitly named, individually validated files are
removed; there is no recursive deletion. PT pruning covers tracked JSON/CSV
copies only, so a new untracked redundant copy must be backed up/committed
or inspected separately before it can be removed.

The publisher streams one source file at a time through gzip level 9. Gzip
headers contain no source filename and have zero timestamp. It writes each
file atomically, changes only published manifest URLs to `.json.gz`, and
switches the manifest after every compressed series is ready. Source JSON,
source manifest, estimates and coverage remain unchanged. The published
manifest is compact JSON with the same schema and metadata. Availability
CSV remains available in the shared directory.

## Browser and URL behavior

`app.js` fetches compressed series and uses the browser's
`DecompressionStream('gzip')` before parsing JSON. It also accepts a host
that already decoded HTTP transport gzip. Download labels explicitly say
JSON.gz. A browser without the decompression API receives an explanation;
the compact manifest and downloadable gzip files remain public.

Dataset URLs are obtained from the canonical manifest rather than assembled
from candidate/year values. After pruning, old direct `.json` dataset URLs
and old PT data URLs no longer exist. GitHub Pages cannot provide HTTP
redirects for arbitrary static JSON paths; an HTML redirect is not usable by
`fetch().json()`. Keeping those raw copies would defeat the size reduction.
The post URLs, language switch and current download links remain valid.
Consumers of older direct dataset links should migrate to the canonical
manifest. Historical originals remain recoverable from Git history.

Revalidate the manifest on each page load (`cache: 'no-cache'`), and bump
the JS query version when deploying this loader alongside gzip manifests.
Both QMD pages use `shared-gzip-v1` for this transition.

## Checks

```sh
node posts/explorador-eleitoral/test-app.cjs --ui-only
node posts/explorador-eleitoral/test-publish-static.cjs
node posts/explorador-eleitoral/test-publication.cjs
```

The publisher test uses isolated, explicitly labelled fixtures; it verifies
lossless deterministic compression and refuses pruning without a Git backup.
The publication check verifies real shared URLs, gzip integrity and manifest
targets after publishing. Run the publisher after exports stop; do not
read or compress real source files concurrently with the R exporter.
