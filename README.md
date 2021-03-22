# cardano-entropy

## Pre-requisites:

```bash
$ brew install chromedriver
```

## Run to download NYSE data and take its hash

```bash
$ cabal -v0 run cardano-entropy nyse --workspace="$WORKSPACE" --username="$USERNAME" --password="$PASSWORD"
Downloaded: /Users/jky/tmp/download-0ac80eea1ebf36da/NYSE_20210319.csv
Hash: 42e1611e701d4b8885da5ef5cf54f2e4a56f77b675835fcae6c132aff09a0f46
```

Options:

* `--worksapce`: Where temporary files will go.  This can be set to your temporary directory.
* `--username`: Username obtained by registering on http://www.eoddata.com/.
* `--password`: Password obtained by registering on http://www.eoddata.com/.

## Run to download GIS data and take its hash

```bash
$ cabal -v0 run cardano-entropy nyse --workspace="$WORKSPACE" --end-date="$DATE"
Downloaded: /Users/jky/tmp/download-0ac80eea1ebf36da/NYSE_20210319.csv
Hash: 42e1611e701d4b8885da5ef5cf54f2e4a56f77b675835fcae6c132aff09a0f46
```

Options:

* `--workspace`: Where temporary files will go.  This can be set to your temporary directory.
* `--date`: The date that marks the end of the 24 hour window for which we want to filter the data.
