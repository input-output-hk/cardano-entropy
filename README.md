# cardano-entropy

## Pre-requisites:

```bash
$ brew install selenium-server-standalone
$ brew install chromedriver
```

## Run to download NYSE data and take its hash

```bash
$ run cardano-entropy nyse --workspace="$WORKSPACE" --username="$USERNAME" --password="$PASSWORD" --date="$DATE"
Downloaded: /Users/jky/tmp/download-0ac80eea1ebf36da/NYSE_20210319.csv
Hash: 42e1611e701d4b8885da5ef5cf54f2e4a56f77b675835fcae6c132aff09a0f46
```

Options:

* `--worksapce`: Where temporary files will go.  This can be set to your temporary directory.
* `--username`: Username obtained by registering on http://www.eoddata.com/.
* `--password`: Password obtained by registering on http://www.eoddata.com/.
* `--end-date`: The last date for which end of day market data should be downloaded.  Format `YYYY-MM-DD`.
* `--days`: The number of days that should be downloaded.
* `--headless`: Whether or not to run Chrome headless.  `True` or `False`.  Default `True`.

## Run to download GIS data and take its hash

```bash
$ cardano-entropy nyse --workspace="$WORKSPACE" --end-date="$DATE"
Downloaded: /Users/jky/tmp/download-0ac80eea1ebf36da/NYSE_20210319.csv
Hash: 42e1611e701d4b8885da5ef5cf54f2e4a56f77b675835fcae6c132aff09a0f46
```

Options:

* `--workspace`: Where temporary files will go.  This can be set to your temporary directory.
* `--date`: The date that marks the end of the 24 hour window for which we want to filter the data.

## Run to download GIS data and take its hash

```bash
$ cardano-entropy jma-quake --workspace="$WORKSPACE" --end-date-time "$END_DATE_TIME' --hours "$HOURS"
Time window: 2020-03-22 06:00:00 UTC <= t < 2020-03-24 00:00:00 UTC
Downloading: "https://www.data.jma.go.jp/multi/data/VXSE53/en.json?_=1585008000000"
Downloaded to /Users/jky/tmp/download-jma-quake-74d6be37c450a863/latest.json
Filtered to /Users/jky/tmp/download-jma-quake-74d6be37c450a863/selected.json
Hash: 4f53cda18c2baa0c0354bb5f9a3ecbe5ed12ab4d8e11ba873c2f11161202b945
```

Options:

* `--workspace`: Where temporary files will go.  This can be set to your temporary directory.
* `--end-date-time`: The date that marks the end of the window for which we want to filter the data.
* `--hours`: The length window in hours for which we want to filter the data.

