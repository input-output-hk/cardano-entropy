# cardano-entropy

[![workflow](https://github.com/input-output-hk/cardano-entropy/actions/workflows/haskell.yml/badge.svg?branch=main)](https://github.com/input-output-hk/cardano-entropy/actions/workflows/haskell.yml?branch=main)

## Background

The Cardano entropy parameter will be updated during the epoch starting on `April 5, 2021`;
the entropy parameter will be determined by the hash of the last block prior to
`Wed Apr 7`, `15:44:51 UTC = slot 151200` of `epoch 258`; this hash value appears in the first
block appearing in slot `151200` or later.

Considering the hash chain structure of the blockchain, this block hash is determined by the
entire chain up to this point. Note that the parameter update mechanism requires delegates'
votes to appear on-chain prior to `Wed Apr 7`, `15:44:51 UTC = slot 151200` of `epoch 25`.

IO Global scientists and engineers will inject transactions with metadata determined by several
public sources of entropy: hashes of the closing prices of the New York Stock Exchange on
`April 6 2021`, and real-time seismic data from the US Geological Survey, the University of
Athens, and the Japan Meteorological Society.

A more detailed timeline of the process is presented below:

```
Epoch starts:                                           Mon Apr 5, 21:44:51 UTC = slot 0 of epoch 258
Insert randomness generated on or after:                Tue Apr 6, 9:44:51 UTC = slot 43200 of epoch 258
(NYSE opens)                                            Tue Apr 6, 13:30 UTC
(NYSE closes)                                           Tue Apr 6, 20:00 UTC
(NYSE data available on eoddata.com)                    Wed Apr 7, 1:00 UTC
Seismic data window                                     Epoch start <= WINDOW < Wed Apr 7, 9:44:51 UTC
Insert randomness before:                               Wed Apr 7, 15:44:51 UTC = slot 151200 of epoch 258
Nonce = prev-block hash from first block on or after:   Wed Apr 7, 15:44:51 UTC = slot 151200 of epoch 258
Parameter-changing Tx must be included before:          Wed Apr 7, 21:44:51 UTC = slot 172800 of epoch 258
```

## Pre-requisites:

### Install Selenium Server and Chrome Driver

You will need both the Selenium Server and Chrome Driver installed.

Some of the commands require that you have the selenium server running in the background:

#### MacOS

To install:

```bash
$ brew install selenium-server-standalone
$ brew install chromedriver
```

Then to run:

```bash
$ selenium-server -port 4444
```

#### MacOS or Linux via Docker

To run:

```bash
$ docker run -d -p 4444:4444 -v /dev/shm:/dev/shm -v "$WORKSPACE:$WORKSPACE" selenium/standalone-chrome:4.0.0-beta-3-prerelease-20210321
```

Note, the directory pointed to by `$WORKSPACE` must exist and must remain the same for running the commands below.

## Run to download NYSE data and take its hash

```bash
$ cardano-entropy nyse --workspace="$WORKSPACE" --username="$USERNAME" --password="$PASSWORD" --date="$DATE"
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

