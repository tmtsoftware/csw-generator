# CSW Generator

This project provides a command line app that uses the TMT ICD database to generate and publish CSW events for testing.

There are plans to also generate Scala and/or Java code with CSW parameter keys. 

Work in progress...

## Requirements

Requires a local icd database.
To get this, install the [icd](https://github.com/tmtsoftware/icd) applications and run
```
icd-git --ingest
```
once to create the database from the GitHub ICD repos.

## Generating events 

To generate events, use the `csw-event-generator` command line app:

```
Usage: csw-event-generator [options]

-d, --db <name>          The name of the database to use (default: icds4)
-h, --host <hostname>    The host name where the database is running (default: localhost)
-p, --port <number>      The port number to use for the database (default: 27017)
-s, --subsystem <subsystem>[:version]
Specifies the subsystem (and optional version) to be used by any following options
-c, --component <name>   Specifies the component to be used by any following options (subsystem must also be specified)
-e, --event <name>       Specifies the event to be used by any following options (subsystem and component must also be specified)
--publish                Start publishing the selected events at the defined rate (Needs subsystem, component and optionally event options)
--help
--version

```


