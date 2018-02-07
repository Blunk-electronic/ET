# ET
## An Electronic Tool for checking style and desgin conventions
- Supposed to import various CAE formats like KiCad, EAGLE, ...
- Currently only KiCad V4 is supported.

- First change into the root directory of your KiCad projects and generate a configuration file where prefixes, units of measurement and other things are defined with this command:

```sh
$ et --make_configuration my_configuration.txt
```

- This file is now placed in the root directory of your KiCad projects. Edit this file according to your own customs.

- To import a KiCad V4 design run this command: 

```sh
 et --configuration_file my_configuration.txt --import_format kicad_v4 --import_project my_kicad_project/
```
 
optionally provide a log level for debugging:

```sh 
 et --configuration_file my_configuration.txt --import_format kicad_v4 --import_project my_kicad_project/ --log_level 2
```

- ET creates in the projects root directory a folder named "ET" where you find logfiles, reports, netlists, statistics, BOMs, ...

- If there are lots of projects to be imported, write their names in the configuration file in section [IMPORT_MODULES]. Then run this command:

```sh
 et --import_modules --configuration_file my_configuration.txt --log_level 1
```

Installation:
- install the GNAT Ada compiler (version 7 or later)
- run the install script install.sh as non-root user
- The script installs the executable binary in $HOME/bin and further-on creates a hidden directory .ET in $HOME where configuration files live
- currently there is nothing to do in the configuration directory -> leave it as it is
- for help contact info@blunk-electronic.de . You are highly welcome :-)
