# ET
An Electronic Tool for checking style and desgin conventions
- Supposed to import various CAE formats like KiCad, EAGLE, ...
- Currently only KiCad V4 is supported.
- To import a KiCad V4 design change into the root directory of your KiCad projects and run this command:

 et --import_format kicad_v4 --import_project my_kicad_project/

 optionally provide a log level for debugging:

 et --import_format kicad_v4 --import_project my_kicad_project/ --log_level 2

- ET creates in the projects root directory a folder named "ET" where you find reports, netlists, statistics, BOMs, ...

Installation:
- install the GNAT Ada compiler (version 4.5 or later)
- run the install script install.sh as non-root user
- The script installs the executable binary in $HOME/bin and further-on creates a hidden directory .ET in $HOME where configuration files live
