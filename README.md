# ET - ECAD Electronic Tool
## An approach to model complex schematics and layouts

### The Idea behind
- Most ECAD tools do not allow to opening, checking and editing of multiple designs simultaneously.
- We need real hierarchic and modular designs.
- We need a text based, machine and human readable format for design files.
- Design checks provided by common ECAD tools are way too superficial and trivial.
- Style guides must be checked against.
- The tool must be highly scripting capable.
- We want to do agile hardware develpment which requires the features mentioned above.
- The tool must be open sourced.
- In the long run, a nice GUI will come with ET.
- Your feedback and collaboration is highly welcome !

### Supported CAE formats
- KiCad V4, V5
- Planned is to support also EAGLE.
- An ET native format.

### Usage
- First change into the root directory of your KiCad projects and generate a configuration file where prefixes, units of measurement and other things are defined with this command:

```sh
$ et --make_configuration my_configuration.txt
```

- This file is now placed in the root directory of your KiCad projects. Edit it according to your customs.

- To import a single KiCad V5 design into a native project 'my_et_project' run this command: 

```sh
$ et --configuration_file my_configuration.txt --import_format kicad_v5 --import_module my_kicad_project/ --project my_et_project
```
 
optionally provide a log level for debugging:

```sh 
$ et --configuration_file my_configuration.txt --import_format kicad_v5 --import_module my_kicad_project/ --project my_et_project --log_level 2
```

- ET creates in the projects root directory a folder named "ET" where you find logfiles, netlists, statistics, BOMs, ...

- If there are lots of projects to be imported, write their names in the configuration file in section [IMPORT_MODULES]. Then run this command:

```sh
$ et --import_modules --configuration_file my_configuration.txt --project my_et_project --log_level 1
```

### Installation
- Install the GNAT Ada compiler (version 7 or later). It should come along with major linux distros.
- Run the install script install.sh as non-root user.

```sh
$ sh install.sh
```

- The script installs the executable binary et in $HOME/bin and further-on creates a hidden directory .ET in $HOME where other configuration files live.
- Currently there is nothing to do in the configuration directory -> leave it as it is.
- For help contact info@blunk-electronic.de . You are highly welcome :-)

#### Why Ada ??
- The only programming language that provides a robust and strong typing system is Ada.
- Objects and structures within a schematic, library and board layout are very very complex things and require sound modelling.
- If saftey critical and military applications use Ada, then is must be good for an advanced ECAD system as well.
- Ada is beautiful :-)
