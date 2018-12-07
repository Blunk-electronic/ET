# ET - ECAD Electronic Tool
## An approach to model and develop complex schematics and layouts

### The Idea behind
- Most ECAD tools do not allow opening, checking and editing of multiple designs simultaneously.
- We need real hierarchic and modular designs.
- We need a text based, machine and human readable format for design files.
- Design checks provided by common ECAD tools are way too superficial and trivial.
- Style guides must be checked against.
- The tool must be highly scripting capable.
- We want to do agile hardware develpment which requires the features mentioned above.
- The tool must be open sourced.
- In the long run, a nice GUI will come with ET.
- Your feedback and collaboration is highly welcome !

## Outstanding Features
- native Linux support
- ASCII / text based design and device model files - optimized for version control with GIT
- human readable and editable design and model files
- multi-schematic/board/layout support
- true hierarchic and modular design
- submodules instantiated in parent module by reference
- extensive design rule checking (device prefixes, purpose of user-interactive devices, partcodes, pinout of board-to-board connections ...)
- interfacing with system modelling tools

### Examples of design and component models
- A module file (containing schematic and layout stuff) looks like this <https://github.com/Blunk-electronic/ET/blob/master/examples/dummy/dummy.mod>
- Device model <https://github.com/Blunk-electronic/ET/blob/master/examples/device.dev>
- Symbol model <https://github.com/Blunk-electronic/ET/blob/master/examples/symbol.sym>
- Package model <https://github.com/Blunk-electronic/ET/blob/master/examples/package.pac>
- A so called rig-configuration that describes module instances and board-to-board connections <https://github.com/Blunk-electronic/ET/blob/master/examples/dummy/dummy.conf>

### Example of an ERC configuration file
- See this example <https://github.com/Blunk-electronic/ET/blob/master/examples/conf.txt>

### Supported CAE formats
- KiCad V4, V5 (import only)
- Planned is to support also EAGLE (import only)
- An ET native format as shown above.

### Usage

#### Importing a KiCad project

- First change into the root directory of your KiCad projects and generate a configuration file where prefixes, units of measurement and other things are defined with this command:

```sh
$ et --make_configuration my_configuration.txt
```

- This file is now placed in the root directory of your KiCad projects. Edit it according to your customs.

- To import a single KiCad V5 design into a native project 'my_et_project' run this command: 

```sh
$ et --configuration_file my_configuration.txt --import_format kicad_v5 --import_module my_kicad_project/
```
 
optionally provide a log level for debugging:

```sh 
$ et --configuration_file my_configuration.txt --import_format kicad_v5 --import_module my_kicad_project/ --log_level 2
```

- ET creates in the projects root directory a folder named "ET" where you find the imported projects, logfiles, netlists, statistics, BOMs, ...


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
- If saftey/mission critical and military applications use Ada, then is must be good for an advanced ECAD system as well.
- Ada is defined by ISO/IEC 8652:2012 and MIL-STD-1815
- Ada is beautiful :-)

### Collaboration
- We need a nice web site for the project.
- You are highly welcome !
