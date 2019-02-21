# ET - ECAD Electronic Modeling Tool
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

#### Creating a conventions file
The conventions file is the place where file where prefixes, units of measurement and other things are defined. It is
not mandatory. This step can be omitted. If so, lots of design checks wil not be performed.
Change into the root directory of your projects and generate a conventions with this command:

```sh
$ et --make_conventions conventions.txt
```

This file can now be found in the root directory of your projects. Edit it according to your customs.

#### Importing a KiCad project
To import a single KiCad V5 design into a native project 'my_et_project' run this command: 

```sh
$ et --import_format kicad_v5 --import_project my_kicad_project/
```

If a conventions file exits then it can be invoked for more detailled design checks:

```sh
$ et --conventions conventions.txt --import_format kicad_v5 --import_project my_kicad_project/
```

Optionally provide a log level for debugging:

```sh 
$ et --conventions conventions.txt --import_format kicad_v5 --import_project my_kicad_project/ --log_level 2
```

ET creates in the current working directory a folder named "ET/et_import" where you find the now native project.
There is also an import report where log messages can be found. See "ET/reports". Depending on the log level this report
contains more or less debug information.

#### Opening an ET native project
To open a newly imported KiCad project or a native project like 'my_et_project' run this command: 

```sh
$ et --open my_et_project/
```

Or if the project lives somewhere else:

```sh
$ et --open /home/user/ecad/my_et_project/
```

A log level can also be passed:

```sh
$ et --open /home/user/ecad/my_et_project/ --log_level 2
```

If a conventions file is available run:

```sh
$ et --conventions conventions.txt --open /home/user/ecad/my_et_project/ --log_level 2
```

The project can also be saved under a different name at a different place:

```sh
$ et --conventions conventions.txt --open /home/user/ecad/my_et_project/ --save_as /home/user/tmp/eval --log_level 2
```

ET creates in the current working directory a folder named "ET/reports" for log messages.


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
