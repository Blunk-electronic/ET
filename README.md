# SYSTEM ET
## An ECAD-Tool for complex schematics and layouts

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
- true hierarchic and modular design with interfaces at the module boundaries
- submodules instantiated in parent module by reference
- extensive design rule checking (device prefixes, purpose of user-interactive devices, partcodes, pinout of board-to-board connections ...)
- interfacing with system modelling tools

### Examples of design and component models
- A module file (containing schematic and layout stuff) looks like this <https://github.com/Blunk-electronic/ET/blob/master/examples/dummy/dummy.mod>
- There is a strict separation between symbol, package/footprint and device:
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
For everything ET does you will find in the current working directory a folder named "ET/reports" for log messages.

#### Importing a KiCad project
To import a single KiCad V5 design into a native project 'my_et_project' run this command: 

```sh
$ et --import-format kicad_v5 --import-project my_kicad_project/
```

Optionally provide a log level for debugging:

```sh 
$ et --import-format kicad_v5 --import-project my_kicad_project/ --log-level 2
```

ET creates in the current working directory a folder named "ET/et_import" where you find the now native project.
Inside the project you will find a directory named 'libraries' where the imported component libraries
associated with the project live. 
There is also an import report where log messages can be found. See "ET/reports". Depending on the log level this report
contains more or less debug information.

NOTE: Currently all kinds of text styles (normal, italic, bold) are ignored and replaced
by ET internal hard coded fonts.

#### Creating a conventions file
The conventions file is the place where file where prefixes, units of measurement and other things are defined. It is
not mandatory. This step can be omitted. If so, lots of design checks wil not be performed.
Change into the root directory of your projects and generate a conventions with this command:

```sh
$ et --make-conventions conventions.txt
```

This file can now be found in the root directory of your projects. Edit it according to your customs.


#### Creating an ET native project
To create a new project like 'my_new_et_project' run this command. CAUTION: An already existing project
of the same name will be deleted without warning !

```sh
$ et --create-project my_new_et_project/
```


#### Opening an ET native project
To open a native project like 'my_et_project' run this command: 

```sh
$ et --open-project my_et_project/
```

Optionally the file name of the generic module to be opened can be added:

```sh
$ et --open-project my_et_project/ --module my_et_project/power_supply.mod
```

or just

```sh
$ et --open-project my_et_project/ --module power_supply.mod
```

The module file must exist in the project directory.


If the project lives somewhere else:

```sh
$ et --open-project /home/user/ecad/my_et_project/
```

A log level can also be passed:

```sh
$ et --open-project /home/user/ecad/my_et_project/ --log-level 2
```

If a conventions file is available run:

```sh
$ et --conventions conventions.txt --open-project /home/user/ecad/my_et_project/ --log-level 2
```

The project can also be saved under a different name at a different place:

```sh
$ et --conventions conventions.txt --open-project /home/user/ecad/my_et_project/ --save-project-as /home/user/tmp/eval --log-level 2
```

Opening a project includes syntax checking. See the report for details.


#### Creating an ET native package (or footprint)
Packages can be real or virtual. Virtual components are things like testpoints or edge connectors.
By default a real package will be created.
The newly created package should be saved right away.
To create a native package drawing like 'S_0805.pac' run this command: 

```sh
$ et --create-package --package-appearance real --save-package-as tmp/dummy_S_0805.pac
```

```sh
$ et --create-package --package-appearance virtual --save-package-as tmp/dummy_connector.pac
```

Since the appearance has a default, it can be omitted:

```sh
$ et --create-package --save-package-as tmp/dummy_S_0805.pac
```


#### Opening an ET native package (or footprint)
To open a native package drawing like 'S_0805.pac' run this command: 

```sh
$ et --open-package libraries/packages/S_0805.pac
```

The package can also be saved under a different name at a different place:

```sh
$ et --open-package libraries/packages/S_0805.pac --save-package-as tmp/dummy_S_0805.pac
```
Opening a package includes syntax checking. See the report for details.




#### Creating an ET native symbol
A symbol is an abstraction of a component in the schematic. 
Symbols can represent a virtual component such as a GND-symbol
or something real like a resistor that is mounted on the PCB.
By default a pcb-type symbol will be created.
The newly created symbol should be saved right away.
To create a native symbol like 'opamp.sym' run this command: 

```sh
$ et --create-symbol --symbol-appearance pcb --save-symbol-as tmp/dummy_opamp.sym
```

```sh
$ et --create-symbol --symbol-appearance virtual --save-symbol-as tmp/gnd.sym
```

Since the appearance has a default, it can be omitted:

```sh
$ et --create-symbol --save-symbol-as tmp/dummy_opamp.sym
```



#### Opening an ET native symbol
To open a native symbol drawing like 'opamp.sym' run this command: 

```sh
$ et --open-symbol libraries/symbols/opamp.sym
```

The symbol can also be saved under a different name at a different place:

```sh
$ et --open-symbol libraries/symbols/opamp.sym --save-symbol-as tmp/dummy_opamp.sym
```
Opening a symbol includes syntax checking. See the report for details.



#### Creating an ET native device
A device is the compound of symbol(s) and package(s).

```sh
$ et --create-device --device-appearance pcb --save-device-as tmp/TL084D.dev
```

```sh
$ et --create-device --device-appearance virtual --save-device-as tmp/gnd.dev
```

Since the appearance has a default, it can be omitted:

```sh
$ et --create-device --device-appearance pcb --save-device-as tmp/TL084D.dev
```


#### Creating drawing frame templates
There is a distiction between drawing frames for schematic and PCB.

To create or open a schematic frame:
```sh
$ et --create-schematic-frame tmp/frames/A4_landscape.frs
```

```sh
$ et --open-schematic-frame lib/frames/schematic/A4_landscape.frs --save-schematic-frame-as tmp/frames/A4_landscape.frs
```

Similar a board frame can be created or opened:
```sh
$ et --create-pcb-frame tmp/frames/A4_landscape.frb
```

```sh
$ et --open-pcb-frame lib/frames/pcb/A4_landscape.frb --save-pcb-frame-as tmp/frames/A4_landscape.frb
```

#### Runmode
By default ET launches a GUI (which is currently under construction). For automated processing
the GUI is not required. A command line switch for the runmode can be used. This example launches 
ET in headless mode, which means without any graphical user interface:

```sh
$ et --open-project my_project --runmode headless
```

Depending on the specified runmode, ET launches a dedicated GUI. The next example starts the
module edtior, which is default. In this mode you can edit the schematic and layout of a module:

```sh
$ et --open-project my_project --runmode module
```

Other runmodes currently under construction are:
- rig
- symbol
- package
- device


#### Executing scripts
ET has an internal script processor that reads and executes a script file. The scripting feature allows manipulating designs without GUI:

```sh
$ et --open-project my_et_project/ --script my_et_project/my_script.scr --save-project-as modified_project --log-level 2
```

Find the script processor command set [here](script_command_set.md).
<!--An example of a script that modifies things in a dummy module (named "submoduletest") can be seen here:
<https://github.com/Blunk-electronic/ET_training/blob/master/submoduletest/test.scr>
As the script processor is evolving more an more commands are moving up from section "not supported" to "supported".-->

### Installation
- Currently there is no proper install script.
- Install the following packages: 
    - the GNAT Ada compiler (version 7 or later). It should come along with major linux distros.
    - make
    - gprbuild
    - gtkada

- Change into src/et and follow the instructions in readme.txt.

<!--- Run the install script install.sh as non-root user.

```sh
$ sh install.sh
```

- The script installs the executable binary et in $HOME/bin and further-on creates a hidden directory .ET in $HOME where other configuration files live.
- Currently there is nothing to do in the configuration directory -> leave it as it is.
- For help contact info@blunk-electronic.de . You are highly welcome :-)-->

#### Why Ada ??
- The only programming language that provides a robust and strong typing system is Ada.
- Objects and structures within a schematic, library and board layout are very very complex things and require sound modelling.
- If saftey/mission critical and military applications use Ada, then is must be good for an advanced ECAD system as well.
- Ada is defined by ISO/IEC 8652:2012 and MIL-STD-1815
- Ada is beautiful :-)

### Roadmap, things to do and issues
- zero-Ohms resistors
- accessories of components (screws, washers, clamps, ...)
- import EAGLE projects and libraries
- graphical user interface (GTKada based)
- web browser support so that ET can be operated on every operating system

### Collaboration
- We need a nice web site for the project.
- You are highly welcome !
