# SYSTEM ET
## An ECAD-Tool for complex schematics and layouts

### The Idea behind
- Most ECAD tools do not allow opening, checking and editing of multiple designs simultaneously.
- We need real hierarchic and modular designs.
- We need a text based, machine and human readable format for design files.
- Design checks provided by common ECAD tools are way too superficial and trivial.
- Style guides must be checked against.
- The tool must be highly scripting capable (Everything that can be done via the GUI must also be possible via commandline or script.).
- We want to do agile hardware develpment which requires the features mentioned above.
- The tool must be open sourced.
- Currently the GUI is under construction.
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
- A module file (containing schematic and layout stuff) looks like this <https://github.com/Blunk-electronic/ET_training/blob/master/single-led/main.mod>
- There is a strict separation between symbol, package/footprint and device:
- Device model <https://github.com/Blunk-electronic/ET_component_library/blob/master/devices/active/logic/7400_ext.dev>
- Symbol model <https://github.com/Blunk-electronic/ET_component_library/blob/master/symbols/logic/NAND.sym>
- Package model <https://github.com/Blunk-electronic/ET_component_library/blob/master/packages/S_SO14.pac>
- A so called rig-configuration that describes module instances and board-to-board connections <https://github.com/Blunk-electronic/ET_training/blob/master/single-led/single-led.rig>

<!--### Example of an ERC configuration file
- See this example <https://github.com/Blunk-electronic/ET/blob/master/examples/conf.txt>-->


### Documetation and User Manual

- User Manual <http://www.blunk-electronic.de/ET/pdf/caesystemet.pdf>



### Installation
- Currently there is no proper install script.
- Install the following packages: 
    - the GNAT Ada compiler (version 9 or later). It should come along with major linux distros.
    - make
    - gprbuild
    - gtkada 

- Find a installation howto for gtkada and gprbuild here <https://github.com/Blunk-electronic/ada_training>

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
- import EAGLE and KiCad V6 projects and libraries
- web browser support so that ET can be operated on every operating system

### Collaboration
- We need a nice web site for the project.
- You are highly welcome !
