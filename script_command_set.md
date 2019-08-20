# SYSTEM ET SCRIPT COMMAND SET

## BASICS
Scripting is an important feature of advanced CAE tools. It allows:
- repeating and automating of tedious or error prone tasks.
- software testing without graphical user interfaces
- generating schematics, layouts and device models via third party software

A script in general is just a list of things to do by the CAE system. This list
is a plain ASCII file. Comments start with #. The script commands have a verb-noun structure.
Their meaning should be obvious in most cases. Additional explanation is provided in comments
if required. The verb-noun structure is: DOMAIN MODULE VERB NOUN PARAMETER[S]

## DELETING DEVICES AND UNITS
#############################
```
schematic submoduletest delete device R1
```

```
schematic submoduletest delete unit R1 1
```

