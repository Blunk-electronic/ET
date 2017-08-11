------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET LIBRARIES                            --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 Mario Blunk, Blunk electronic                 --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
------------------------------------------------------------------------------

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
with ada.text_io;				use ada.text_io;

with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;
--with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_general;				use et_general;

package et_libraries is

	-- LIBRARY NAMES AND DIRECTORIES

	-- For storing bare library names like "bel_primitives" we use this bounded string:
	library_name_length_max : constant natural := 100; -- CS: increase if necessary
    package type_library_name is new generic_bounded_length(library_name_length_max); use type_library_name;

	-- Bare library names can be stored further-on in an ordered set like this:
	-- We use a doubly linked list because the order of the library names sometimes matters.
    package type_list_of_library_names is new doubly_linked_lists (
		element_type => type_library_name.bounded_string);

	project_libraries : type_list_of_library_names.list;
	
	-- The base directory where libraries live is stored in a bounded string:
	library_directory_length_max : constant positive := 300; -- CS: increase if necessary
	package type_library_directory is new generic_bounded_length(library_directory_length_max); use type_library_directory;

	lib_dir : type_library_directory.bounded_string; -- here the path to the project libraries is stored
	-- CS: this should be a list of paths
	
	-- If a library is fully specified with path, name and extension we store them in bounded strings:
	library_full_name_max : constant positive := library_directory_length_max + library_name_length_max + 4;
	package type_library_full_name is new generic_bounded_length(library_full_name_max);
	use type_library_full_name;

	-- Full library names can be stored furhter-on in an ordered set like this:
	-- We use a doubly linked list because the order of the library names sometimes matters.
-- 	package type_list_of_full_library_names is new doubly_linked_lists (
-- 		element_type => type_library_full_name.bounded_string);

	
-- SCHEMATIC RELATED

 	port_name_length	: constant natural := 50;
	package type_port_name is new generic_bounded_length(port_name_length); use type_port_name;

 	component_name_length_max : constant natural := 100;
	package type_component_name is new generic_bounded_length(component_name_length_max); use type_component_name;
	
	-- The name of a pin may have 10 characters which seems sufficient for now.
 	pin_name_length	: constant natural := 10;
	package type_pin_name is new generic_bounded_length(pin_name_length); use type_pin_name;

-- LAYOUT RELATED

	-- PACKAGES AND VARIANTS
	-- A component package is something like "SOT32" or "NDIP14". It is a more or less standardized (JEDEC)
	-- designator for the housing or the case of an electronical component. The package name is independed of
	-- the actual purpose of a component. An LED can have an SOT23 package and a transistor can also come in an SOT23.

	-- The variant is usually a suffix in a component name, given by its manufacturer. The variant is a manufacturer
	-- specific abbrevation for the package a component comes with.
	-- Example: An opamp made by TI can be the type TL084N or TL084D. N means the NDIP14 package
	-- whereas D means the SO14 package.

	-- component package names like "SOT23" or "TO220" are stored in bounded strings:
	-- Kicad refers to them as "footprints".
	component_package_name_length_max : constant positive := 100;
	package type_component_package_name is new generic_bounded_length(component_package_name_length_max);
	--use type_component_package_name;

	-- component package variant names like "N" or "D" are stored in short bounded strings:
	component_variant_name_length_max : constant positive := 10;
	package type_component_variant_name is new generic_bounded_length(component_variant_name_length_max);
	use type_component_variant_name;

	-- A component variant is a composite of the package, the library it is stored in and
	-- a connection list. The connection list maps from port names to pin/pad names.
	type type_component_variant is record
		packge	: type_component_package_name.bounded_string;
		library	: type_library_full_name.bounded_string;
		-- CS: connection list
	end record;

	-- Component variants are stored in a map where they are accessed by the variant name.
	package type_component_variants is new ordered_maps (
		key_type => type_component_variant_name.bounded_string,
		element_type => type_component_variant);
	
-- TEXT FIELDS

	-- A text field in the library gets extended by simple coordinates.
	-- Text fields can be regarded as attributes. Some of them are mandatory.
	-- They can be collected in a simple list.
	type type_text (meaning : et_general.type_text_meaning) is new et_general.type_text with record
		coordinates		: et_general.type_coordinates;
	end record;

	package type_texts is new indefinite_doubly_linked_lists (
		element_type => type_text);
	
	type type_texts_basic is record
		reference	: type_text (meaning => et_general.reference);
		value		: type_text (meaning => et_general.value);
		commissioned: type_text (meaning => et_general.commissioned);
		updated		: type_text (meaning => et_general.updated);
		author		: type_text (meaning => et_general.author);
	end record;
	
-- PORTS
	
	-- A port is something where a net can be attached to.
	-- The name of a port represents the function of the port like (A14 or RST_N)

	-- The port has an electrical direction:
	type type_port_direction is (
		DIGIAL_IN,
		DIGIAL_OUT,
		ANALOG_IN,
		ANALOG_OUT,
		PASSIVE, 		-- no explicit direction
		NOT_CONNECTED,
		POWER_OUT, 		-- a power source
		POWER_IN		-- a power sink
		);

	type type_port_visible is ( ON, OFF);
	type type_pin_visible is ( ON, OFF);
	
	-- Initially, at the lowest level (usually library level), a port has a name, direction,
	-- coordinates, orientation, flags for making port and pin name visible. 
	-- Later, other values are assigned like pin name. CS: set defaults
	type type_port is record
		name              : type_port_name.bounded_string; -- example: "CLOCK" -- CS: should be the key in the port list instead
		-- port swap level ?
		
		-- Kicad requirement: sometimes the supply port name has a special position
		name_position_offset : type_grid; 
		
		direction         : type_port_direction; -- example: "passive"
		coordinates       : type_coordinates;
		orientation       : type_orientation;
		port_name_visible : type_port_visible;
		pin_name_visible  : type_pin_visible;
		pin               : type_pin_name.bounded_string; -- example: "144" or in case of a BGA package "E14"
		-- pin_position_offset ?
	end record;

	-- Ports are collected in a map.
	package type_ports is new ordered_maps ( 
		key_type => type_port_name.bounded_string,
		element_type => type_port);



	
-- SHAPES 
	
	-- Straight lines are collected in a simple list.
	type type_line is record
		coordinates_start : type_coordinates;
		coordinates_end   : type_coordinates;
	end record;
	package type_lines is new doubly_linked_lists (
		element_type => type_line);

	-- Arcs are collected in a simple list.
	type type_arc is record
		coordinates_start		: type_coordinates;
		coordinates_end			: type_coordinates;
		coordinates_circumfence	: type_coordinates;
	end record;
	package type_arcs is new doubly_linked_lists (
		element_type => type_arc);

	-- Circles are collected in a simple list.
	type type_circle is record
		coordinates_start : type_coordinates;
		coordinates_end   : type_coordinates;
		coordinates_center: type_coordinates;
	end record;
	package type_circles is new doubly_linked_lists (
		element_type => type_circle);

	-- Shapes are wrapped in a the type_shapes:
	type type_shapes is record
		lines		: type_lines.list;
		arcs 		: type_arcs.list;
		circles		: type_circles.list;
	end record;


-- SYMBOLS AND UNITS

	-- In schematics electrical components like resistors, capactors and inductors are called "symbols".
	-- Since they are frequently used we store such things in symbol libraries like bel_primitives.sym.
	-- The symbol name is something very general like "NAND", "Resistor", "Switch"

	-- A component has one or more units. A unit is a subsection of a component (EAGLE refer to them as "gates").
	-- There are internal units, which exist for the particular component exclusively. 
	-- An internal unit has a symbol and furhter properties like a swap level.
	-- There are external units, which are used for frequently used symbols like resistors or capacitors.
	-- An external unit is just a reference to a symbol library, the symbol name therein and other properties
	-- like swap level.	
	-- The unit name is something like "I/O Bank 3", "PWR" or "Switch 1" "Switch 2"

	-- SYMBOLS
	
	symbol_name_length_max : constant natural := 50;
	package type_symbol_name is new generic_bounded_length(symbol_name_length_max); use type_symbol_name;
	
	type type_symbol is record
		shapes		: type_shapes;
		ports		: type_ports.map;
		texts_basic	: type_texts_basic;
	end record;


	-- UNITS GENERAL

	unit_name_length_max : constant natural := 50;	
	package type_unit_name is new generic_bounded_length(unit_name_length_max); use type_unit_name;	
	
	unit_swap_level_max : constant natural := 10;
	type type_unit_swap_level is new natural range 0..unit_swap_level_max;
	unit_swap_level_default : constant := type_unit_swap_level'first;

	
	-- INTERNAL UNITS
	
	-- An internal unit is a symbol with a swap level.
	-- An internal unit is owned by a particular component exclusively.
	type type_unit_internal is record
		symbol		: type_symbol;
		coordinates	: type_coordinates;
		swap_level	: type_unit_swap_level;
	end record;

	-- Internal units are collected in a map:
	package type_units_internal is new ordered_maps (
		key_type => type_unit_name.bounded_string, -- like "I/O-Bank 3" "A" or "B"
		element_type => type_unit_internal);


	-- EXTERNAL UNITS

	-- External units have a reference to an external symbol.
	-- External units are stored in a library and are shared by many components.
	type type_unit_reference is record
		library		: type_library_full_name.bounded_string; -- like /my_libraries/logig.sym
		name		: type_symbol_name.bounded_string;		 -- like "NAND" or "Resistor" or "Switch"
	end record;

	-- An external unit has a reference and a swap level.
	type type_unit_external is record
		reference	: type_unit_reference;
		coordinates	: type_coordinates;
		swap_level	: type_unit_swap_level;		
	end record;

	-- External units are collected in a map;
	package type_units_external is new ordered_maps (
		key_type => type_unit_name.bounded_string,		 -- like "I/O-Bank 3"
		element_type => type_unit_external);

	
	-- COMPONENTS

	-- A component may have internal and/or external units.
	-- It also has text fields.
	type type_component (appearance : et_general.type_component_appearance) is record
		units_internal	: type_units_internal.map;
		units_external	: type_units_external.map;
		texts_basic		: type_texts_basic; -- basic text fields
		-- In addition to the basic text fields we need more text fields:
		partcode		: type_text(meaning => et_general.partcode); -- like "R_PAC_S_0805_VAL_"
		fnction			: type_text(meaning => et_general.p_function); -- to be filled in schematic later by the user
		datasheet		: type_text(meaning => et_general.datasheet); -- might be useful for some special components
		case appearance is

			-- If a component appears in the schematic only, it does not
			-- have any package variants.
			when et_general.sch => null;

			-- If a component appears in both schematic and layout it comes 
			-- with at least one package variant. We store variants in a map.
			when et_general.sch_pcb => 
				variants :	type_component_variants.map;

			
			when others => null; -- CS
		end case;

	end record;
	
	-- Components are stored in a map.
	-- Within the map they are accessed by a key type_component_name (something like "CAPACITOR").
	package type_components is new indefinite_ordered_maps (
		key_type => type_component_name.bounded_string, -- example: "TRANSISTOR_PNP"
		element_type => type_component);
	use type_components;


	
	package type_libraries is new ordered_maps (
		key_type => type_library_full_name.bounded_string,
		element_type => type_components.map);
	
	procedure a;
	
end et_libraries;

-- Soli Deo Gloria
