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
with ada.containers.ordered_maps;

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

	unit_name_length : constant natural := 50;
	package type_unit_name is new generic_bounded_length(unit_name_length); use type_unit_name;

 	component_name_length_max : constant natural := 100;
	package type_component_name is new generic_bounded_length(component_name_length_max); use type_component_name;
	
	-- The name of a pin may have 10 characters which seems sufficient for now.
 	pin_name_length	: constant natural := 10;
	package type_pin_name is new generic_bounded_length(pin_name_length); use type_pin_name;



-- COORDINATES
	subtype type_grid is et_general.type_grid range -10000.00 .. 10000.00; -- CS: unit assumed is MIL !!!

	type type_coordinates is record
		x,y				: type_grid;
	end record;

	
-- TEXT FIELD
	type type_text_field is new et_general.type_text_field with record
		coordinates		: type_coordinates;
	end record;


-- PORT
	-- A port is something where a net can be attached at.
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



-- DEVICE
	
	-- UNIT
	-- A unit is a sub-unit of a component. EAGLE refer to them as "gates".
	-- A component contains at least one unit.
	-- Examples of a unit: resistor symbol, i/o-bank of an fpga, NAND-gate

	-- outline segments 
	-- The unit outline is composed of various elements like lines, arcs or cicles.
	
	-- Straight lines of a unit will be collected in a simple list.
	type type_line is record
		coordinates_start : type_coordinates;
		coordinates_end   : type_coordinates;
	end record;
	package type_lines is new doubly_linked_lists (
		element_type => type_line);

	-- Arcs of a unit will be collected in a simple list.
	type type_arc is record
		coordinates_start		: type_coordinates;
		coordinates_end			: type_coordinates;
		coordinates_circumfence	: type_coordinates;
	end record;
	package type_arcs is new doubly_linked_lists (
		element_type => type_arc);

	-- Circles of a unit will be collected in a simple list.
	type type_circle is record
		coordinates_start : type_coordinates;
		coordinates_end   : type_coordinates;
		coordinates_center: type_coordinates;
	end record;
	package type_circles is new doubly_linked_lists (
		element_type => type_circle);

	-- Ports of a unit will be collected in a map.
	package type_ports is new ordered_maps ( 
		key_type => type_port_name.bounded_string,
		element_type => type_port);

	-- Text fields in the library are can be regarded as attributes.
	type type_field is new et_general.type_text_field with record
		coordinate	: type_coordinates;
	end record;
	package type_fields is new doubly_linked_lists (
		element_type => type_field);

	swap_level_max : constant natural := 10;
	type type_swap_level is new natural range 0..swap_level_max;
	swap_level_default : constant := type_swap_level'first;
	
	-- A unit has coordinates, consists of segment lists , ports and fields.
	-- EAGLE refers to units as "gates". KiCad refers to them as "units":
	type type_unit is record
		swap_level	: type_swap_level;
		lines		: type_lines.list;
		arcs 		: type_arcs.list;
		circles		: type_circles.list;
		port_list 	: type_ports.map;
        fields		: type_fields.list;
	end record;

	-- Units of a component will be collected in a map.
	package type_units is new ordered_maps (
		key_type => type_unit_name.bounded_string, -- the key to a unit is its name
		element_type => type_unit);

	type type_component is new et_general.type_component with record
		units			: type_units.map;
	end record;

	
	-- Components are stored in an ordered map.
	-- Within the map they are accessed by a key type_component_name (something like "CAPACITOR").
	package type_components is new ordered_maps (
		key_type => type_component_name.bounded_string, -- example: "TRANSISTOR_PNP"
		element_type => type_component);
	use type_components;

	package type_libraries is new ordered_maps (
		key_type => type_library_full_name.bounded_string,
		element_type => type_components.map);
	
	procedure a;
	
end et_libraries;

