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

-- SCHEMATIC RELATED

 	port_name_length	: constant natural := 50;
	package type_port_name is new generic_bounded_length(port_name_length); use type_port_name;

	device_unit_name_length : constant natural := 50;
	package type_device_unit_name is new generic_bounded_length(device_unit_name_length); use type_device_unit_name;

 	device_name_in_library_length : constant natural := 100;
	package type_device_name_in_library is new generic_bounded_length(device_name_in_library_length); use type_device_name_in_library;
	
	-- The name of a pin may have 10 characters which seems sufficient for now.
 	pin_name_length	: constant natural := 10;
	package type_pin_name is new generic_bounded_length(pin_name_length); use type_pin_name;


-- COORDINATES
	subtype type_grid is et_general.type_grid range -1000.00 .. 1000.00; -- CS: unit assumed is MIL !!!

	type type_coordinates is record
		x,y				: type_grid;
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

	-- Initially, at the lowest level (usually library level), a port has a name, direction,
	-- coordinates, orientation, flags for making port and pin name visible. 
	-- Later, other values are assigned like pin name. CS: set defaults
	type type_port is record
		name              : type_port_name.bounded_string; -- example: "CLOCK"
		direction         : type_port_direction; -- example: "passive"
		coordinates       : type_coordinates;
		orientation       : type_orientation;
		display_port_name : boolean := true;
		display_pin_name  : boolean := true;
		pin               : type_pin_name.bounded_string; -- example: "144" or in case of a BGA package "E14"
 		--device            : type_device_name.bounded_string; -- example: "IC501" CS: wrong ?
	end record;



-- DEVICE
	
	-- UNIT
	-- A device unit is a sub-unit of a schematic device. EAGLE refer to them as "gate".
	-- A schematic device contains at least one unit.
	-- Examples of a unit: resistor symbol, i/o-bank of an fpga, NAND-gate

	-- outline segments 
	-- The device unit outline is composed of various elements like lines, arcs or cicles.
	
	-- Straight lines of a unit will be collected in a simple list.
	type type_device_unit_outline_segment_line is record
		coordinates_start : type_coordinates;
		coordinates_end   : type_coordinates;
	end record;
	package type_list_of_device_unit_outline_segments_lines is new doubly_linked_lists (
		element_type => type_device_unit_outline_segment_line);

	-- Arcs of a unit will be collected in a simple list.
	type type_device_unit_outline_segment_arc is record
		coordinates_start		: type_coordinates;
		coordinates_end			: type_coordinates;
		coordinates_circumfence	: type_coordinates;
	end record;
	package type_list_of_device_unit_outline_segments_arcs is new doubly_linked_lists (
		element_type => type_device_unit_outline_segment_arc);

	-- Circles of a unit will be collected in a simple list.
	type type_device_unit_outline_segment_circle is record
		coordinates_start : type_coordinates;
		coordinates_end   : type_coordinates;
		coordinates_center: type_coordinates;
	end record;
	package type_list_of_device_unit_outline_segments_circles is new doubly_linked_lists (
		element_type => type_device_unit_outline_segment_circle);

	-- Ports of a unit will be collected in a map.
	package type_list_of_device_unit_ports is new ordered_maps ( 
		key_type => type_port_name.bounded_string,
		element_type => type_port);

	-- Text fields in the library are can be regarded as attributes.
	type type_text_field is new et_general.type_text_field with record
		coordinates             : type_coordinates;
	end record;
	package type_text_fields is new doubly_linked_lists (
		element_type => type_text_field);
	
	-- A unit has a name, coordinates, consists of segment lists , ports and fields.
	-- EAGLE refers to units as "gates". KiCad refers to them as "units":
	type type_device_unit is record
		name					: type_device_unit_name.bounded_string; -- like 1,2,A,B or PWR
		--coordinates				: type_coordinates_basic; -- CS: could be relevant for a device editor
		outline_segments_lines	: type_list_of_device_unit_outline_segments_lines.list;
		outline_segments_arcs 	: type_list_of_device_unit_outline_segments_arcs.list;
		outline_segments_circles: type_list_of_device_unit_outline_segments_circles.list;
		port_list 				: type_list_of_device_unit_ports.map;
        fields					: type_text_fields.list;
        -- CS: timestamp
	end record;

	-- Units of a device will be collected in a map.
	package type_device_unit_list is new ordered_maps (
		key_type => type_device_unit_name.bounded_string, -- the key to a device unit is its own name
		element_type => type_device_unit);

	-- A device has a physical appearance, a generic name in the library, an annotation in the schematic,
	-- a list of units, ...
-- 	type type_device_physical_appearance is ( virtual, footprint); -- CS: cable , wire ?
	type type_device is new et_general.type_device with record
-- 		physical_appearance : type_device_physical_appearance := virtual; -- sometimes there is just a schematic
-- 		name_in_library 	: type_device_name_in_library.bounded_string; -- example: "TRANSISTOR_PNP"
		-- CS: library file name ?
		units			: type_device_unit_list.map;
-- 		case physical_appearance is
-- 			when footprint =>
-- 				null; 		-- CS: port-pin map ?
-- 			when others => 
-- 				null;
		-- 		end case;

	end record;

	
	-- The kicad schematic editor refers to schematic elements as "components"
	-- (EAGLE refers to them as "symbols").
	-- Components are composites of lines, arcs, circles, texts, ports:
-- 	type type_component is record
-- 		outline_segments_lines	: type_list_of_device_block_outline_segments_lines.list;
-- 		outline_segments_arcs 	: type_list_of_device_block_outline_segments_arcs.list;
-- 		outline_segments_circles: type_list_of_device_block_outline_segments_circles.list;
-- 		port_list 				: type_list_of_device_block_ports.map;
--         text_list				: type_list_of_device_block_texts.list;
-- 	end record;

	-- Components have a name and are stored in an ordered map.
	-- Within the map they are accessed by a key type_component_name (something like "CAPACITOR").
-- 	component_name_length_max : constant positive := 100; -- CS: we restrict the part name to 100 characters
-- 	package type_component_name is new generic_bounded_length(component_name_length_max); use type_component_name;
-- 	package type_list_of_components is new ordered_maps (
-- 		key_type => type_component_name.bounded_string,
-- 		element_type => type_component);

	procedure a;
	
end et_libraries;

