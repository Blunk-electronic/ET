------------------------------------------------------------------------------
--                                                                          --
--                          SYSTEM ET NETLIST                               --
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
--   ToDo: 

-- with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
-- with ada.strings.unbounded; 	use ada.strings.unbounded;
with ada.containers;            use ada.containers;
-- with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
-- with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
-- with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_coordinates;
with et_libraries;
with et_schematic;
with et_string_processing;

package et_netlist is

	-- Whenever we deal with netlist files this type should be used:
	netlist_file_name_length : constant positive := 100; -- CS: should suffice for now
	package type_netlist_file_name is new generic_bounded_length (netlist_file_name_length); 
	--use type_netlist_file_name;

	
	use et_schematic.type_net_name;
-- 	use et_schematic.type_ports;

	-- This is a coponent port with its basic elements:
	type type_port_base is tagged record
		pin			: et_libraries.type_pin_name.bounded_string; -- the pin name like 3,4 or E3, A2
		port		: et_libraries.type_port_name.bounded_string; -- the port name like GPIO1, GPIO2
		coordinates : et_coordinates.type_coordinates;
		direction	: et_libraries.type_port_direction; -- example: "passive" -- used for ERC
		style		: et_libraries.type_port_style;	-- used for ERC
	end record;

	function port_sits_on_segment (
	-- Returns true if the given port sits on the given net segment.
		port	: in type_port_base'class;
		segment	: in et_schematic.type_net_segment'class) 
		return boolean;


-- PORTLISTS -- required as intermediate stage for netlist generation.
	-- Base ports of a component are collected in a simple list.

	package type_base_ports is new doubly_linked_lists ( 
		element_type => type_port_base); 
	use type_base_ports;

	-- The components with their ports are collected in a map with the component reference as key:
	package type_portlists is new ordered_maps (
		key_type => et_libraries.type_component_reference,
		element_type => type_base_ports.list,
		"<" => et_schematic.compare_reference);

	function build_portlists return type_portlists.map;
	-- Returns a list of components with the absolute positions of their ports as they are placed in the schematic.
	
	function first_port (component_cursor : in type_portlists.cursor) return type_base_ports.cursor;
	-- Returns a cursor pointing to the first port of a component in the portlists.




	

	use et_schematic.type_net_name;

	-- If component ports are to be listed, we need additionally the component reference:
	type type_port is new type_port_base with record
		reference	: et_libraries.type_component_reference;
	end record;

	function reference (port : in type_port) return string;
	-- Returns the component reference of the given port.

	function port (port : in type_port) return string;
	-- Returns the port name of the given port.

	function pin (port : in type_port) return string;
	-- Returns the pin name of the given port.
	
	function compare_ports (left, right : in type_port) return boolean;
	-- Returns true if left comes before right. Compares by component name and pin name.
	-- If left equals right, the return is false.	
	
	package type_ports is new ordered_sets (
		element_type => type_port,
		"<" => compare_ports);
	use type_ports;

	-- This is the netlist of a single submodule:	
	package type_netlist is new ordered_maps (
		key_type => et_schematic.type_net_name.bounded_string, -- net name like "MCU_CLOCK"
		element_type => type_ports.set); -- the list of ports connected with the net

	use type_netlist;
	use et_coordinates.type_submodule_name;
	
	-- The rig netlist is a map of netlists. For each submodule we have a
	-- distinctive netlist. The key to the particular netlist is the submodule name.
	package type_rig_netlists is new ordered_maps (
		key_type => et_coordinates.type_submodule_name.bounded_string, -- example "MOTOR_DRIVER"
		element_type => type_netlist.map); -- the netlist of the submodule

	
	-- This is the place where we store the netlists of the rig.
	rig : type_rig_netlists.map;
	module_cursor : type_rig_netlists.cursor;

	procedure first_module;
	-- Resets the module_cursor to the first module of the rig.

	function first_net return type_netlist.cursor;
	-- Returns a cursor to the first net of the current module (indicated by module_cursor).

	function net_count return count_type;
	-- Returns the number of nets of the current module.
	
	function first_port (net_cursor : in type_netlist.cursor) return type_ports.cursor;
	-- Returns a cursor to the first port of the given net in the current module (indicated by module_cursor).

	function port_count (net_cursor : in type_netlist.cursor) return count_type;
	-- Returns the number of ports of the given net of the current module.
	
	
-- 	
-- 	procedure set_module (
-- 	-- Sets the active module. Leaves module_cursor pointing
-- 	-- to the module.
-- 		module_name : in et_coordinates.type_submodule_name.bounded_string);




	
	
	procedure make_netlists;
	-- Builids the netlists of all modules in the rig.
	-- Netlists are to be exported in individual project directories in the work directory of ET.


	
	procedure write_netlists;
	-- Exports/Writes the netlists of the rig in separate files.
	-- Call this procedure after executing procedure make_netlist !	
end et_netlist;

-- Soli Deo Gloria
