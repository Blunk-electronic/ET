------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             NETLISTS                                     --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 


with ada.text_io;				use ada.text_io;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
-- with ada.containers.vectors;
-- with ada.containers.doubly_linked_lists;
-- with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.multiway_trees;
-- with ada.containers.indefinite_ordered_maps;
-- with ada.containers.ordered_sets;
with ada.containers.indefinite_ordered_sets;

with et_general;				use et_general;
with et_schematic;
with et_libraries;

with et_string_processing;		use et_string_processing;



package netlists is

	-- Whenever we deal with netlist files this type should be used:
	file_name_length_max : constant positive := 100; -- CS: should suffice for now
	package type_file_name is new generic_bounded_length (file_name_length_max); 

	extension_netlist : constant string := "net";

	function to_string (name : in type_file_name.bounded_string) return string;
	function to_file_name (name : in string) return type_file_name.bounded_string;

	type type_port (direction : et_libraries.type_port_direction) is record
		device			: et_libraries.type_device_name; -- IC4		
		port			: et_libraries.type_port_name.bounded_string; -- CLOCK, CE, VDD, GND
		characteristics	: et_libraries.type_port (direction); -- direction, sensitivity, ...
		terminal		: et_libraries.type_terminal_name.bounded_string; -- H4, 1, 16
	end record;

	
	function "<" (left, right : in type_port) return boolean;
	
	package type_ports is new indefinite_ordered_sets (
		element_type	=> type_port);

	use type_ports;

	level_separator : constant character := '/';

	function to_prefix (instance : in type_module_instance_name.bounded_string) -- OSC1
	-- Converts an instance name to a net prefix with a trailing level separator.		
		return et_general.type_net_name.bounded_string;

	type type_net is record
		devices		: type_ports.set;
		submodules	: et_schematic.type_ports_submodule.set;
		netchangers	: et_schematic.type_ports_netchanger.set;
		scope		: et_schematic.type_net_scope;
	end record;
	
	type type_net_name is record
		base_name	: et_general.type_net_name.bounded_string; -- output
		prefix		: et_general.type_net_name.bounded_string; -- CLK_GENERATOR/FLT1/
	end record;
		
	function "<" (left, right : in type_net_name) return boolean;
	
	package type_nets is new ordered_maps (
		key_type		=> type_net_name, 
		element_type	=> type_net);


	type type_module is record
		name		: type_module_instance_name.bounded_string; -- OSC1
		nets		: type_nets.map;
	end record;
	
	package type_modules is new ada.containers.multiway_trees (type_module);
	
	
	procedure write_netlist (
	-- Creates the netlist (which inevitably and intentionally overwrites the previous file).
		nets			: in type_modules.tree;
		module_name		: in type_module_name.bounded_string; -- motor_driver
		file_name		: in type_file_name.bounded_string; -- netlist.net
		log_threshold	: in type_log_level);
	
end netlists;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
