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

--   The two letters "CS" indicate a "construction site" where things are not
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
with ada.containers.doubly_linked_lists;
-- with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.multiway_trees;
-- with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;
with ada.containers.indefinite_ordered_sets;

with et_general;				use et_general;
with et_schematic;
with et_libraries;
with submodules;
with assembly_variants;
with et_string_processing;		use et_string_processing;



package netlists is

	-- Whenever we deal with netlist files this type should be used:
	file_name_length_max : constant positive := 100; -- CS: should suffice for now
	package type_file_name is new generic_bounded_length (file_name_length_max); 

	extension_netlist : constant string := "net";

	function to_string (name : in type_file_name.bounded_string) return string;
	function to_file_name (name : in string) return type_file_name.bounded_string;


	-- For ERC of netlists the connected devices are modelled by this type:
	type type_device_port_extended (direction : et_libraries.type_port_direction) is record
		device			: et_libraries.type_device_name; -- IC4		
		port			: et_libraries.type_port_name.bounded_string; -- CLOCK, CE, VDD, GND
		characteristics	: et_libraries.type_port (direction); -- direction, sensitivity, ...
		terminal		: et_libraries.type_terminal_name.bounded_string; -- H4, 1, 16
	end record;
	
	function "<" (left, right : in type_device_port_extended) return boolean;
	
	package type_device_ports_extended is new indefinite_ordered_sets (
		element_type	=> type_device_port_extended);

	use type_device_ports_extended;

	
	-- For inheriting net names from one module to another the ports
	-- of submodules are modelled by this type.
	-- NOTE: The selector "direction" has nothing to do with direction of
	-- energy flow. It determines whether a signal coming out of a submodule
	-- enforces its name on the net in the parent module or vice versa:
	type type_submodule_port_extended is record
		module		: type_module_instance_name.bounded_string; -- MOT_DRV_3
		port		: type_net_name.bounded_string; -- CLOCK_GENERATOR_OUT
		direction	: submodules.type_netchanger_port_name; -- master/slave
	end record;

	function "<" (left, right : in type_submodule_port_extended) return boolean;
	
	package type_submodule_ports_extended is new ordered_sets (
		element_type	=> type_submodule_port_extended);
	
	
	
	level_separator : constant character := '/';

	function to_prefix (instance : in type_module_instance_name.bounded_string) -- OSC1
	-- Converts an instance name to a net prefix with a trailing level separator.		
		return et_general.type_net_name.bounded_string;

	type type_net is tagged record
		devices		: type_device_ports_extended.set;
		submodules	: type_submodule_ports_extended.set;
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
	
	-- In the tree of modules, each module provides its
	-- generic name, instance name and a list of its nets:
	type type_module is record
		generic_name	: type_module_name.bounded_string; -- amplifier, $ET_TEMPLATES/motor_driver		
		instance_name	: type_module_instance_name.bounded_string; -- OSC1
		nets			: type_nets.map;
	end record;
	
	package type_modules is new ada.containers.multiway_trees (type_module);

	type type_netchanger_count is new natural;
	type type_submodule_count is new natural;

	type type_netchanger_ports is record
		masters	: type_netchanger_count := 0;
		slaves	: type_netchanger_count := 0;
		total	: type_netchanger_count := 0;
	end record;

	type type_submodule_ports is record
		masters	: type_submodule_count := 0;
		slaves	: type_submodule_count := 0;
		total	: type_submodule_count := 0;
	end record;
	
	type type_port_count is record
		netchangers	: type_netchanger_ports;
		submodules	: type_submodule_ports;
	end record;

	function port_count (net_cursor : in type_nets.cursor)
		return type_port_count;
	-- Returns the number of netchanger and submodule ports in the given net.

	-- A primary net enforces its name on all subordinated secondary nets.
	-- Primary nets are those which fulfil ALL follwing criteria:
	--  1. scope of net is LOCAL.
	--  2. have no netchanger slave ports. Reason: Nets with slave ports always inherit the 
	--     name of the net on the master port. 
	--  3. have no submodule ports with direction "slave". Reason: The net inside the submdule
	--     enforces its name on the net in the parent module.
	-- If a net does not fulfil any of those criteria it is a secondary net.

	-- The number of master ports in a net (of submodules or netchangers) is not limited as
	-- such a net may extend into numerous secondary nets.
	-- It is ILLEGAL for a net to have more than one slave port. Reason: The net names
	-- of primary nets would contend here.
	function is_primary (net_cursor : in type_nets.cursor) return boolean;
	-- Returns true if given net is a primary net according to the terms above.
	-- Performs some other important checks on slave ports of netchangers and submodules.


	
	-- When searching global nets in submodules we need a type for a global net of a submodule
	-- and a list thereof:
	type type_global_net is record
		--submodule	: type_module_name.bounded_string; -- amplifier, $ET_TEMPLATES/motor_driver		
		submodule	: type_modules.cursor;
		net			: type_nets.cursor;
	end record;
	
	package type_global_nets is new doubly_linked_lists (
		element_type	=> type_global_net);

	function global_nets_in_submodules (
	-- Returns a list of cursors to same named nets in submodules.
		module_cursor	: in type_modules.cursor; -- the module that contains the port
		net_cursor		: in type_nets.cursor;
		log_threshold	: in type_log_level)
		return type_global_nets.list;



	
	function net_on_netchanger (
	-- Returns a cursor to the net connected with the given netchanger
	-- port opposide to the given port.
	-- If the given port is a master, then the net connected with the
	-- slave is returned (and vice versa).
	-- If the netchanger is not connected then the return is no_element.
		module_cursor	: in type_modules.cursor; -- the module that contains the port
		port			: in et_schematic.type_port_netchanger;
		log_threshold	: in type_log_level)
		return type_nets.cursor;

	function net_in_submodule (
	-- Returns a cursor to the submodule net connected with the given
	-- submodule port.
	-- If the port is not connected inside the submodule then the return is no_element.
		module_cursor	: in type_modules.cursor; -- the module that contains the port
		port			: in type_submodule_port_extended;
		log_threshold	: in type_log_level)		
		return type_nets.cursor;

	function net_in_parent_module (
	-- Returns a cursor to the net in the parent module connected with the given net.
	-- Searches for a net in the parent module that is connected to the submodule instance
	-- given by element (module_cursor).instance_name, a port named after key (net_cursor).base_name
	-- and port direction "slave".
	-- If the net is in the top module, then the return is no_element.
	-- If the net is not connected in the parent module (via the port in the box representing
	-- the submodule instance) then the return is no_element.
		module_cursor	: in type_modules.cursor; -- the module that contains the net
		net_cursor		: in type_nets.cursor;
		log_threshold	: in type_log_level)
		return type_nets.cursor;
	
	-- The final netlist is a tree that reflects primary nets with their subordinated
	-- secondary nets.
	type type_netlist_net is new type_net with record
		name	: type_net_name; -- base_name and prefix
	end record;

	package type_netlist is new ada.containers.multiway_trees (type_netlist_net);

	
	function make_netlist (
	-- If write_file ist true, creates the netlist file (which inevitably and intentionally 
	-- overwrites the previous file).
	-- - modules contains the modules and their nets ordered in a tree structure.
	-- - module_name is the name of the top module. to be written in the header of the netlist file.
	-- - The netlist file will be named after the module name.
		modules			: in type_modules.tree;
		module_name		: in type_module_name.bounded_string; -- motor_driver 
		variant_name	: in assembly_variants.type_variant_name.bounded_string; -- low_cost
		write_file		: in boolean;
		log_threshold	: in type_log_level)
		return type_netlist.tree;
	
end netlists;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
