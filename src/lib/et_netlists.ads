------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             NETLISTS                                     --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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


with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.multiway_trees;
with ada.containers.ordered_sets;
with ada.containers.indefinite_ordered_sets;

with et_net_names;				use et_net_names;
with et_netchangers;			use et_netchangers;
with et_netchangers.schematic;	use et_netchangers.schematic;

with et_assembly_variant_name;	use et_assembly_variant_name;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;
with et_symbol_model;			use et_symbol_model;
with et_port_direction;			use et_port_direction;
with et_port_names;				use et_port_names;
with et_symbol_ports;			use et_symbol_ports;
with et_terminal_name;			use et_terminal_name;
with et_device_name;			use et_device_name;
with et_module_names;			use et_module_names;
with et_module_instance;		use et_module_instance;
with et_net_ports_netchangers;	use et_net_ports_netchangers;
with et_net_scope;				use et_net_scope;
with et_netlist_name;			use et_netlist_name;


package et_netlists is



	
	
-- DEVICES:

	-- For netlists the connected devices are modelled by this type:
	type type_device_port_extended (direction : type_port_direction) is record
		device			: type_device_name; -- IC4		
		port			: pac_port_name.bounded_string; -- CLOCK, CE, VDD, GND
		characteristics	: type_symbol_port (direction); -- direction, sensitivity, ...
		terminal		: pac_terminal_name.bounded_string; -- H4, 1, 16
	end record;

	
	function "<" (left, right : in type_device_port_extended) return boolean;
	
	package pac_device_ports_extended is new indefinite_ordered_sets (
		element_type	=> type_device_port_extended);

	use pac_device_ports_extended;




	
	
-- SUBMODULES:
	
	-- For inheriting net names from one module to another the ports
	-- of submodules are modelled by this type.
	-- NOTE: The selector "direction" has nothing to do with direction of
	-- energy flow. It determines whether a signal coming out of a submodule
	-- enforces its name on the net in the parent module or vice versa:
	type type_submodule_port_extended is record
		module		: pac_module_instance_name.bounded_string; -- MOT_DRV_3
		port		: pac_net_name.bounded_string; -- CLOCK_GENERATOR_OUT
		direction	: type_netchanger_port_name; -- master/slave
	end record;

	function "<" (left, right : in type_submodule_port_extended) return boolean;
	
	package pac_submodule_ports_extended is new ordered_sets (
		element_type	=> type_submodule_port_extended);
	
	
	
	level_separator : constant character := '/';

	-- Converts an instance name to a net prefix with a trailing level separator.
	function to_prefix (instance : in pac_module_instance_name.bounded_string) -- OSC1
		return pac_net_name.bounded_string;


	

	


	

-- NETS:
	
	type type_netlist_ports is tagged record
		devices		: pac_device_ports_extended.set;
		submodules	: pac_submodule_ports_extended.set;
		netchangers	: pac_netchanger_ports.set;
		scope		: type_net_scope;
	end record;

	
	type type_net_name is record
		base_name	: pac_net_name.bounded_string; -- output
		prefix		: pac_net_name.bounded_string; -- CLK_GENERATOR/FLT1/
	end record;

	
	function "<" (left, right : in type_net_name) return boolean;


	procedure log_net_name (
		name			: in type_net_name;
		primary			: in boolean;
		log_threshold	: in type_log_level);


	
	package pac_netlist_nets is new ordered_maps (
		key_type		=> type_net_name, 
		element_type	=> type_netlist_ports);


	use pac_netlist_nets;
	



	


-- MODULE:
	
	-- In the tree of modules, each module provides its
	-- generic name, instance name and a list of its nets:
	type type_netlist_module is record
		generic_name	: pac_module_name.bounded_string; -- amplifier, $ET_TEMPLATES/motor_driver		
		instance_name	: pac_module_instance_name.bounded_string; -- OSC1
		nets			: pac_netlist_nets.map;
	end record;

	
	package pac_netlist_modules is new multiway_trees (type_netlist_module);

	use pac_netlist_modules;




	
	

-- PORTS:

	
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
	

	-- Returns the number of netchanger and submodule 
	-- ports in the given net.
	function get_port_count (
		net_cursor : in pac_netlist_nets.cursor)
		return type_port_count;


	
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

	
	-- Returns true if given net is a primary net according to the terms above.
	-- Performs some other important checks on slave ports of netchangers and submodules.
	function is_primary (net_cursor : in pac_netlist_nets.cursor) return boolean;


	
	-- When searching global nets in submodules we need a type for a global net of a submodule
	-- and a list thereof:
	type type_global_net is record
		--submodule	: pac_module_name.bounded_string; -- amplifier, $ET_TEMPLATES/motor_driver		
		submodule	: pac_netlist_modules.cursor;
		net			: pac_netlist_nets.cursor;
	end record;

	
	package pac_global_nets is new doubly_linked_lists (
		element_type	=> type_global_net);


	
	
	-- Returns a list of cursors to same named nets in submodules.
	function global_nets_in_submodules (
		module_cursor	: in pac_netlist_modules.cursor; -- the module that contains the port
		net_cursor		: in pac_netlist_nets.cursor;
		log_threshold	: in type_log_level)
		return pac_global_nets.list;



	
	
	-- Returns a cursor to the net connected with the given netchanger
	-- port opposide to the given port.
	-- If the given port is a master, then the net connected with the
	-- slave is returned (and vice versa).
	-- If the netchanger is not connected then the return is no_element.
	function net_on_netchanger (
		module_cursor	: in pac_netlist_modules.cursor; -- the module that contains the port
		port			: in type_port_netchanger;
		log_threshold	: in type_log_level)
		return pac_netlist_nets.cursor;



	
	-- Returns a cursor to the submodule net connected with the given
	-- submodule port.
	-- If the port is not connected inside the submodule then the return is no_element.
	function net_in_submodule (
		module_cursor	: in pac_netlist_modules.cursor; -- the module that contains the port
		port			: in type_submodule_port_extended;
		log_threshold	: in type_log_level)		
		return pac_netlist_nets.cursor;



	
	-- Returns a cursor to the net in the parent module connected with the given net.
	-- Searches for a net in the parent module that is connected to the submodule instance
	-- given by element (module_cursor).instance_name, a port named after key (net_cursor).base_name
	-- and port direction "slave".
	-- If the net is in the top module, then the return is no_element.
	-- If the net is not connected in the parent module (via the port in the box representing
	-- the submodule instance) then the return is no_element.
	function net_in_parent_module (
		module_cursor	: in pac_netlist_modules.cursor; -- the module that contains the net
		net_cursor		: in pac_netlist_nets.cursor;
		log_threshold	: in type_log_level)
		return pac_netlist_nets.cursor;



	
	-- The final netlist is a tree that reflects 
	-- primary nets with their subordinated secondary nets:
	type type_netlist_net is new type_netlist_ports with record
		name	: type_net_name; -- base_name and prefix
	end record;


	
	-- The netlist is a tree containing primary nets as top level nodes and
	-- lots of subordinated secondary nets. A secondary net itself may have lots 
	-- of further secondary nets. We limit the nesting depth to a reasonable value.
	nesting_depth_max : constant positive := 100; -- CS increase if nessecary

	
	package pac_module_netlist is new multiway_trees (type_netlist_net);
	use pac_module_netlist;





	
	
	use pac_assembly_variant_name;
	

	-- As there are assembly variants, for each of them 
	-- a dedicated netlist must be generated.
	-- The key into the list is the assembly variant name
	-- like "low_cost". It is empty if the default variant is adressed.
	-- The element is a tree of netlists. It provides info on primary 
	-- and secondary net dependencies:
	package pac_module_netlists is new ordered_maps (
		key_type		=> pac_assembly_variant_name.bounded_string,
		element_type	=> pac_module_netlist.tree);



	
	
end et_netlists;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
