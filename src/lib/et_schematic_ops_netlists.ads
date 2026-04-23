------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   SCHEMATIC OPERATIONS / NETLISTS                        --
--                                                                          --
--                               B o d y                                    --
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

--   For correct displaying set tab with in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with et_module_names;					use et_module_names;
with et_generic_modules;				use et_generic_modules;
with et_device_name;					use et_device_name;
with et_device_renumbering;				use et_device_renumbering;
with et_devices_electrical;				use et_devices_electrical;
with et_netlists;						use et_netlists;
with et_net_names;						use et_net_names;
with et_net_ports;						use et_net_ports;
with et_net_ports_devices;				use et_net_ports_devices;
with et_net_ports_submodules;			use et_net_ports_submodules;
with et_assembly_variant_name;			use et_assembly_variant_name;
with et_logging;						use et_logging;



package et_schematic_ops_netlists is

	use pac_generic_modules;

	
	-- Adds further properties to the given device ports.
	-- Ignores ports of virtual devices (like GND symbols).
	-- Additional properties are electrical characteristics
	-- and the terminal name.
	function extend_ports (
		module_cursor	: in pac_generic_modules.cursor;
		ports 			: in pac_device_ports.set)
		return pac_device_ports_extended.set;

	

	-- Collects net names of the given module and its variant
	-- in container netlist_tree. The adressed netlist is
	-- given by netlist_cursor.
	-- Collects names and ports of devices, submodules,
	-- and netchangers. Adds to the device index the given offset.
	-- If offset is zero, we are dealing with the top module.
	procedure collect_nets (
		module_cursor	: in pac_generic_modules.cursor;
		variant			: in pac_assembly_variant_name.bounded_string;
		prefix			: in pac_net_name.bounded_string; -- DRV3/OSC1/
		offset			: in type_name_index;
		netlist_tree 	: in out pac_netlist_modules.tree;
		netlist_cursor 	: in pac_netlist_modules.cursor;
		log_threshold	: in type_log_level);



	-- Builds a prefix like CLK_GENERATOR/FLT1/ for the
	-- given submodule in a tree of submodules.
	-- The prefix is composed of the overlying parent submodule
	-- instances:
	function make_prefix (
		tree_cursor		: in pac_renumber_modules.cursor)
		return pac_net_name.bounded_string;



	-- This procedure examines a given submodule "Sub-Parent"
	-- for other submodules inside.	
	-- The netlist_tree is the result of this procedure.
	-- It is extended by each submodule "Sub-Child" that 
	-- is inside "Sub-Parent".
	-- Cursor netlist_cursor points to a the submodule candidate
	-- "Sub-Parent" that is to be examined.
	procedure query_submodules (
		-- The cursor to the top-module of the whole design:
		module_cursor	: in pac_generic_modules.cursor;
		variant_name	: in pac_assembly_variant_name.bounded_string; -- of top module
		
		-- This is the netlist_tree to be extended with
		-- submodules "Sub-Childs" of the candidate submodule
		-- "Sub-Parent":
		netlist_tree 	: in out pac_netlist_modules.tree;
		
		-- This cursor points to the submodule to be examined:
		netlist_cursor 	: in out pac_netlist_modules.cursor;
		
		variant			: in out pac_assembly_variant_name.bounded_string;
		log_threshold	: in type_log_level);

	
	
	-- Generates the netlists of all assembly variants from the given top module.
	-- If parameter "write_files" is true, then exports the netlists in files.
	-- The netlist files are named after the module name and the variant name.
	procedure make_netlists (
		module_cursor 	: in pac_generic_modules.cursor;
		write_files		: in boolean := false;
		log_threshold	: in type_log_level);


	
end et_schematic_ops_netlists;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
