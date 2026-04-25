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


with et_generic_modules;				use et_generic_modules;
with et_net_ports_devices;				use et_net_ports_devices;
with et_net_ports_submodules;			use et_net_ports_submodules;
with et_netlist_devices;				use et_netlist_devices;
with et_netlist_submodules;				use et_netlist_submodules;
with et_assembly_variant_name;			use et_assembly_variant_name;
with et_logging;						use et_logging;



package et_schematic_ops_netlists_2 is

	use pac_generic_modules;

	type type_netlist_category is (
		NETLIST_CAT_1,
		NETLIST_CAT_2);
		-- CS: others ?
		
		
	function to_string (
		category	: in type_netlist_category)
		return string;
		
		
	-- Converts from a string like 1, 2, 3, to 
	-- a type_netlist_category:
	function to_netlist_category (
		category	: in string)
		return type_netlist_category;
		
		
		
	-- Adds further properties to the given device ports.
	-- Ignores ports of virtual devices (like GND symbols).
	-- Additional properties are electrical characteristics
	-- and the terminal name.
	function extend_ports (
		module_cursor	: in pac_generic_modules.cursor;
		ports 			: in pac_device_ports.set)
		return pac_device_ports_extended.set;

		
	-- Adds further properties to the given submodule ports:
	function extend_ports (
		module_cursor	: in pac_generic_modules.cursor;
		ports 			: in pac_net_submodule_ports.set)
		return pac_submodule_ports_extended.set;
		
		
		
		
	-- Generates for the given assembly variant of a
	-- generic module a netlist of category 1:
	procedure make_netlist_cat_1 (
		module_cursor 	: in pac_generic_modules.cursor;
		variant			: in pac_assembly_variant_name.bounded_string;
		log_threshold	: in type_log_level);


	
end et_schematic_ops_netlists_2;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
