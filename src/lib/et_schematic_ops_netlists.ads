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

--   For correct displaying set tab with in your edtior to 4.

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
with et_devices_electrical;				use et_devices_electrical;
with et_netlists;
with et_net_ports;						use et_net_ports;

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
		return et_netlists.pac_device_ports_extended.set;

	

	
	-- Generates the netlists of all assembly variants from the given top module.
	-- If parameter "write_files" is true, then exports the netlists in files.
	-- The netlist files are named after the module name and the variant name.
	procedure make_netlists (
		module_cursor 	: in pac_generic_modules.cursor;
		write_files		: in boolean := false;
		log_threshold	: in type_log_level);


	-- Generates the netlist files of all assembly variants from the given top module.
	-- The netlist files are named after the module name and the variant name.
	procedure make_netlists (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		log_threshold	: in type_log_level);

	
end et_schematic_ops_netlists;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
