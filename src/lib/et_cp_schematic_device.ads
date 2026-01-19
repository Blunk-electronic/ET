------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--               COMMAND PROCESSOR / SCHEMATIC / DEVICE                     --
--                                                                          --
--                               S p e c                                    --
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
-- <http://www.gnu.org/licenses/>.
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

with et_generic_modules;		use et_generic_modules;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;
with et_cmd_sts;				use et_cmd_sts;



package et_cp_schematic_device is

	-- This procedure parses a command that adds an electrical
	-- device to the module.
	-- example: "schematic demo add device $HOME/git/BEL/ET_component_library/devices/active/logic/7400_ext.dev 1 100 140 0 D"
	procedure add_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);


	-- This procedure parses a command that renames a device like
	-- "schematic led_driver rename device IC1 IC2"
	procedure rename_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);

		
		
	-- This procedure parses a command that deletes a 
	-- whole device (with all units) like
	-- "schematic led_driver delete device IC1":
	procedure delete_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);


		
	-- This procedure parses a command that copies a 
	-- device like
	-- "schematic led_driver copy device IC1 2 210 100 0":	
	procedure copy_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);
		
		
	
	-- This procedure parses a command that sets the value of a device like
	-- "schematic led_driver set value R1 100R"
	procedure set_device_value (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);


		
	-- This procedure parses a command that sets the purpose of a device like
	-- "schematic led_driver set purpose R1 Temperature"
	procedure set_device_purpose (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);

		
		
		
	-- This procedure parses a command that sets the partcode of a device like
	-- "schematic led_driver set partcode R1 R_PAC_S_0805_VAL_100R"
	procedure set_device_partcode (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);

		
		
	-- This procedure parses a command that sets the package
	-- variant of a device.
	-- Example: "schematic led_driver set variant R1 S_0805"
	procedure set_device_package_variant (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);
		


	-- Renumbers devices according to the sheet number.
	procedure renumber_devices (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);


	
	
end et_cp_schematic_device;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
