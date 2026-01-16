------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   COMMAND PROCESSOR / BOARD / DEVICE                     --
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



package et_cp_board_device is


	-- This procedure parses a command that shows (highlights)
	-- a device:
	-- Via the argument L1 .. L3 the amount of information to be output can
	-- be controlled. For values greate L1 a properties window is opened that
	-- fits all the information in.
	-- example: "demo led_driver show device L1 IC12"
	-- CS: For level L3 write in a file given via command argument.
	procedure show_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);



	-- This procedure parses a command to add
	-- a non-electric device.
	-- Example 1: add device $HOME/git/BEL/ET_component_library/packages/fiducials/crosshair_4.pac FD 5 10
	-- Example 2: add device $HOME/git/BEL/ET_component_library/packages/fiducials/crosshair_4.pac FD 5 10 45
	-- Example 3: add device $HOME/git/BEL/ET_component_library/packages/fiducials/crosshair_4.pac FD 5 10 45 bottom	
	procedure add_non_electrical_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);




	-- This procedure parses a command to delete
	-- a non-electrical device:
	-- Example: "board led_driver delete device FD1"
	procedure delete_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);



	-- This procedure parses a command to copy
	-- a non-electrical device:
	-- Example: "board led_driver copy device FD1 230 100"
	procedure copy_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);




	procedure move_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);



	-- This procedure parses a command that rotates a device.
	-- Example: board led_driver rotate device IC20
	-- Example: board led_driver rotate device IC20 absolute 45
	-- Example: board led_driver rotate device IC20 relative 10
	procedure rotate_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);




	-- This procedure parses a command to rename a non-electrical device:
	-- Example: board led_driver rename device FD1 FD2
	procedure rename_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);





	-- This procedure parses a command that flips a device.
	-- Example: board led_driver flip device IC20
	-- Example: board led_driver flip device IC20 bottom/top
	procedure flip_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);

	


	-- This procedure parses a command that moves a placeholder
	-- for name, value or purpose of a device.
	-- Example: "board led_driver move value R1 silkscreen top 2 absolute 100 115"
	-- Example: "board led_driver move value IC1 assy_doc bottom 2 relative -5 0"
	procedure move_device_placeholder (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);



	-- This procedure parses a command that rotates a placeholder
	-- for name, value or purpose of a device.
	-- Example: "board led_driver rotate value R1 silkscreen top 2 absolute 45"
	-- Example: "board led_driver rotate value IC1 assy_doc bottom 2 relative -10"
	procedure rotate_device_placeholder (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);




	-- This procedure parses a command that restores
	-- the placeholders of a device.
	-- Example: "board led_driver restore placeholders R1"
	procedure restore_device_placeholders (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);

	
end et_cp_board_device;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
