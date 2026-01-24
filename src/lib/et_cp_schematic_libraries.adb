------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--          COMMAND PROCESSOR / SCHEMATIC / DEVICE LIBRARY PATHS            --
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

with ada.text_io;						use ada.text_io;
with ada.characters.handling;			use ada.characters.handling;
with ada.strings; 						use ada.strings;

with et_meta_device_libraries_schematic;	use et_meta_device_libraries_schematic;
with et_schematic_ops_meta;					use et_schematic_ops_meta;


package body et_cp_schematic_libraries is


	procedure add_library_path (
   		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		path : pac_library_path_schematic.bounded_string;
		
	begin
		log (text => "add device library path (schematic)", level => log_threshold);
		log_indentation_up;

		
		-- "schematic demo set library $HOME/ET_devices"
		case cmd_field_count is
			when 5 =>
				path := to_library_path (get_field (cmd, 5));
				add_library_path (module, path, log_threshold + 1);
				
			when 6 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);

			when others =>
				command_incomplete (cmd);
		end case;


		log_indentation_down;
	end add_library_path;

	


		
end et_cp_schematic_libraries;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
