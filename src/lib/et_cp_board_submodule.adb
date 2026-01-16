------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                 COMMAND PROCESSOR / BOARD / SUBMODULE                    --
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
-- To Do:
-- - rework
--
--

with ada.text_io;						use ada.text_io;
with ada.characters.handling;			use ada.characters.handling;
with ada.strings; 						use ada.strings;

with et_board_geometry;					use et_board_geometry;
with et_module_instance;				use et_module_instance;
with et_board_ops;						use et_board_ops;



package body et_cp_board_submodule is

	use pac_geometry_2;
	use pac_generic_modules;
	
	

	procedure move_submodule (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
		
	begin
		-- CS log message

		case cmd_field_count is
			when 8 =>
				move_submodule (
					module_name 	=> key (module),
					instance		=> to_instance_name (get_field (cmd, 5)), -- OSC1
					coordinates		=> to_coordinates (get_field (cmd, 6)),  -- relative/absolute
					point			=> type_vector_model (set (
										x => to_distance (dd => get_field (cmd, 7)),
										y => to_distance (dd => get_field (cmd, 8)))),
					log_threshold	=> log_threshold + 1
					);

			when 9 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others =>
				command_incomplete (cmd);
		end case;
		
	end move_submodule;


		
end et_cp_board_submodule;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
