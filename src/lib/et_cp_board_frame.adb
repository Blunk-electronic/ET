------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                COMMAND PROCESSOR / BOARD / DRAWING FRAME                 --
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
-- To Do:
--
--
--

with ada.text_io;						use ada.text_io;
with ada.characters.handling;			use ada.characters.handling;
with ada.strings; 						use ada.strings;

with et_board_geometry;					use et_board_geometry;
with et_drawing_frame;					use et_drawing_frame;
with et_board_ops_frame;				use et_board_ops_frame;
with et_canvas_board;					use et_canvas_board;
with et_coordinates_abs_rel;			use et_coordinates_abs_rel;


package body et_cp_board_frame is

	use pac_geometry_2;
	use pac_drawing_frame;
	

	procedure move_drawing_frame (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
		
		p : et_drawing_frame.type_position;
		c : type_coordinates;
	begin
		-- CS log message
		
		case cmd_field_count is
			when 7 => -- board led_driver move frame absolute -20 -50
				c := to_coordinates (get_field (cmd, 5));   -- relative/absolute
				
				p.x := et_drawing_frame.to_distance (get_field (cmd, 6));
				p.y := et_drawing_frame.to_distance (get_field (cmd, 7));

				move_drawing_frame (
					module_cursor 	=> module,
					coordinates		=> c,
					point			=> p,
					log_threshold	=> log_threshold + 1
					);

				
			when 8 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others =>
				command_incomplete (cmd);
		end case;
	end move_drawing_frame;


	
end et_cp_board_frame;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
