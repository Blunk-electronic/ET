------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                      BOARD OPERATIONS FRAME                              --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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

with ada.text_io;					use ada.text_io;
with et_canvas_board_2;


package body et_board_ops.frame is


	use pac_devices_sch;
	use pac_devices_non_electric;
	use pac_nets;


	
	procedure move_drawing_frame (
		module_cursor	: in pac_generic_modules.cursor;
		coordinates		: in type_coordinates; -- relative/absolute		
		point			: in et_frames.type_position; -- x/y
		log_threshold	: in type_log_level) 
	is
		use et_canvas_board_2;
		use pac_drawing_frame;

		
		procedure set_origin (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is 
			p1 : et_frames.type_position;
		begin
			case coordinates is
				when ABSOLUTE =>
					et_frames.set_position (module.board.frame.frame, point);

				when RELATIVE =>
					p1 := et_frames.get_position (module.board.frame.frame);
					-- CS
					-- move_by (module.board.frame.position, to_distance_relative (point));
			end case;
		end set_origin;

		
		use et_frames;
		
	begin
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (key (module_cursor)) &
					 " moving drawing frame origin to " & to_string (point),
					 level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (key (module_cursor)) &
					 " moving drawing frame origin by " & to_string (point), 
					 level => log_threshold);
		end case;

		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> set_origin'access);

	end move_drawing_frame;





	function get_frame_position (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)								
		return et_frames.type_position
	is
		result : et_frames.type_position;

		
		procedure get_origin (
			module_name	: in pac_module_name.bounded_string;
			module 		: in type_module) 
		is begin
			result := et_frames.get_position (module.board.frame.frame);
		end get_origin;

		
	begin
		log (text => "module " & to_string (key (module_cursor)) &
			" query_frame drawing frame position",
		level => log_threshold);

		
		query_element (
			position	=> module_cursor,
			process		=> get_origin'access);

		
		return result;
	end get_frame_position;

		

end et_board_ops.frame;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16