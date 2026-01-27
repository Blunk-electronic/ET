------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                  COMMAND PROCESSOR / BOARD / CANVAS                      --
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
-- - propose arguments if command incomplete--
--
--

with ada.text_io;						use ada.text_io;

with et_generic_modules;				use et_generic_modules;
with et_runmode;						use et_runmode;
with et_modes.board;					use et_modes.board;
with et_board_ops_grid;

with et_canvas.cmd;
with et_canvas_board;					use et_canvas_board;




package body et_cp_board_canvas is
		

	package pac_canvas_cmd is new et_canvas_board.pac_canvas.cmd;
	use pac_canvas_cmd;


	
	procedure zoom_all (
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);
	begin
		log (text => "zoom all", level => log_threshold);
		log_indentation_up;

		-- Zoom commands can only be executed in a graphical runmode:
		case runmode is
			when MODE_MODULE =>

				case noun is
					when NOUN_ALL => -- zoom all
						case cmd_field_count is
							when 4 => 
								log (text => "zoom all", level => log_threshold + 1);
								zoom_to_fit_all;

							when 5 .. type_field_count'last => 
								command_too_long (cmd, cmd_field_count - 1);

							when others =>
								command_incomplete (cmd);
								
						end case;

					when others => 
						null;

				end case;

				
			when others =>
					skipped_in_this_runmode (log_threshold + 1);
					
		end case;

		log_indentation_down;
	end zoom_all;

	



	procedure set_zoom (
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is begin
		log (text => "set zoom", level => log_threshold);
		log_indentation_up;

		parse_canvas_command (cmd, VERB_SET, NOUN_ZOOM);
		
		log_indentation_down;
	end set_zoom;


	

	
	

	procedure set_grid (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		use et_board_ops_grid;
	begin
		log (text => "set grid", level => log_threshold);
		log_indentation_up;

		-- Set the grid on the canvas:
		parse_canvas_command (cmd, VERB_SET, NOUN_GRID);

		-- The global variable "grid" has now been set
		-- as requested by the operator.
		
		-- Assign the grid in the database:
		set_grid (
			module_name 	=> pac_generic_modules.key (module),
			grid			=> pac_canvas.grid,
			log_threshold	=> log_threshold + 1);

		log_indentation_down;
	end set_grid;



	


	procedure set_scale (
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is begin
		log (text => "set scale", level => log_threshold);
		log_indentation_up;

		parse_canvas_command (cmd, VERB_SET, NOUN_SCALE);
		
		-- The global scale variable "M" has now been set
		-- as requested by the operator.
		
		-- CS: scale_objects (see demo program)

		-- CS: Assign the scale in the database.
		log_indentation_down;
	end set_scale;





	procedure move_cursor (
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is begin
		log (text => "move cursor", level => log_threshold);
		log_indentation_up;

		-- CS parse_canvas_command (cmd, VERB_MOVE, NOUN_CURSOR);
		-- CS system hangs
		
		log_indentation_down;
	end move_cursor;

	



	procedure set_cursor (
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is begin
		log (text => "set cursor", level => log_threshold);
		log_indentation_up;

		parse_canvas_command (cmd, VERB_SET, NOUN_CURSOR);
		
		log_indentation_down;
	end set_cursor;





	procedure set_color (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is begin
		log (text => "set color", level => log_threshold);
		log_indentation_up;

		parse_canvas_command (cmd, VERB_SET, NOUN_COLOR);
		redraw_board;
		
		log_indentation_down;
	end set_color;
	
		
end et_cp_board_canvas;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
