------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                COMMAND PROCESSOR / BOARD / OUTLINE AND HOLES             --
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
--
--
--

with ada.text_io;						use ada.text_io;
with ada.characters.handling;			use ada.characters.handling;
with ada.strings; 						use ada.strings;

with et_pcb_sides;						use et_pcb_sides;
with et_pcb_signal_layers;

with et_display;						use et_display;
with et_display.board;					use et_display.board;

with et_generic_modules;				use et_generic_modules;
with et_modes.board;					use et_modes.board;

with et_board_geometry;					use et_board_geometry;
with et_board_outline; 					use et_board_outline;
with et_board_ops.outline;				use et_board_ops.outline;
with et_keywords;



package body et_cp_board_outline is


	use pac_contours;

	
	
	procedure draw_board_outline (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);

		
		-- Extract from the given command the 
		-- arguments (everything after "outline"):
		-- Example command: 
		-- "board demo draw outline line 0 0 line 50 0 line 50 50 line 0 50"
		arguments : constant type_fields_of_line := 
			remove_field (get_fields (cmd), 1, 4);

		-- Build a basic contour from the arguments:
		c : constant type_contour := type_contour (to_contour (arguments));
	begin
		-- CS log message
		
		-- Convert the contour to a pcb outer edge type
		-- and assign it to the module:
		set_outline (
			module,
			(c with null record),
			log_threshold + 1);
		
	end draw_board_outline;








	procedure draw_board_hole (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);

		-- Extract from the given command the 
		-- arguments (everything after "hole"):
		-- example command: board demo draw hole line 2 9 line 2 1 line 8 9
		arguments : constant type_fields_of_line := 
			remove_field (get_fields (cmd), 1, 4);

		-- Build a basic contour from the arguments:
		c : constant type_contour := type_contour (to_contour (arguments));
	begin
		-- CS log message
		
		-- Convert the contour to an inner pcb edge type and add it to
		-- the already existing holes:
		set_hole (module, (c with null record), log_threshold + 1);
	end draw_board_hole;


		
	
end et_cp_board_outline;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
