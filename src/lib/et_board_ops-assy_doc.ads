------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                BOARD OPERATIONS / ASSEMBLY DOCUMENTATION                 --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2023                                                --
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

with et_text;
with et_assy_doc;					use et_assy_doc;
with et_assy_doc.boards;			use et_assy_doc.boards;

package et_board_ops.assy_doc is


	-- CS rework procedures so that a module cursor
	-- is used instead the module_name.
	
	use et_board_shapes_and_text;
	use pac_text_board;


	-- Draws a line in the assembly documentation.
	procedure draw_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in type_doc_line;
		log_threshold	: in type_log_level);


	-- Returns all lines in the vicinity of the given point:
	function get_lines (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		point			: in type_vector_model;
		catch_zone		: in type_catch_zone; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_doc_lines.list;
						   

	-- Modifies that status flag of a line (see package et_object_status):
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		line_cursor		: in pac_doc_lines.cursor;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);


	-- Sets the proposed-flag of all lines which are
	-- in the given zone around the given place.
	procedure propose_lines (
		module_cursor	: in pac_generic_modules.cursor;
		point			: in type_vector_model; -- x/y
		face			: in type_face;
		catch_zone		: in type_catch_zone; -- the circular area around the place
		count			: in out natural; -- the number of affected lines
		log_threshold	: in type_log_level);


	-- Clears the proposed-flag and the selected-flag of all lines:
	procedure reset_proposed_lines (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);



	type type_line_segment is record
		face	: type_face;
		cursor	: pac_doc_lines.cursor;
	end record;
	
	-- Returns the first line according to the given flag.
	-- If no line has been found, then the return is no_element:
	function get_first_line (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_line_segment;


	-- Advances to the next proposed line, starting at
	-- the given line. Traverses through the lines
	-- in a circular manner. If there are no
	-- proposed lines, then line assumes default value (no_element).
	-- If there is only one proposed line, then line is unchanged.
	-- CS last_item indicates that the last line has been reached:
	procedure next_proposed_line (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in out type_line_segment;
		-- CS last_item		: in out boolean;
		log_threshold	: in type_log_level);

	
	
	procedure move_line (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		line			: in type_doc_line;
		point_of_attack	: in type_vector_model;
		-- coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);


	
	-- Draws an arc in the assembly documentation.
	procedure draw_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_doc_arc;
		log_threshold	: in type_log_level);

	
	-- Draws a circle in the assembly documentation.
	procedure draw_circle (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;	
		circle			: in type_doc_circle;
		log_threshold	: in type_log_level);

	
	-- Deletes the segment of the assembly documentation that crosses the given point.
	-- CS currently deletes the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
	procedure delete (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		point			: in type_vector_model; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level);


	-- Deletes the given line in the given module:
	procedure delete (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		line			: in type_doc_line;
		log_threshold	: in type_log_level);

	
	-- Returns all texts in the vicinity of the given point:
	function get_texts (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		point			: in type_vector_model;		
		catch_zone		: in type_catch_zone; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_doc_texts.list;


	-- Moves a text:
	procedure move_text (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		text			: in type_doc_text;
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model;
		log_threshold	: in type_log_level);
	
end et_board_ops.assy_doc;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
