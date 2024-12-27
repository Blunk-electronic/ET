------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   BOARD OPERATIONS / BOARD CONTOURS                      --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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

with et_pcb_contour;				use et_pcb_contour;


package et_board_ops.board_contour is

	-- CS rework procedures so that a module cursor
	-- is used instead the module_name.

	use et_board_shapes_and_text;
	use pac_contours;
	


	-- Assigns the given module a PCB outer edge.
	-- Overwrites the already existing outline as there can
	-- be only one outline:
	procedure set_outline (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		outline			: in type_outer_contour;
		log_threshold	: in type_log_level);


	-- Draws a contour.
	-- The given contour can consist of a single segment or a
	-- fragment of a contour.
	-- 1. If the given contour is a single segment or a fragment
	--    then the procedure searches for an already existing contour
	--    which is incomplete (or open) and tries to append or prepend
	--    the given contour fragment to the existing open contour.
	-- 2. If this attempt fails, then the given contour is rejected.
	-- 3. If the existing contour is already closed, then the given fragment
	--    is rejected.
	procedure draw_outline (
		module_cursor	: in pac_generic_modules.cursor;
		outline			: in type_outer_contour;
		log_threshold	: in type_log_level);

	
	-- Returns the outer edge of the PCB:
	function get_outline (
		module_cursor	: in pac_generic_modules.cursor)
		return type_outer_contour;

	
	function get_outline (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		log_threshold	: in type_log_level)
		return type_outer_contour;
	

	-- Deletes the segment of the outline that crosses the given point.
	-- CS currently rips up the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
	procedure delete_outline (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		point			: in type_vector_model; -- x/y
		accuracy		: in type_accuracy;
		log_threshold	: in type_log_level);

	
	
	
	-- Adds a hole to the already existing holes:
	procedure add_hole (
		module_cursor	: in pac_generic_modules.cursor;
		hole			: in type_hole;
		log_threshold	: in type_log_level);

	
	procedure add_hole (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		hole			: in type_hole;
		log_threshold	: in type_log_level);

	
	-- Returns the holes of the given module:
	function get_holes (
		module_cursor	: in pac_generic_modules.cursor)
		return pac_holes.list;

	
	-- Deletes the segment of a hole that crosses the given point.
	-- CS currently rips up the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
	procedure delete_hole (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		point			: in type_vector_model; -- x/y
		accuracy		: in type_accuracy;
		log_threshold	: in type_log_level);


	
	
end et_board_ops.board_contour;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
