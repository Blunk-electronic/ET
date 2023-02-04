------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   BOARD OPERATIONS / STOP MASK                           --
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

package et_board_ops.stop_mask is

	use et_stop_mask;


	-- CS rework procedures so that a module cursor
	-- is used instead the module_name.

	
	
	-- Draws a line in the stop mask layer.
	procedure draw_stop_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in type_stop_line;
		log_threshold	: in type_log_level);

	
	-- Draws an arc in the stop mask layer.
	procedure draw_stop_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_stop_arc;		
		log_threshold	: in type_log_level);

	
	-- Draws an circle in the stop mask layer.
	procedure draw_stop_circle (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		circle			: in type_stop_circle;
		log_threshold	: in type_log_level);

	
	-- Deletes the segment of the stop mask that crosses the given point.
	-- CS currently deletes the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
	procedure delete_stop (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		point			: in type_point; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level);

	
											
end et_board_ops.stop_mask;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
