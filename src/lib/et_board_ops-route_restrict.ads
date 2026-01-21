------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   BOARD OPERATIONS / ROUTE RESTRICT                      --
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

with et_board_geometry;					use et_board_geometry;
use et_board_geometry.pac_geometry_2;

with et_module_names;					use et_module_names;
with et_generic_modules;				use et_generic_modules;
with et_route_restrict.boards;			use et_route_restrict.boards;

with et_logging;						use et_logging;


package et_board_ops.route_restrict is


	use pac_generic_modules;
	

	-- CS rework procedures so that a module cursor
	-- is used instead the module_name.

	-- Draws route restrict line.
	procedure draw_route_restrict_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		line			: in type_route_restrict_line;
		log_threshold	: in type_log_level);

	
	-- Draws a route restrict arc.
	procedure draw_route_restrict_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		arc				: in type_route_restrict_arc;
		log_threshold	: in type_log_level);

	
	-- Draws a route restrict circle.
	procedure draw_route_restrict_circle (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		circle			: in type_route_restrict_circle;
		log_threshold	: in type_log_level);	

	
	-- Deletes the segment of route restrict that crosses the given point.
	-- CS currently rips up the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
	procedure delete_route_restrict (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level);
	

	-- Draws a route restrict zone:
	procedure draw_zone (
		module_cursor	: in pac_generic_modules.cursor;
		zone			: in type_route_restrict_contour;
		log_threshold	: in type_log_level);


	
end et_board_ops.route_restrict;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
