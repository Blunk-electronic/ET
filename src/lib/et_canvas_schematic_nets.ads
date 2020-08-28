------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS SCHEMATIC NETS                            --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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
--                                                                          --
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
-- DESCRIPTION:
-- 

with et_general;					use et_general;
with et_coordinates;				use et_coordinates;
use et_coordinates.pac_geometry_sch;

with et_project.modules;			use et_project.modules;
with et_schematic;
with et_schematic_ops;
with et_schematic_ops.nets;
with et_string_processing;			use et_string_processing;

package et_canvas_schematic_nets is

	use et_project.modules.pac_generic_modules;
	
	-- Deletes a net segment in the vicinity of given point.
	-- If more than one segment near point found, then it sets the
	-- cursor selected_segment to the first segment and requests
	-- for clarification.
	procedure delete_net_segment (point : in type_point);

	-- Advances cursor selected_segment to next segment in list selected_segments.
	procedure clarify_net_segment;

	-- Deletes the net segment being pointed at by cursor selected_segment.
	procedure delete_selected_net_segment;

	-- When a net route is being drawn, then this global variable
	-- shall be used:
	net_route : et_schematic.pac_shapes.type_route_live;
	
	-- Resets the components of the net route.
	-- Exception: Leaves the bend style as it is.
	procedure reset_net_route;

	-- Inserts a net segment in the module.
	procedure insert_net_segment (
		module			: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		segment			: in et_schematic.type_net_segment;
		log_threshold	: in type_log_level);

	function valid_for_net_segment (
		point			: in type_point;
		log_threshold	: in type_log_level)
		return boolean;
	
end et_canvas_schematic_nets;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
