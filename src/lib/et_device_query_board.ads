------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   DEVICE QUERY OPERATIONS IN BOARD                       --
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


--with et_terminals;
with et_route_restrict.packages;		use et_route_restrict.packages;
with et_keepout;						use et_keepout;

with et_symbols;						
with et_schematic;						use et_schematic;
with et_device_query_schematic;			use et_device_query_schematic;
with et_pcb;							use et_pcb;
with et_pcb_stack;						use et_pcb_stack;
with et_packages;						use et_packages;
with et_pcb_coordinates;				use et_pcb_coordinates;
with et_board_shapes_and_text;			use et_board_shapes_and_text;
with et_pcb_contour;					use et_pcb_contour;

package et_device_query_board is

	use pac_geometry_2;
	use pac_polygons;
	use pac_contours;
	use pac_devices_sch;
	use pac_devices_non_electric;

	
-- CONDUCTOR OBJECTS:
	
	-- Returns the outlines of conductor objects of the non-electrical
	-- device (according to its position and rotation in the board) 
	-- as a list of polygons.
	-- Conductor objects are: terminals, texts, lines, arcs, circles.
	-- NOTE regarding circles: The inside of circles is ignored. Only the outer
	--  edge of a conductor circle is converted to a polygon.
	-- Adresses only those objects which are affected by
	-- the given layer category:
	function get_conductor_polygons (
		device_cursor	: in pac_devices_non_electric.cursor;
		layer_category	: in type_signal_layer_category) -- outer top, inner, outer bottom 
		return pac_polygon_list.list;

	-- Returns the outlines of conductor objects of the electrical
	-- device (according to its position and rotation in the board) 
	-- as a list of polygons.
	-- Conductor objects are: texts, lines, arcs, circles.
	-- NOTE regarding circles: The inside of circles is ignored. Only the outer
	--  edge of a conductor circle is converted to a polygon.
	-- Adresses only those objects which are affected by
	-- the given layer category.
	-- If the device is virtual, then the returned list is empty:
	function get_conductor_polygons (
		device_cursor	: in pac_devices_sch.cursor;
		layer_category	: in type_signal_layer_category) -- outer top, inner, outer bottom 
		return pac_polygon_list.list;

	-- Returns the conductor objects of the given electrical device
	-- (according to its flip status, position and rotation in the board) 
	-- Adresses only those objects affected by the given face:
	function get_conductor_objects (
		device_cursor	: in pac_devices_sch.cursor;
		layer_category	: in type_signal_layer_category)
		return type_conductor_objects;
	
	-- Returns the conductor objects of the given non-electrical device
	-- (according to its flip status, position and rotation in the board) 
	-- Adresses only those objects affected by the given face:
	function get_conductor_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		layer_category	: in type_signal_layer_category)
		return type_conductor_objects;


	
-- ROUTE RESTRICT:
	
	-- Returns the outlines of route restrict objects of the non-electrical
	-- device (according to its position and rotation in the board) 
	-- as a list of polygons.
	-- NOTE regarding circles: The inside of circles is ignored. Only the outer
	--  edge of a circle is converted to a polygon.
	-- Adresses only those objects which are affected by
	-- the given layer category:
	function get_route_restrict_polygons (
		device_cursor	: in pac_devices_non_electric.cursor;
		layer_category	: in type_signal_layer_category)
		return pac_polygon_list.list;

	-- CS function get_route_restrict_objects (
		--device_cursor	: in pac_devices_non_electric.cursor;
		--layer_category	: in type_signal_layer_category)
		-- return et_route_restrict.packages.type_one_side;
	
	-- CS likewise for electrical device

	
-- VIA RESTRICT:
	
	-- CS function get_via_restrict_objects (
		--device_cursor	: in pac_devices_non_electric.cursor;
		--layer_category	: in type_signal_layer_category)
		-- return et_via_restrict.packages.type_one_side;
	
	-- CS likewise for electrical device

	
-- KEEPOUT:
	
	-- Returns the keepout objects of the given device
	-- (according to its flip status, position and rotation in the board) 
	-- Adresses only those objects affected by the given face:
	function get_keepout_objects (
		device_cursor	: in pac_devices_sch.cursor;
		face			: in type_face)
		return type_keepout;


	-- Returns the keepout objects of the given device
	-- (according to its flip status, position and rotation in the board) 
	-- Adresses only those objects affected by the given face:
	function get_keepout_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		face			: in type_face)
		return type_keepout;

	-- CS likewise for electrical device


	
	-- CS likewise for stencil, stopmask, assy dock, silkscreen

	

-- HOLES:

	-- Returns the outlines of holes of the electrical device
	-- (according to its position and rotation in the board).
	-- If the device is virtual, then the returned list is empty:
	function get_holes (
		device_cursor	: in pac_devices_sch.cursor)
		return pac_holes.list;

	
	-- Returns the outlines of holes of the non-electrical device
	-- (according to its position and rotation in the board):
	function get_holes (
		device_cursor	: in pac_devices_non_electric.cursor)
		return pac_holes.list;
	

	-- Returns the outlines of holes of the electrical device
	-- (according to its position and rotation in the board) as
	-- a list of polygons.
	-- If the device is virtual, then the returned list is empty:
	function get_hole_polygons (
		device_cursor	: in pac_devices_sch.cursor)
		return pac_polygon_list.list;

	
	-- Returns the outlines of holes of the non-electrical device
	-- (according to its position and rotation in the board) as
	-- a list of polygon:
	function get_hole_polygons (
		device_cursor	: in pac_devices_non_electric.cursor)
		return pac_polygon_list.list;
	
	
end et_device_query_board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
