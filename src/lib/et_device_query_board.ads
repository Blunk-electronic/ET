------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   DEVICE QUERY OPERATIONS IN BOARD                       --
--                                                                          --
--                               S p e c                                    --
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


with et_terminals;						use et_terminals;
with et_route_restrict.packages;
with et_via_restrict.packages;
with et_keepout;						use et_keepout;
with et_stopmask;						use et_stopmask;
with et_stopmask.packages;
with et_stencil;						use et_stencil;


with et_text;							use et_text;
with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.packages;	use et_device_placeholders.packages;

with et_silkscreen;						use et_silkscreen;
with et_silkscreen.packages;

with et_assy_doc;						use et_assy_doc;
with et_assy_doc.packages;

with et_symbols;	
with et_schematic;						use et_schematic;
with et_pcb;							use et_pcb;
with et_pcb_stack;						use et_pcb_stack;
with et_packages;						use et_packages;
with et_pcb_sides;						use et_pcb_sides;
with et_pcb_coordinates_2;				use et_pcb_coordinates_2;
with et_board_shapes_and_text;			use et_board_shapes_and_text;
with et_pcb_contour;					use et_pcb_contour;

with et_nets;

-- with et_project.modules;				use et_project.modules;
with et_generic_module;					use et_generic_module;
with et_object_status;

package et_device_query_board is

	use pac_polygons;
	use pac_devices_sch;
	use pac_devices_non_electric;

	use pac_geometry_2;


	function is_proposed (
		device_cursor	: in et_schematic.pac_devices_sch.cursor) -- IC45
		return boolean;

	function is_selected (
		device_cursor	: in et_schematic.pac_devices_sch.cursor) -- IC45
		return boolean;



	function is_proposed (
		device_cursor	: in pac_devices_non_electric.cursor) -- FD1
		return boolean;

	function is_selected (
		device_cursor	: in pac_devices_non_electric.cursor) -- FD1
		return boolean;



	-- Maps from a given cursor of a non-electrical device
	-- to a cursor to the package model:
	function get_package_model (
		device_cursor	: in pac_devices_non_electric.cursor)
		return pac_package_models.cursor;

	
	
	-- Returns the current position (x/y/rotation/face) of the 
	-- given electrical device:
	function get_position (
		device_cursor	: in et_schematic.pac_devices_sch.cursor) -- IC45
		return type_package_position;


	-- Returns the current position (x/y/rotation/face) of the 
	-- given non-electrical device:
	function get_position (
		device_cursor	: in pac_devices_non_electric.cursor) -- FD1
		return type_package_position;

	
	-- Returns the current face of the given electrical device:
	function get_face (
		device_cursor	: in et_schematic.pac_devices_sch.cursor) -- IC45
		return type_face; -- top/bottom


	-- Returns the current face of the given non-electrical device:
	function get_face (
		device_cursor	: in et_pcb.pac_devices_non_electric.cursor) -- FD1
		return type_face; -- top/bottom


	-- Returns the position of a terminal of the given device in the board.
	-- The device must be real (appearance SCH_PCB).
	function get_terminal_position (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in et_schematic.pac_devices_sch.cursor; -- IC45
		terminal_name	: in pac_terminal_name.bounded_string) -- H7, 14
		return type_terminal_position_fine;

	
	-- CS ?
	-- Same as above function but takes a terminal cursor instead of a terminal name
	--function get_terminal_position (
		--module_cursor	: in pac_generic_modules.cursor;
		--device_cursor	: in et_schematic.pac_devices_sch.cursor; -- IC45
		--terminal_cursor	: in pac_terminals.cursor) -- H7, 14
		--return type_terminal_position_fine;


	-- Returns ALL terminals of the given device.
	-- This query assumes the default assembly
	-- variant, means the device of interest exists in any case:
	function get_all_terminals (
		device_cursor	: in et_schematic.pac_devices_sch.cursor) -- IC45
		return pac_terminals.map;



	-- Returns the unconnected terminals of the given device
	-- in the given module. This query assumes the default assembly
	-- variant, means the device of interest exists in any case:
	function get_unconnected_terminals (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in et_schematic.pac_devices_sch.cursor) -- IC45
		return pac_terminals.map;

	

	-- This controlled type is used by the functon to_polygon below:
	type type_terminal_polygon (exists : boolean) is record
		case exists is
			when TRUE	=> 
				polygon		: pac_polygons.type_polygon;
				position	: type_terminal_position_fine;
				
			when FALSE	=> null;
		end case;
	end record;

	
	-- Returns the position of a terminal and its contour as a polygon.
	-- If the terminal does not affect the given layer category,
	-- then nothing happens here -> returns just a "false".
	-- See specification of type_terminal_polygon above.
	function to_polygon (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in pac_devices_sch.cursor;
		terminal_cursor	: in pac_terminals.cursor;
		layer_category	: in type_signal_layer_category;
		tolerance		: in type_distance_positive)
		return type_terminal_polygon;

	
	
-- CONDUCTORS
	
	-- Returns the conductor objects of the given electrical device
	-- (according to its flip status, position and rotation in the board) 
	-- Adresses only those objects affected by the given face:
	function get_conductor_objects (
		device_cursor	: in pac_devices_sch.cursor;
		layer_category	: in type_signal_layer_category)
		return type_conductor_objects;

	
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

	
	-- Returns the conductor objects of the given non-electrical device
	-- (according to its flip status, position and rotation in the board) 
	-- Adresses only those objects affected by the given face:
	function get_conductor_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		layer_category	: in type_signal_layer_category)
		return type_conductor_objects;


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

	
	
-- ROUTE RESTRICT:

	-- Returns the route restrict objects of the given non-electrical device
	-- (according to its flip status, position and rotation in the board) 
	-- Adresses only those objects affected by the given face:	
	function get_route_restrict_objects (
		device_cursor	: in pac_devices_sch.cursor;
		layer_category	: in type_signal_layer_category)
		return et_route_restrict.packages.type_one_side;

	
	-- Returns the outlines of route restrict objects of the electrical
	-- device (according to its position and rotation in the board) 
	-- as a list of polygons.
	-- NOTE regarding circles: The inside of circles is ignored. Only the outer
	--  edge of a circle is converted to a polygon.
	-- Adresses only those objects which are affected by
	-- the given layer category:
	function get_route_restrict_polygons (
		device_cursor	: in pac_devices_sch.cursor;
		layer_category	: in type_signal_layer_category)
		return pac_polygon_list.list;

	
	-- Returns the route restrict objects of the given electrical device
	-- (according to its flip status, position and rotation in the board) 
	-- Adresses only those objects affected by the given face:	
	function get_route_restrict_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		layer_category	: in type_signal_layer_category)
		return et_route_restrict.packages.type_one_side;

	
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


	
-- VIA RESTRICT:
	
	-- Returns the via restrict objects of the given electrical device
	-- (according to its flip status, position and rotation in the board) 
	-- Adresses only those objects affected by the given face:	
	function get_via_restrict_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		layer_category	: in type_signal_layer_category)
		return et_via_restrict.packages.type_one_side;

	
	-- Returns the via restrict objects of the given non-electrical device
	-- (according to its flip status, position and rotation in the board) 
	-- Adresses only those objects affected by the given face:
	function get_via_restrict_objects (
		device_cursor	: in pac_devices_sch.cursor;
		layer_category	: in type_signal_layer_category)
		return et_via_restrict.packages.type_one_side;


	
	
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


-- STENCIL:
	
	-- Returns the stencil objects of the given electrical device
	-- (according to its flip status, position and rotation in the board) 
	-- Adresses only those objects affected by the given face:
	function get_stencil_objects (
		device_cursor	: in pac_devices_sch.cursor;
		face			: in type_face)
		return type_stencil;


	-- Returns the stencil objects of the given non-electrical device
	-- (according to its flip status, position and rotation in the board) 
	-- Adresses only those objects affected by the given face:
	function get_stencil_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		face			: in type_face)
		return type_stencil;


-- STOPMASK:
	
	-- Returns the stopmask objects of the given electrical device
	-- (according to its flip status, position and rotation in the board) 
	-- Adresses only those objects affected by the given face:
	function get_stopmask_objects (
		device_cursor	: in pac_devices_sch.cursor;
		face			: in type_face)
		return type_stopmask;


	-- Returns the stopmask objects of the given non-electrical device
	-- (according to its flip status, position and rotation in the board) 
	-- Adresses only those objects affected by the given face:
	function get_stopmask_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		face			: in type_face)
		return type_stopmask;


-- PLACEHOLDERS:
	
	-- Maps from meaning of given placeholder to a text content:
	function to_placeholder_content (
		device_cursor	: in pac_devices_sch.cursor; -- electrical device
		placeholder		: in type_placeholder)
		return et_text.pac_text_content.bounded_string;

	-- Maps from meaning of given placeholder to a text content:
	function to_placeholder_content (
		device_cursor	: in pac_devices_non_electric.cursor; -- non-electrical device
		placeholder		: in type_placeholder)
		return et_text.pac_text_content.bounded_string;
	
	
-- SILKSCREEN:
	
	-- Returns the silkscreen objects of the given electrical device
	-- (according to its flip status, position and rotation in the board) 
	-- Adresses only those objects affected by the given face.
	-- Replaces text placeholders by regular texts in silkscreen.
	-- The text placeholders specified in the board overwrite
	-- the default placeholders (as specified in the package model).
	-- CS: In the future there could be an option to keep the
	-- properties of the default placeholders or to use the properties 
	-- as specified in the board.
	-- This behaviour would be similar to the "smash"-function implemented
	-- in other CAE systems:
	function get_silkscreen_objects (
		device_cursor	: in pac_devices_sch.cursor;
		face			: in type_face)
		return type_silkscreen;


	-- Returns the silkscreen objects of the given non-electrical device
	-- (according to its flip status, position and rotation in the board) 
	-- Adresses only those objects affected by the given face.
	-- Replaces text placeholders by regular texts in silkscreen.
	-- The text placeholders specified in the board overwrite
	-- the default placeholders (as specified in the package model).
	-- CS: In the future there could be an option to keep the
	-- properties of the default placeholders or to use the properties 
	-- as specified in the board.
	-- This behaviour would be similar to the "smash"-function implemented
	-- in other CAE systems:
	function get_silkscreen_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		face			: in type_face)
		return type_silkscreen;

	
-- ASSEMBLY DOCUMENTATION:
	
	-- Returns the assy_doc objects of the given electrical device
	-- (according to its flip status, position and rotation in the board) 
	-- Adresses only those objects affected by the given face.
	-- Replaces text placeholders by regular texts in assy_doc.
	-- The text placeholders specified in the board overwrite
	-- the default placeholders (as specified in the package model).
	-- CS: In the future there could be an option to keep the
	-- properties of the default placeholders or to use the properties 
	-- as specified in the board.
	-- This behaviour would be similar to the "smash"-function implemented
	-- in other CAE systems:
	function get_assy_doc_objects (
		device_cursor	: in pac_devices_sch.cursor;
		face			: in type_face)
		return type_assy_doc;


	-- Returns the assy_doc objects of the given non-electrical device
	-- (according to its flip status, position and rotation in the board) 
	-- Adresses only those objects affected by the given face.
	-- Replaces text placeholders by regular texts in assy_doc.
	-- The text placeholders specified in the board overwrite
	-- the default placeholders (as specified in the package model).
	-- CS: In the future there could be an option to keep the
	-- properties of the default placeholders or to use the properties 
	-- as specified in the board.
	-- This behaviour would be similar to the "smash"-function implemented
	-- in other CAE systems:
	function get_assy_doc_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		face			: in type_face)
		return type_assy_doc;



	

-- HOLES:

	-- Returns the outlines of holes of the electrical device
	-- (according to its position and rotation in the board).
	-- If the device is virtual, then the returned list is empty:
	function get_holes (
		device_cursor	: in pac_devices_sch.cursor)
		return pac_holes.list;
	

	-- Returns the outlines of holes of the electrical device
	-- (according to its position and rotation in the board) as
	-- a list of polygons.
	-- If the device is virtual, then the returned list is empty:
	function get_hole_polygons (
		device_cursor	: in pac_devices_sch.cursor)
		return pac_polygon_list.list;


	-- Returns the outlines of holes of the non-electrical device
	-- (according to its position and rotation in the board):
	function get_holes (
		device_cursor	: in pac_devices_non_electric.cursor)
		return pac_holes.list;

	
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
