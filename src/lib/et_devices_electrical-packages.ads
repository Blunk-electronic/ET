------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   DEVICES ELECTRICAL / PACKAGES                          --
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
-- To Do:
-- 1. device accessories
--
--
--
-- DESCRIPTION:
--
-- The subprograms in this package address the package
-- of a device. So the targeted device must be real.
-- Otherwise exceptions wlll be raised.
--

with et_board_geometry;					use et_board_geometry;
with et_board_coordinates;				use et_board_coordinates;

with et_coordinates_formatting;			use et_coordinates_formatting;

with et_device_placeholders;
with et_device_placeholders.packages;	use et_device_placeholders.packages;


with et_route_restrict.packages;
with et_via_restrict.packages;

with et_board_holes;					use et_board_holes;

with et_silkscreen;						use et_silkscreen;
with et_assy_doc;						use et_assy_doc;
with et_keepout;						use et_keepout;
with et_stopmask;						use et_stopmask;
with et_stencil;						use et_stencil;
with et_pcb_signal_layers;				use et_pcb_signal_layers;
with et_pcb_sides;						use et_pcb_sides;

with et_port_names;						use et_port_names;
with et_unit_name;						use et_unit_name;
with et_package_variant_name;			use et_package_variant_name;
with et_package_variant;				use et_package_variant;
with et_terminals;						use et_terminals;
with et_conductors_floating_package;	use et_conductors_floating_package;
with et_logging;						use et_logging;


package et_devices_electrical.packages is

	use pac_geometry_2;


-- PACKAGE VARIANT:


	-- Returns the package variant of the given device.
	-- The device must be real. Otherwise constraint error is raised.
	function get_package_variant (
		device : in pac_devices_electrical.cursor)
		return type_package_variant_name;


	-- Returns a list of available package variants:
	function get_available_package_variants (
		device : in pac_devices_electrical.cursor)
		return pac_package_variants.map;






-- POSITION:

	-- Writes the position of the package in the log file.
	-- If device is virtual, nothing happens:
	procedure log_package_position (
		device_cursor	: in pac_devices_electrical.cursor;
		log_threshold	: in type_log_level);



	-- Returns the current position (x/y/rotation/face) of the
	-- given electrical device:
	function get_position (
		device_cursor	: in pac_devices_electrical.cursor) -- IC45
		return type_board_object;



-- FACE:

	-- Returns the current position (x/y) of the
	-- given electrical device:
	function get_place (
		device_cursor	: in pac_devices_electrical.cursor) -- IC45
		return type_vector_model;



	-- Returns the current face of the given electrical device:
	function get_face (
		device_cursor	: in pac_devices_electrical.cursor) -- IC45
		return type_face; -- top/bottom












-- CONDUCTOR OBJECTS:


	-- Returns the conductor objects of the given electrical device
	-- (according to its flip status, position and rotation in the board)
	-- Adresses only those objects affected by the given face:
	function get_conductor_objects (
		device_cursor	: in pac_devices_electrical.cursor;
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
		device_cursor	: in pac_devices_electrical.cursor;
		layer_category	: in type_signal_layer_category) -- outer top, inner, outer bottom
		return et_board_geometry.pac_polygons.pac_polygon_list.list;




-- ROUTE RESTRICT:

	-- Returns the route restrict objects of the given non-electrical device
	-- (according to its flip status, position and rotation in the board)
	-- Adresses only those objects affected by the given face:
	function get_route_restrict_objects (
		device_cursor	: in pac_devices_electrical.cursor;
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
		device_cursor	: in pac_devices_electrical.cursor;
		layer_category	: in type_signal_layer_category)
		return et_board_geometry.pac_polygons.pac_polygon_list.list;




-- VIA RESTRICT:


	-- Returns the via restrict objects of the given non-electrical device
	-- (according to its flip status, position and rotation in the board)
	-- Adresses only those objects affected by the given face:
	function get_via_restrict_objects (
		device_cursor	: in pac_devices_electrical.cursor;
		layer_category	: in type_signal_layer_category)
		return et_via_restrict.packages.type_one_side;





-- KEEPOUT:

	-- Returns the keepout objects of the given device
	-- (according to its flip status, position and rotation in the board)
	-- Adresses only those objects affected by the given face:
	function get_keepout_objects (
		device_cursor	: in pac_devices_electrical.cursor;
		face			: in type_face)
		return type_keepout;




-- STENCIL:

	-- Returns the stencil objects of the given electrical device
	-- (according to its flip status, position and rotation in the board)
	-- Adresses only those objects affected by the given face:
	function get_stencil_objects (
		device_cursor	: in pac_devices_electrical.cursor;
		face			: in type_face)
		return type_stencil;



-- STOPMASK:

	-- Returns the stopmask objects of the given electrical device
	-- (according to its flip status, position and rotation in the board)
	-- Adresses only those objects affected by the given face:
	function get_stopmask_objects (
		device_cursor	: in pac_devices_electrical.cursor;
		face			: in type_face)
		return type_stopmask;


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
	-- This behavior would be similar to the "smash"-function implemented
	-- in other CAE systems:
	function get_silkscreen_objects (
		device_cursor	: in pac_devices_electrical.cursor;
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
	-- This behavior would be similar to the "smash"-function implemented
	-- in other CAE systems:
	function get_assy_doc_objects (
		device_cursor	: in pac_devices_electrical.cursor;
		face			: in type_face)
		return type_assy_doc;





-- HOLES:

	-- Returns the outlines of holes of the electrical device
	-- (according to its position and rotation in the board).
	-- If the device is virtual, then the returned list is empty:
	function get_holes (
		device_cursor	: in pac_devices_electrical.cursor)
		return pac_holes.list;


	-- Returns the outlines of holes of the electrical device
	-- (according to its position and rotation in the board) as
	-- a list of polygons.
	-- If the device is virtual, then the returned list is empty:
	function get_hole_polygons (
		device_cursor	: in pac_devices_electrical.cursor)
		return et_board_geometry.pac_polygons.pac_polygon_list.list;





-- TERMINALS:


	-- Maps from the given device cursor, unit and port name
	-- to a cursor of the linked terminal.
	-- A port is always linked with a terminal.
	-- The given device must be real. Otherwise a constraint error will be raised:
	function get_terminal (
		device	: in pac_devices_electrical.cursor;
		unit	: in type_unit_name;
		port	: in type_port_name)
		return et_terminals.pac_terminals.cursor;


	-- CS function get_terminal (
		-- device	: in pac_devices_electrical.cursor;
		-- unit	: in pac_units.cursor;
		-- port	: in pac_ports.cursor)
		-- return et_terminals.pac_terminals.cursor;


	-- Returns ALL terminals of the given device.
	-- This query assumes the default assembly
	-- variant, means the device of interest exists in any case.
	-- If the given device is virtual, then an empty list is returned:
	function get_all_terminals (
		device_cursor	: in pac_devices_electrical.cursor) -- IC45
		return pac_terminals.map;



end et_devices_electrical.packages;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
