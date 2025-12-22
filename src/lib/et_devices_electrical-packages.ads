------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   DEVICES ELECTRICAL / PACKAGES                          --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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
--		1. device accessories
-- 
-- DESCRIPTION:
-- 
-- This package is about the type, basic properties and subprograms related
-- to the packages of real devices.

with et_board_layer_category;			use et_board_layer_category;
with et_board_geometry;					use et_board_geometry;
with et_board_coordinates;				use et_board_coordinates;

with et_coordinates_formatting;			use et_coordinates_formatting;

with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.packages;	use et_device_placeholders.packages;

with et_silkscreen;						use et_silkscreen;
with et_silkscreen.packages;

with et_assy_doc;						use et_assy_doc;
with et_assy_doc.packages;

with et_route_restrict.packages;
with et_via_restrict.packages;

with et_pcb_contour;					use et_pcb_contour;

with et_board_text;
with et_keepout;						use et_keepout;
with et_stopmask;						use et_stopmask;
with et_stopmask.packages;
with et_stencil;						use et_stencil;
with et_pcb_signal_layers;				use et_pcb_signal_layers;
with et_pcb_sides;						use et_pcb_sides;

with et_material;
with et_text;
with et_device_purpose;					use et_device_purpose;
with et_device_value;					use et_device_value;
with et_port_names;						use et_port_names;
with et_unit_name;						use et_unit_name;
with et_device_partcode;				use et_device_partcode;
with et_package_name;					use et_package_name;
with et_package_model_name;				use et_package_model_name;
with et_package_variant;				use et_package_variant;
with et_terminals;						use et_terminals;
with et_package_model;					use et_package_model;
with et_package_library;				use et_package_library;
with et_object_status;					use et_object_status;
with et_logging;						use et_logging;


package et_devices_electrical.packages is

	use pac_geometry_2;


	

-- VALUE:
	
	procedure set_value (
		device	: in out type_device_electrical;
		value	: in pac_device_value.bounded_string);

	
	function get_value (
		device	: in type_device_electrical)
		return pac_device_value.bounded_string;

	
	function get_value (
		device	: in type_device_electrical)
		return string;


	-- Returns the value of the given device.
	-- The device must be real. Otherwise constraint error is raised.
	function get_value (
		device : in pac_devices_electrical.cursor)
		return pac_device_value.bounded_string;


	function has_value (
		device	: in type_device_electrical)
		return boolean;

	
	

-- PARTCODE:

	procedure set_partcode (
		device		: in out type_device_electrical;
		partcode	: in pac_device_partcode.bounded_string);

	
	function get_partcode (
		device	: in type_device_electrical)
		return pac_device_partcode.bounded_string;

	
	function get_partcode (
		device	: in type_device_electrical)
		return string;


	-- Returns the partcode of the given device.
	-- The device must be real. Otherwise constraint error is raised.
	function get_partcode (
		device : in pac_devices_electrical.cursor)
		return pac_device_partcode.bounded_string;


	function has_partcode (
		device	: in type_device_electrical)
		return boolean;

	


-- PURPOSE:
	
	procedure set_purpose (
		device	: in out type_device_electrical;
		purpose	: in pac_device_purpose.bounded_string);

	
	function get_purpose (
		device	: in type_device_electrical)
		return pac_device_purpose.bounded_string;

	
	function get_purpose (
		device	: in type_device_electrical)
		return string;


	-- Returns the purpose of the given device.
	-- The device must be real. Otherwise constraint error is raised.
	function get_purpose (
		device : in pac_devices_electrical.cursor)
		return pac_device_purpose.bounded_string;
	

	function has_purpose (
		device	: in type_device_electrical)
		return boolean;
	

	
	
-- PACKAGE VARIANTS:
	
	-- Returns the name of the package variant of the device.
	-- The device must be a real device.
	-- Otherwise an exception will be raised:
	function get_package_variant (
		device : in type_device_electrical)
		return pac_package_variant_name.bounded_string;


	-- Returns a list of available package variants:
	function get_available_package_variants (
		device : in type_device_electrical)
		return pac_package_variants.map;



	-- Returns the package variant of the given device.
	-- The device must be real. Otherwise constraint error is raised.
	function get_package_variant (
		device : in pac_devices_electrical.cursor)
		return pac_package_variant_name.bounded_string;


	-- Returns a list of available package variants:
	function get_available_package_variants (
		device : in pac_devices_electrical.cursor)
		return pac_package_variants.map;



	

-- POSITION:

	-- IMPORTANT: For the following subprograms applies:
	-- These procedures and functions address the package
	-- of a device.
	-- The targeted device must be real. Otherwise an exception is raised:

	-- Sets the position of the package in the board drawing.
	procedure set_position (
		device		: in out type_device_electrical;
		position	: in type_package_position);


	-- Returns the position of the package in the board drawing.
	function get_position (
		device : in type_device_electrical)
		return type_package_position;
							   

	-- Returns the position of the package in the board drawing.
	function get_position (
		device	: in type_device_electrical;
		format	: in type_output_format := FORMAT_1)
		return string;


	function get_rotation (
		device	: in out type_device_electrical)
		return type_rotation_model;

	
	procedure set_rotation (
		device		: in out type_device_electrical;
		rotation	: in type_rotation_model);


	procedure set_rotation_relative (
		device		: in out type_device_electrical;
		rotation	: in type_rotation_model);

	
	procedure set_face (
		device	: in out type_device_electrical;
		face	: in type_face);				   


	procedure toggle_face (
		device	: in out type_device_electrical);
	

	function get_face (
		device	: in type_device_electrical)
		return type_face;

	
	function get_face (
		device	: in type_device_electrical)
		return string;

	
	procedure set_place (
		device	: in out type_device_electrical;
		place	: in type_vector_model);


	-- Moves the device by the given offset:
	procedure set_place_relative (
		device	: in out type_device_electrical;
		offset	: in type_vector_model);					

	
	function get_place (
		device	: in type_device_electrical)
		return type_vector_model;
	

	function get_place (
		device	: in type_device_electrical;
		format	: in type_output_format := FORMAT_1)
		return string;





	-- Writes the position of the package in the log file. 
	-- If device is virtual, nothing happens:
	procedure log_package_position (
		device_cursor	: in pac_devices_electrical.cursor;
		log_threshold	: in type_log_level);


	

	
	-- IMPORTANT: The following subprograms address the
	-- package of a device. The package must be real. Otherwise
	-- an exception will be raised.
	
	-- Returns the current position (x/y/rotation/face) of the 
	-- given electrical device:
	function get_position (
		device_cursor	: in pac_devices_electrical.cursor) -- IC45
		return et_board_coordinates.type_package_position;


	-- Returns the current position (x/y) of the 
	-- given electrical device:
	function get_place (
		device_cursor	: in pac_devices_electrical.cursor) -- IC45
		return et_board_geometry.pac_geometry_2.type_vector_model;

	
	
	-- Returns the current face of the given electrical device:
	function get_face (
		device_cursor	: in pac_devices_electrical.cursor) -- IC45
		return type_face; -- top/bottom



	

-- PLACEHOLDERS:

	
	-- Resets the positions of placeholders back to the
	-- defaults as specified in the package model:
	procedure reset_placeholder_positions (
		device		: in out type_device_electrical);


	
	-- Moves the placeholder given by meaning, layer, face and index.
	-- NOTE: Index identifies the targeted placeholder in connection
	--       with its meaning. For example, if meaning is "value" and index is 3
	--       then the 3rd value placeholder is adressed.
	--
	-- If no matching placeholder has been found, then nothing happens.
	-- CS; An error flag output by this procedure could be useful.
	--
	-- If coordinates is absolute, then the affected placeholder
	-- is moved to the given point and its anchor mode set accordingly.
	-- If coordinates is relative, then the affected placeholder
	-- is moved by the x/y-distance given by point and its anchor mode
	-- set accordingly:
	procedure move_placeholder (
		device		: in out type_device_electrical;
		meaning		: in type_placeholder_meaning;					 
		layer		: in type_placeholder_layer; -- silkscreen, assy doc
		face		: in type_face;
		index		: in type_placeholder_index; -- 1, 2, 3, ...
		coordinates	: in type_coordinates; -- relative/absolute
		point		: in type_vector_model); -- x/y
		-- CS rename to destination_offset rework documentation above
	

	
	-- Rotates the placeholder given by meaning, layer, face and index.
	-- NOTE: Index identifies the targeted placeholder in connection
	--       with its meaning. For example, if meaning is "value" and index is 3
	--       then the 3rd value placeholder is adressed.
	--
	-- If no matching placeholder has been found, then nothing happens.
	-- CS; An error flag output by this procedure could be useful.
	--
	-- If coordinates is absolute, then the placeholder
	-- is rotated to the given rotation.
	-- If coordinates is relative, then the placeholder
	-- is rotated by the given rotation:
	procedure rotate_placeholder (
		device		: in out type_device_electrical;
		meaning		: in type_placeholder_meaning;					 
		layer		: in type_placeholder_layer; -- silkscreen, assy doc
		face		: in type_face;
		index		: in type_placeholder_index; -- 1, 2, 3, ...
		coordinates	: in type_coordinates;
		rotation	: in type_rotation_model);

	




	
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
	-- This behaviour would be similar to the "smash"-function implemented
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
	-- This behaviour would be similar to the "smash"-function implemented
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
		unit	: in pac_unit_name.bounded_string;
		port	: in pac_port_name.bounded_string)
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
