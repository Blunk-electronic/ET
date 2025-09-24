------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         DEVICES NON-ELECTRICAL                           --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   to do:
--
-- 
-- DESCRIPTION:
-- 
-- This package is about the type, basic properties and subprograms related
-- to so called "non-electrical" devices as they are modelled in the board. 
-- These devices have a representation in the board domain only.


with ada.containers; 					use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;

with et_pcb_sides;						use et_pcb_sides;
with et_board_geometry;					use et_board_geometry;
with et_board_coordinates;				use et_board_coordinates;
with et_board_text;						use et_board_text;
with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.packages;	use et_device_placeholders.packages;
with et_package_model;					use et_package_model;
with et_package_library;				use et_package_library;
with et_package_names;					use et_package_names;
with et_device_name;					use et_device_name;
with et_pcb_stack;						use et_pcb_stack;
with et_pcb_contour;					use et_pcb_contour;
with et_commit;
with et_object_status;					use et_object_status;
with et_mirroring;						use et_mirroring;
with et_route_restrict.packages;
with et_via_restrict.packages;
with et_keepout;						use et_keepout;
with et_stencil;						use et_stencil;
with et_stopmask;						use et_stopmask;
with et_silkscreen;						use et_silkscreen;
with et_assy_doc;						use et_assy_doc;
with et_text;


package et_devices_non_electrical is
	
	use pac_geometry_2;
	use pac_polygons;


	procedure device_name_in_use (
		name : in type_device_name); -- FD1, MH1, ...

	

	-- In this world, if a package is flipped, then it is
	-- mirrored along the Y-axis.
	-- This function maps from flip status to mirror along y-axis.
	-- If flipped is false, then the return is MIRRROR_NO.
	-- If flipped is true, then the return is MIRROR_ALONG_Y_AXIS:
	function to_mirror_along_y_axis ( -- CS remove ?
		flipped : in type_flipped)
		return type_mirror;


	

	-- Devices which do not have a counterpart in the schematic 
	-- (like fiducials, mounting holes, ...). They can have
	-- terminals. But the terminals are not connected with any net.
	-- They have names like H1 (hole) or FD (fiducial).
	-- This is NOT about accessories of the module !
	-- These devices do NOT appear in the BOM !
	-- We collect them in an indefinite ordered map.
	-- CS: Move into a separate package !
	
	type type_device_non_electric is record
		position			: et_board_coordinates.type_package_position; -- incl. rotation and face
		text_placeholders	: type_text_placeholders;
		package_model		: pac_package_model_file_name.bounded_string; -- ../lbr/packages/fiducial.pac
		-- CS cursor to package model instead ?

		-- CS ?
		--value		: pac_device_value.bounded_string; -- 470R
		--partcode	: pac_device_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
		--purpose		: pac_device_purpose.bounded_string; -- brightness_control

		status : type_object_status;
	end record;
	

	
	procedure set_proposed (
		device : in out type_device_non_electric);


	procedure clear_proposed (
		device : in out type_device_non_electric);


	function is_proposed (
		device : in type_device_non_electric)
		return boolean;


		

	
	procedure set_selected (
		device : in out type_device_non_electric);

	
	procedure clear_selected (
		device : in out type_device_non_electric);

	
	function is_selected (
		device : in type_device_non_electric)
		return boolean;


	

	procedure set_moving (
		device : in out type_device_non_electric);


	procedure clear_moving (
		device : in out type_device_non_electric);

	
	function is_moving (
		device : in type_device_non_electric)
		return boolean;


	

	
	procedure set_locked (
		device : in out type_device_non_electric);


	procedure clear_locked (
		device : in out type_device_non_electric);

	
	function is_locked (
		device : in type_device_non_electric)
		return boolean;



	procedure modify_status (
		device		: in out type_device_non_electric;
		operation	: in type_status_operation);



	procedure reset_status (
	   device : in out type_device_non_electric);

	
	
	-- CS: this should be a hashed map:
	package pac_devices_non_electric is new ordered_maps ( -- CS rename to pac_devices_non_electrical
		key_type		=> type_device_name, -- H1, FD2, ...
		element_type	=> type_device_non_electric);

	use pac_devices_non_electric;
	

	
	-- Iterates the non-electric devices. 
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		devices	: in pac_devices_non_electric.map;
		process	: not null access procedure (position : in pac_devices_non_electric.cursor);
		proceed	: not null access boolean);

	

	function get_device_name (
		device : in pac_devices_non_electric.cursor)
		return type_device_name;

	
	
	-- Returns the name of the non-electical device
	-- as string:
	function get_device_name (
		device : in pac_devices_non_electric.cursor)
		return string;
	
		
	function is_proposed (
		device : in pac_devices_non_electric.cursor)
		return boolean;
	

	function is_selected (
		device : in pac_devices_non_electric.cursor)
		return boolean;


	function is_moving (
		device : in pac_devices_non_electric.cursor)
		return boolean;
	

	function is_locked (
		device : in pac_devices_non_electric.cursor)
		return boolean;





	-- Maps from a given cursor of a non-electrical device
	-- to a cursor to the package model:
	function get_package_model (
		device_cursor	: in pac_devices_non_electric.cursor)
		return pac_package_models.cursor;

	
	
	-- Returns the current position (x/y/rotation/face) of the 
	-- given non-electrical device:
	function get_position (
		device_cursor	: in pac_devices_non_electric.cursor) -- FD1
		return type_package_position;


	
	-- Returns the current position (x/y) of the 
	-- given non-electrical device:
	function get_position (
		device_cursor	: in pac_devices_non_electric.cursor) -- FD1
		return type_vector_model;



	-- Returns the current face of the given non-electrical device:
	function get_face (
		device_cursor	: in pac_devices_non_electric.cursor) -- FD1
		return type_face; -- top/bottom

	



	
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

	


	-- Returns the via restrict objects of the given electrical device
	-- (according to its flip status, position and rotation in the board) 
	-- Adresses only those objects affected by the given face:	
	function get_via_restrict_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		layer_category	: in type_signal_layer_category)
		return et_via_restrict.packages.type_one_side;

	


	-- Returns the keepout objects of the given device
	-- (according to its flip status, position and rotation in the board) 
	-- Adresses only those objects affected by the given face:
	function get_keepout_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		face			: in type_face)
		return type_keepout;

	


	-- Returns the stencil objects of the given non-electrical device
	-- (according to its flip status, position and rotation in the board) 
	-- Adresses only those objects affected by the given face:
	function get_stencil_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		face			: in type_face)
		return type_stencil;



	-- Returns the stopmask objects of the given non-electrical device
	-- (according to its flip status, position and rotation in the board) 
	-- Adresses only those objects affected by the given face:
	function get_stopmask_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		face			: in type_face)
		return type_stopmask;



	-- Maps from meaning of given placeholder to a text content:
	function to_placeholder_content (
		device_cursor	: in pac_devices_non_electric.cursor; -- non-electrical device
		placeholder		: in type_placeholder)
		return et_text.pac_text_content.bounded_string;

	


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


	
	
	
	
	-- COMMITS OF NON-ELECTRICAL DEVICES (required for undo/redo operations via the GUI):
	use et_commit;
	
	package pac_non_electrical_device_commit is new pac_commit (pac_devices_non_electric.map);
	use pac_non_electrical_device_commit;
	
	package pac_non_electrical_device_commits is new doubly_linked_lists (
		element_type	=> pac_non_electrical_device_commit.type_commit);

	type type_non_electrical_devices_undo_redo_stack is record
		dos		: pac_non_electrical_device_commits.list;
		redos	: pac_non_electrical_device_commits.list;
	end record;
	





	
end et_devices_non_electrical;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
