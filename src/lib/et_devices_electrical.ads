------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         DEVICES ELECTRICAL                               --
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
-- This package is about basic properties and subprograms related
-- to so called "electrical devices". These devices have a representation
-- in both schematic and board domain.

with ada.strings.bounded;       		use ada.strings.bounded;
with ada.containers;           			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_ordered_maps;

with et_coordinates_2;
with et_assembly_variants;				use et_assembly_variants;
with et_assembly_variant_name;			use et_assembly_variant_name;

with et_device_placeholders.packages;

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
with et_pcb_stack;						use et_pcb_stack;
with et_pcb_sides;						use et_pcb_sides;
with et_pcb_coordinates_2;
with et_material;
with et_text;
with et_symbols;						use et_symbols;
with et_port_names;						use et_port_names;
with et_device_appearance;				use et_device_appearance;
with et_device_purpose;					use et_device_purpose;
with et_device_model_names;				use et_device_model_names;
with et_device_value;					use et_device_value;
with et_device_name;					use et_device_name;
with et_device_partcode;				use et_device_partcode;
with et_device_library;					use et_device_library;
with et_package_names;					use et_package_names;
with et_package_variant;				use et_package_variant;
with et_terminals;						use et_terminals;
with et_packages;						use et_packages;
with et_commit;
with et_object_status;					use et_object_status;
with et_unit_name;						use et_unit_name;
with et_units;							use et_units;


package et_devices_electrical is



	-- This is a device as it appears in the schematic.
	type type_device_sch (appearance : type_appearance_schematic) is record

		-- The link to the device model like ../libraries/devices/transistor/pnp.dev
		model	: pac_device_model_file.bounded_string;
		-- CS use a cursor to the model instead ?

		-- The units like PWR, A, B, ...
		-- Virtual devices have only one unit (like the GND symbol).
		-- Real devices like a single resistor have one unit.
		-- Real devices like FPGAs have many units (like PWR1, PWR2, GPIO1, GPIO2, ...):
		units	: pac_units.map;
		
		case appearance is
			-- If a device appears in both schematic and layout it has got:
			when APPEARANCE_PCB =>
				value		: pac_device_value.bounded_string; -- 470R
				
				partcode	: pac_device_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
				-- For virtual packages (test points, edge connectors, ...)
				-- usually no partcode is required.

				-- The purpose indicates what the device is doing.
				-- It is usually required for devices that require interaction
				-- with the user of a PCBA:
				purpose		: pac_device_purpose.bounded_string; -- brightness_control

				-- The package variant:
				variant		: pac_package_variant_name.bounded_string; -- D, N

				-- This is layout related. In the layout, the package has a position
				-- and placeholders for name, value and purpose.
				-- The assembly side of a package is by default TOP.
				-- As a result of a flip operation, position.face changes from top to bottom
				-- or vice versa.
				-- Flipping a device to top or bottom means it will be drawn
				-- mirrored along its Y-axis.
				position			: et_pcb_coordinates_2.type_package_position; -- incl. rotation and face
				text_placeholders	: et_device_placeholders.packages.type_text_placeholders;

				-- CS flags that signal whether partcode, purpose, bom are displayed or not.

				-- The status of the package:
				status : type_object_status;
				
			when APPEARANCE_VIRTUAL => null;

		end case;
	end record;




	-- The devices of a module are collected in a map.
	-- CS: This must be a hashed map:
 	package pac_devices_sch is new indefinite_ordered_maps (
		key_type		=> type_device_name, -- something like "IC43"
 		element_type	=> type_device_sch);

	use pac_devices_sch;
	

	
	procedure device_name_in_use (
		name : in type_device_name); -- IC1, R1, ...
	

	
	
	-- Returns the current position (x/y/rotation/face) of the 
	-- given electrical device:
	function get_position (
		device_cursor	: in pac_devices_sch.cursor) -- IC45
		return et_pcb_coordinates_2.type_package_position;


	-- Returns the current position (x/y) of the 
	-- given electrical device:
	function get_position (
		device_cursor	: in pac_devices_sch.cursor) -- IC45
		return et_pcb_coordinates_2.pac_geometry_2.type_vector_model;

	
	
	-- Returns the current face of the given electrical device:
	function get_face (
		device_cursor	: in pac_devices_sch.cursor) -- IC45
		return type_face; -- top/bottom



	-- Returns ALL terminals of the given device.
	-- This query assumes the default assembly
	-- variant, means the device of interest exists in any case:
	function get_all_terminals (
		device_cursor	: in pac_devices_sch.cursor) -- IC45
		return pac_terminals.map;



	
-- CONDUCTOR OBJECTS:

	
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
		return et_pcb_coordinates_2.pac_polygons.pac_polygon_list.list;




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
		return et_pcb_coordinates_2.pac_polygons.pac_polygon_list.list;




-- VIA RESTRICT:
	
	
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




-- STENCIL:
	
	-- Returns the stencil objects of the given electrical device
	-- (according to its flip status, position and rotation in the board) 
	-- Adresses only those objects affected by the given face:
	function get_stencil_objects (
		device_cursor	: in pac_devices_sch.cursor;
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







-- PLACEHOLDERS:
	
	-- Maps from meaning of given placeholder to a text content:
	function to_placeholder_content (
		device_cursor	: in pac_devices_sch.cursor; -- electrical device
		placeholder		: in et_device_placeholders.packages.type_placeholder)
		return et_text.pac_text_content.bounded_string;

	
	
	
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
		return et_pcb_coordinates_2.pac_polygons.pac_polygon_list.list;

	




	
	
-- DEVICE STATUS OPERATIONS:
	
	-- NOTE: Operations regarding the status
	-- apply to the package of the device (in the board domain).
	-- Status opertions for individual units (in the schematic)
	-- are specified in the package et_units.
	-- Regarding set and clear operations: If the device
	-- is not real (APPEARANCE_PCB) then the operation has no effect.
	-- Regarding query operations like is_selected or is_moving: If
	-- the device is not real (APPEARANCE_PCB) then the return is
	-- always false.
	
	procedure set_selected (
		device : in out type_device_sch);
	

	procedure clear_selected (
		device : in out type_device_sch);
	

	function is_selected (
		device : in type_device_sch)
		return boolean;
	

	
	procedure set_proposed (
		device : in out type_device_sch);
	

	procedure clear_proposed (
		device : in out type_device_sch);

	
	function is_proposed (
		device : in type_device_sch)
		return boolean;



	
	procedure set_moving (
		device : in out type_device_sch);
	

	procedure clear_moving (
		device : in out type_device_sch);

	
	function is_moving (
		device : in type_device_sch)
		return boolean;


	
	
	procedure modify_status (
		device		: in out type_device_sch;
		operation	: in type_status_operation);
	

	
	procedure reset_status (
		device : in out type_device_sch);




	-- Returns true if given device is real (means if it has a physical 
	-- counterpart in the PCB layout). For a resistor it returns true.
	-- For a GND symbol it returns false:
	function is_real (
		device : in pac_devices_sch.cursor) 
		return boolean;
	

	function is_proposed (
		device : in pac_devices_sch.cursor)
		return boolean;
	

	function is_selected (
		device : in pac_devices_sch.cursor)
		return boolean;

	
	function is_moving (
		device : in pac_devices_sch.cursor)
		return boolean;

	
	
	-- Iterates the devices. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		devices	: in pac_devices_sch.map;
		process	: not null access procedure (position : in pac_devices_sch.cursor);
		proceed	: not null access boolean);

	

	
-- DEVICE QUERY OPERATIONS:
	
	
	-- Returns the name of the device indicated by
	-- the given cursor:
	function to_string (
		device_cursor : in pac_devices_sch.cursor)
		return string;
		



	-- Maps from schematic device to device model (in library):
	function get_device_model (
		device : in pac_devices_sch.cursor)
		return pac_devices_lib.cursor;


	-- Returns the name of the package model of the given device
	-- according to the current package variant of the device.
	-- The given device must be real. Otherwise constraint error arises here.	
	function get_package_model ( -- CS rename to get_package_model_name
		device : in pac_devices_sch.cursor)
		return pac_package_model_file_name.bounded_string; -- libraries/packages/smd/SOT23.pac
	

	-- Returns the cursor to the package model of the given device
	-- according to the current package variant of the device.
	-- The given device must be real. Otherwise constraint error arises here.	
	function get_package_model (
		device : in pac_devices_sch.cursor)
		return pac_package_models.cursor;


	-- Returns true if the given device has a real package.
	-- The given device must have appearance SCH_PCB. 
	-- Otherwise a constraint error is raised.
	function has_real_package (
		device : in pac_devices_sch.cursor)
		return boolean;

	
	-- Returns the value of the given device.
	-- The device must be real. Otherwise constraint error is raised.
	function get_value (
		device : in pac_devices_sch.cursor)
		return pac_device_value.bounded_string;

	
	-- Returns the purpose of the given device.
	-- The device must be real. Otherwise constraint error is raised.
	function get_purpose (
		device : in pac_devices_sch.cursor)
		return pac_device_purpose.bounded_string;


	-- Returns the partcode of the given device.
	-- The device must be real. Otherwise constraint error is raised.
	function get_partcode (
		device : in pac_devices_sch.cursor)
		return pac_device_partcode.bounded_string;



	-- Returns the package variant of the given device.
	-- The device must be real. Otherwise constraint error is raised.
	function get_package_variant (
		device : in pac_devices_sch.cursor)
		return pac_package_variant_name.bounded_string;



	-- Maps from the given terminal to the linked port and unit.
	-- The given device must be real. Otherwise a constraint error
	-- will be raised:
	function get_port (
		device		: in pac_devices_sch.cursor;
		terminal	: in et_terminals.pac_terminal_name.bounded_string) -- H7, 1, 14
		return type_get_port_result;


	-- Maps from the given device cursor, unit and port name 
	-- to a cursor of the linked terminal.
	-- A port is always linked with a terminal.
	-- The given device must be real. Otherwise a constraint error will be raised:
	function get_terminal (
		device	: in pac_devices_sch.cursor;
		unit	: in pac_unit_name.bounded_string;
		port	: in pac_port_name.bounded_string)
		return et_terminals.pac_terminals.cursor;

	

	
	
	-- COMMITS OF ELECTRICAL DEVICES (required for undo/redo operations via the GUI):
	use et_commit;
	
	package pac_device_commit is new pac_commit (pac_devices_sch.map);
	use pac_device_commit;
	
	package pac_device_commits is new doubly_linked_lists (
		element_type	=> pac_device_commit.type_commit);

	type type_devices_undo_redo_stack is record
		dos		: pac_device_commits.list;
		redos	: pac_device_commits.list;
	end record;

	
	

	
		
end et_devices_electrical;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
