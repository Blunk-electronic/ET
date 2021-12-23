------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        CANVAS SCHEMATIC UNITS                            --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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

with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;

with gtk.window;					use gtk.window;

with et_coordinates;				use et_coordinates;
use et_coordinates.pac_geometry_sch;

with et_geometry;					use et_geometry;
with et_general;					use et_general;
with et_canvas_general;				use et_canvas_general;
with et_project.modules;			use et_project.modules;
with et_symbols;					use et_symbols;
with et_devices;					use et_devices;
with et_nets;						use et_nets;
with et_schematic;					
with et_schematic_ops;				use et_schematic_ops;
with et_schematic_ops.units;		use et_schematic_ops.units;
with et_string_processing;			use et_string_processing;
with et_logging;					use et_logging;

with et_canvas_schematic_nets;


package et_canvas_schematic_units is

	use et_symbols.pac_geometry_2;
	use et_project.modules.pac_generic_modules;

	-- Whenever a unit (or the whole device with all its units)
	-- is selected via the GUI, we store cursors of its
	-- parent device and the unit itself via this type.
	-- This type is to be used for a device and a unit that
	-- is already placed in the schematic. So we use it
	-- for example for moving, dragging or rotating of a unit.
	type type_selected_unit is record
		device	: et_schematic.pac_devices_sch.cursor;

		-- If the cursor to the actual unit is no_element then
		-- the whole device is regarded as selected:
		unit	: et_schematic.pac_units.cursor;
	end record;

	package pac_proposed_units is new doubly_linked_lists (type_selected_unit);
	use pac_proposed_units;

	-- These variables are used by the GUI when the operator selects a unit:
	proposed_units	: pac_proposed_units.list;
	selected_unit	: pac_proposed_units.cursor;

	-- Clears the list proposed_units.
	-- Resets selected_unit to no_element.
	procedure clear_proposed_units;
	
	-- Collects all units in the vicinity of the given point:
	function collect_units (
		module			: in pac_generic_modules.cursor;
		place			: in et_coordinates.type_position; -- sheet/x/y
		catch_zone		: in type_catch_zone; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_proposed_units.list;

	-- Advances cursor selected_unit to next unit in list selected_units.
	procedure clarify_unit;


-- DELETE UNIT

	-- to be output in the status bar:
	status_delete : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to delete unit." 
		& status_hint_for_abort;
	
	-- Deletes a unit in the vicinity of given point.
	-- If more than one unit near point found, then it sets the
	-- cursor selected_unit to the first unit and requests
	-- for clarification.
	-- In case the last unit of a device has been deleted, then the device is 
	-- deleted entirely from the module.
	procedure delete_unit (point : in type_point);

	-- Deletes the unit being pointed at by cursor selected_unit.
	-- Call this procedure after a clarification.
	procedure delete_selected_unit;

	type type_unit_being_deleted is record
		device	: type_device_name := (others => <>); -- IC45
		unit	: pac_unit_name.bounded_string; -- A
	end record;

	unit_delete : type_unit_being_deleted;


	

-- MOVE/DRAG/ROTATE UNIT

	-- Global information for the GUI when a unit is 
	-- being moved, dragged or rotated:
	type type_unit_being_moved is record
		being_moved			: boolean := false;
		tool				: type_tool := MOUSE;

		-- In case the unit is being dragged, then we backup 
		-- here the original position.
		-- Procedure draw_nets requires that in order
		-- to calculate the displacement of attached net segments:
		original_position	: type_point := origin;
		
		device				: type_device_name := (others => <>); -- IC45
		unit				: pac_unit_name.bounded_string; -- A

		-- In case a unit is being moved from one sheet to another.
		-- This flag notifies the GUI that the unit is to be
		-- drawn on the current visible sheet. This way the
		-- original sheet number is ignored. See et_canvas_schematic.draw_units.
		sheet_changes		: boolean := false;
	end record;

	unit_move : type_unit_being_moved;

	
	-- to be output in the status bar:
	status_move : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to move unit." 
		& status_hint_for_abort;

	-- to be output in the status bar:
	status_drag : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to drag unit." 
		& status_hint_for_abort;

	-- to be output in the status bar:
	status_rotate : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to rotate unit." 
		& status_hint_for_abort;
	
	-- This procedure:
	-- - Clears list of proposed units.
	-- - Sets global variable selected_unit to no_element.
	-- - resets global variable "unit_move" to its default values
	procedure reset_unit_move;

	-- Assigns the final position after the move to the selected unit.
	-- Resets the global variable "unit".
	procedure finalize_move (
		destination		: in type_point;
		log_threshold	: in type_log_level);


	-- While dragging a unit, the attached segments must be dragged along.
	-- So we need a list of selected segments.
	-- There must also be information about the zone of the segment being dragged at:
	subtype type_drag_zone is type_line_zone range START_POINT .. END_POINT;
	
-- 	type type_segment_being_dragged is new et_canvas_schematic_nets.type_selected_segment with record
	type type_segment_being_dragged is record
		segment	: type_net_segment;
		zone	: type_drag_zone;
	end record;

	package pac_segments_being_dragged is new doubly_linked_lists (type_segment_being_dragged);
	use pac_segments_being_dragged;

	segments_being_dragged : pac_segments_being_dragged.list;

	-- Clears list of segments being dragged:
	procedure reset_segments_being_dragged;
	
	-- Assigns the final position after the drag to the selected unit.
	-- Resets the global variable "unit".
	procedure finalize_drag (
		destination		: in type_point;
		log_threshold	: in type_log_level);
	
	-- Locates all units in the vicinity of given point.
	-- If more than one unit near point found, then it sets the
	-- cursor selected_unit to the first unit and requests
	-- for clarification.
	-- If there is only one unite, sets global variable selected_unit accordingly.
	-- If there is no unit, then selected_unit is set to no_element.
	procedure find_units_for_move (point : in type_point);

	-- Locates net segments attached to the unit indicated by
	-- cursor selected_unit. Collects the segments in list
	-- segments_being_dragged (see above):
	procedure find_attached_segments;


	-- Rotates a unit in the vicinity of given point by 90 degree.
	-- If more than one unit near point found, then it sets the
	-- cursor selected_unit to the first unit and requests
	-- for clarification.
	procedure rotate_unit (point : in type_point);

	-- Rotate the unit being pointed at by cursor selected_unit.
	-- Call this procedure after a clarification.
	procedure rotate_selected_unit;


-- ADD UNIT/DEVICE

	-- This function returns the top most important path
	-- of the preferred schematic libraries.
	function get_top_most_important_library return string;

	type type_device_selection is record
		window	: gtk_window;

		-- This flag indicates that the
		-- window is open. The purpose of this flag is
		-- to prevent the window from being opended
		-- multiple times:
		open	: boolean := false;
	end record;
	
	device_selection : type_device_selection;

	-- Returns the status of the "open" flag.
	-- True if the device selection is open.
	-- False if the selection is not open.
	function device_selection_is_open return boolean;
	
	-- Closes the device selection window.
	-- Resets the status flag "open".
	procedure close_device_selection;
	
	-- In order to place a package variant and the associated model
	-- on a menu, use this function.
	-- Field separator is space:
	function to_package_variant_item (variant : in pac_variants.cursor)
		return string;

	-- In order to extract the actual variant name from a the 
	-- menu of package variants, use this function.
	-- Field separator is space.
	function extract_variant_name (menu_item : in string) 
		return pac_package_variant_name.bounded_string;

	
	-- to be output in the status bar:
	status_add : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to add unit." 
		& status_hint_for_abort;

	-- When a unit is being added this type is required:
	type type_unit_being_added is record
		tool		: type_tool := MOUSE;

		-- The cursor to the device model:
		device		: pac_devices_lib.cursor;
		
		variant		: pac_package_variant_name.bounded_string; -- N, D, S_0805
		name		: pac_unit_name.bounded_string; -- A, B, PWR_IO3

		-- The total number of units provided by the device model:
		total		: type_unit_count := type_unit_count'first;
		
		-- The prospective device name (like IC4) once the 
		-- add operation is complete.
		-- This is relevant for the preview only:
		device_pre	: et_devices.type_device_name := (others => <>);

		-- When drawing a unit being added via invoke:
		via_invoke	: boolean := false;
	end record;

	unit_add : type_unit_being_added;

	procedure reset_unit_add;

	-- Builds the device selection window which lets the operator
	-- select the directory and the actual device model.
	-- The window is already up, then nothing happens here:
	procedure add_device;

	procedure finalize_add_device (
		position	: in type_point);


-- INVOKE UNIT
	
	-- to be output in the status bar:
	status_invoke : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to invoke unit." 
		& status_hint_for_abort;
	

	-- Does the final invoking of the unit in the schematic:
	procedure finalize_invoke (
		position		: in type_point;
		log_threshold	: in type_log_level);
	
	-- Shows the available units of the selected device in a menu.
	procedure show_units;
	
	-- Collects units in the vicinity of the given point.
	-- Requests for clarification if more than one unit found.
	procedure invoke_unit (point : in type_point);
	
	
-- PLACEHOLDERS

	-- Whenever a placeholder (for value, purpose or name) is selected
	-- via the GUI, we store cursors of its
	-- parent device and unit via this type.
	subtype type_selected_placeholder is type_selected_unit;
	
	package pac_proposed_placeholders is new doubly_linked_lists (type_selected_placeholder);
	use pac_proposed_placeholders;

	-- These variables are used by the GUI when the operator selects a placeholder:
	proposed_placeholders	: pac_proposed_placeholders.list;
	selected_placeholder	: pac_proposed_placeholders.cursor;

	-- Advances to next placeholder in 
	-- list of proposed placeholders:
	procedure clarify_placeholder;

	-- Clears the list proposed placeholders:
	procedure clear_proposed_placeholders;

	
	-- Global information for the GUI when a placeholder 
	-- is being moved.
	-- The category indicates whether the placeholder 
	-- for name, value or purpose is meant.
	type type_placeholder is record
		category			: type_placeholder_meaning := NAME;
		being_moved			: boolean := false;
		tool				: type_tool := MOUSE;
		absolute_position	: type_point; -- before the move

		device				: type_device_name := (others => <>); -- IC45
		unit				: pac_unit_name.bounded_string; -- A
	end record;

	placeholder_move : type_placeholder;

	
	
	-- Resets placeholder_move and clears
	-- the list of proposed placeholders:
	procedure reset_placeholder;

	
	-- to be output in the status bar:
	status_move_placeholder : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to move placeholder." 
		& status_hint_for_abort;

	-- to be output in the status bar:
	status_rotate_placeholder : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to rotate placeholder." 
		& status_hint_for_abort;

	
	-- Assigns the final position after the move to the selected placeholder.
	-- Resets the global variable "placeholder".
	procedure finalize_move_placeholder (
		destination		: in type_point;
		category		: in type_placeholder_meaning;
		log_threshold	: in type_log_level);

	-- Locates all placeholders in the vicinity of given point.
	-- If more than one placeholder near point found, then it sets the
	-- cursor selected_placeholder to the first placeholder and requests
	-- for clarification.
	procedure find_placeholders (
		point		: in type_point;
		category	: in type_placeholder_meaning);

	-- Rotates the placeholder indicated by selected_placeholder:
	procedure rotate_selected_placeholder (
		category	: in type_placeholder_meaning);

	-- Locates all placeholders of given category in the vicinity of given point.
	-- If more than one placeholder near point found, then it sets the
	-- cursor selected_placeholder to the first placeholder and requests
	-- for clarification.
	-- If only one placeholder found, then the placeholder will be rotated.
	procedure rotate_placeholder (
		point 		: in type_point;
		category	: in type_placeholder_meaning);



-- SET PROPERTIES (value, partcode, purpose, name)

	-- to be output in the status bar:
	status_rename : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to rename device." 
		& status_hint_for_abort;

	status_set_value : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to set value of device." 
		& status_hint_for_abort;

	status_set_purpose : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to set purpose of device." 
		& status_hint_for_abort;

	status_set_partcode : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to set partcode of device." 
		& status_hint_for_abort;

	status_set_variant : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to set package variant of device." 
		& status_hint_for_abort;

	-- Opens a window to enter the properties of the
	-- device indicated by variable "selected_unit".
	-- The actual property is determined by the current
	-- verb and noun.
	-- If the indicated unit belongs to a virtual device, then
	-- an error message will be output in the status bar.
	--  Virtual units have no value, partcode or purpose because
	--  they do not exist in reality.
	procedure window_set_property;
	
	-- Sets a property a unit in the vicinity of given point.
	-- Since a unit is a subset of a device, the property will be 
	-- applied for the whole device.
	-- A property is for example the value, the purpose or
	-- the partcode.
	-- If more than one unit near point found, then it sets the
	-- cursor selected_unit to the first unit and requests
	-- for clarification.
	-- If the selected unit belongs to a virtual device, then
	-- an error message will be output in the status bar.
	--  Virtual units have no value, partcode or purpose because
	--  they do not exist in reality.
	procedure set_property (point : in type_point);

	-- Sets the property of the selected unit (and the whole
	-- device) of the unit being pointed at by cursor selected_unit.
	-- Call this procedure after a clarification.
	-- If the selected unit belongs to a virtual device, then
	-- an error message will be output in the status bar.
	--  Virtual units have no value, partcode or purpose because
	--  they do not exist in reality.
	procedure set_property_selected_unit;


	status_show_device : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to select device." 
		& status_hint_for_abort;

	-- Outputs in the status bar some helpful properties
	-- of the selected device:
	procedure show_properties_of_selected_device; 
	
	-- Locates all units in the vicinity of given point.
	-- If more than one unit near point found, then it sets the
	-- cursor selected_unit to the first unit and requests
	-- for clarification.
	-- If there is only one unite, sets global variable selected_unit accordingly.
	-- If there is no unit, then selected_unit is set to no_element.
	procedure find_units_for_show (point : in type_point);

	
end et_canvas_schematic_units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
