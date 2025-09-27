------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        CANVAS SCHEMATIC UNITS                            --
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
-- DESCRIPTION:
-- 

with ada.containers;	            use ada.containers;
with ada.containers.doubly_linked_lists;

with gtk.window;					use gtk.window;
with gtk.menu_item;					use gtk.menu_item;
with gtk.menu_shell;
with gtk.gentry;
with gtk.file_chooser_button;		use gtk.file_chooser_button;
with gtk.box;						use gtk.box;
with gtk.combo_box;					use gtk.combo_box;

with et_schematic_geometry;			use et_schematic_geometry;
with et_schematic_coordinates;		use et_schematic_coordinates;
use et_schematic_geometry.pac_geometry_2;

with et_generic_module;				use et_generic_module;
with et_symbol_model;				use et_symbol_model;
with et_package_variant;			use et_package_variant;
with et_device_model;				use et_device_model;
with et_device_library;				use et_device_library;
with et_device_name;				use et_device_name;
with et_nets;						use et_nets;
with et_net_segment;				use et_net_segment;
with et_unit_name;					use et_unit_name;
with et_units;						use et_units;
with et_devices_electrical;			use et_devices_electrical;
with et_schematic_ops;				use et_schematic_ops;
with et_schematic_ops.units;		use et_schematic_ops.units;
with et_string_processing;			use et_string_processing;
with et_logging;					use et_logging;

with et_canvas_schematic_nets;
with et_device_placeholders;		use et_device_placeholders;

with et_canvas_messages;			use et_canvas_messages;
with et_canvas_tool;				use et_canvas_tool;


package et_canvas_schematic_units is

	use pac_generic_modules;

	-- Whenever a unit (or the whole device with all its units)
	-- is selected via the GUI, we store cursors of its
	-- parent device and the unit itself via this type.
	-- This type is to be used for a device and a unit that
	-- is already placed in the schematic. So we use it
	-- for example for moving, dragging or rotating of a unit.
	type type_selected_unit is record
		device	: pac_devices_sch.cursor;

		-- If the cursor to the actual unit is no_element then
		-- the whole device is regarded as selected:
		unit	: pac_units.cursor;
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
	-- function collect_units (
	-- 	module			: in pac_generic_modules.cursor;
	-- 	place			: in type_object_position; -- sheet/x/y
	-- 	zone			: in type_zone_radius; -- the circular area around the place
	-- 	log_threshold	: in type_log_level)
	-- 	return pac_proposed_units.list;



-- DELETE UNIT

	-- to be output in the status bar:
	status_delete_unit : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to delete unit." 
		& status_hint_for_abort;
	

	status_delete_device : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to delete device." 
		& status_hint_for_abort;

	

-- MOVE/DRAG/ROTATE UNIT

	
	-- to be output in the status bar:
	status_move : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to move unit." -- CS move object ?
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
	

	-- This procedure is required in order to clarify
	-- which object among the proposed objects is meant.
	-- On every call of this procedure we advance from one
	-- proposed segment to the next in a circular manner
	-- and set it as "selected":
	procedure clarify_object;
	

	-- Locates objects in the vicinity of the given point
	-- and sets their proposed-flag.
	-- Only displayed layers are taken into account.
	-- Depending on how many objects have been found, the behaviour is:
	-- - If only one object found, then it is selected automatically.
	-- - If more than one object found, then clarification is requested.
	--   The first object of them is selected.
	procedure find_objects (
		point : in type_vector_model);


	procedure move_object (
		tool	: in type_tool;
		point	: in type_vector_model);


	procedure rotate_object (
		point	: in type_vector_model);


	procedure delete_object (
		point	: in type_vector_model);




	
	-- Called when the "on_activate" signal is emitted
	-- (usually when ENTER pressed) by the entry field
	-- for the new name in the rename window:
	procedure cb_rename_new_name_entered (
		self : access gtk.gentry.gtk_entry_record'class);
	

	-- This procedure shows the window where the
	-- operator sees the old name of the targeted object
	-- and where he can enter the new name of the object:
	procedure show_rename_window;
	


	
	procedure rename_object (
		point	: in type_vector_model);

	
	
	procedure drag_object (
		tool	: in type_tool;
		point	: in type_vector_model);

	


	

-- ADD UNIT/DEVICE

	-- This function returns the top most important path
	-- of the preferred schematic libraries.
	function get_top_most_important_library return string;

		
	
	-- to be output in the status bar:
	status_add : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to add unit." 
		& status_hint_for_abort;


	
	-- When a unit is being added this type is required:
	type type_unit_being_added is record
		-- The cursor to the device model:
		device		: pac_devices_lib.cursor;
		
		variant		: pac_package_variant_name.bounded_string; -- N, D, S_0805
		name		: pac_unit_name.bounded_string; -- A, B, PWR_IO3

		-- The total number of units provided by the device model:
		total		: type_unit_count := type_unit_count'first;
		
		-- The prospective device name (like IC4) once the 
		-- add operation is complete.
		-- This is relevant for the preview only:
		device_pre	: type_device_name := (others => <>);

		-- The rotation of the unit:
		rotation	: type_rotation := 0.0;
		
		-- Indicates that the information above is valid
		-- and a unit has been selected from the fetch menu
		-- by the operator:
		valid		: boolean := false;		
	end record;


	-- If a unit of a new device is being added, then
	-- all the required preliminary information is stored here:
	unit_add : type_unit_being_added;


	-- Resets unit_add to its default values:
	procedure reset_unit_add;


	
	
	-- This procedure is called when the operator
	-- has selected a package variant:
	procedure cb_package_variant_selected (
		combo : access gtk_combo_box_record'class);

	
	-- This procedure is called when the operator
	-- has selected a library directory from inside 
	-- the properties box:
	procedure cb_model_directory_selected (
		button : access gtk_file_chooser_button_record'class);
	

	-- This is the box that contains a label
	-- and a combo box for package variants.
	-- The box is displayed in the properties box
	-- if package variants exist for a currently selected device model:
	box_package_variant : gtk_vbox;

	-- This flag indicates that the box for
	-- the package variant selection is displayed or not:
	box_package_variant_active : boolean := false;


	-- This procedure removes the box
	-- for the package variant selection from the properties box
	-- and resets the flag box_package_variant_active:
	procedure remove_box_package_variant;
	
	
	-- This procedure is called when the operator
	-- has selected a device model file from inside
	-- the properties box.
	-- Once the operator has selected a model file,
	-- the combo box for the available package variants
	-- is updated:
	procedure cb_device_model_selected (
		button : access gtk_file_chooser_button_record'class);
	

	-- When the operator wants to add a device to the
	-- drawing then this procedure should be called first.
	-- It builds the widgets for the device model selection
	-- in the properties box so that the operator can
	-- select the directory, model and package variant:
	procedure show_device_model_selection;



	-- This procedure adds the unit of a new device
	-- to the drawing. It takes the information stored
	-- in unit_add:
	procedure add_device (
		position	: in type_vector_model);




-- COPY DEVICE:
	
	-- to be output in the status bar:
	status_copy : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to copy a device." 
		& status_hint_for_abort;


	procedure copy_object (
		tool	: in type_tool;
		point	: in type_vector_model);


	
	
	
-- FETCH UNIT:

	-- To fetch a unit means to select a unit of
	-- a device that is already in use:

	-- to be output in the status bar:
	status_fetch : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to fetch unit from device." 
		& status_hint_for_abort;
	

	-- Shows the available units of 
	-- the selected device in a menu:
	procedure show_fetch_window;


	
	-- This type is required when a unit has been
	-- selected from the fetch menu. This type is indicated
	-- to store temporarily information for a preview
	-- of the unit before it dropped at its final position:
	type type_unit_fetch is record
		-- The cursor to the device model:
		device		: pac_devices_lib.cursor;

		-- The name of the unit:
		name		: pac_unit_name.bounded_string; -- A, B, PWR_IO3

		-- The total number of units provided by the device model:
		total		: type_unit_count := type_unit_count'first;
		
		-- The prospective device name (like IC4):
		device_pre	: type_device_name := (others => <>);

		-- The rotation of the unit:
		rotation	: type_rotation := 0.0;
		
		-- Indicates that the information above is valid
		-- and a unit has been selected from the fetch menu
		-- by the operator:
		valid		: boolean := false;
	end record;

	
	-- If a unit of a device is being fetched, then
	-- all the required preliminary information is stored here:
	unit_fetch : type_unit_fetch;


	
	-- Resets the information of the unit_fetch:
	procedure reset_unit_fetch;
	

	-- This callback procedure is called when the fetch menu is closed
	-- by the operator by pressing the ESCAPE key:
	procedure cb_fetch_menu_destroy (
		menu : access gtk.menu_shell.gtk_menu_shell_record'class);
	

	-- This callback procedure is called when the operator
	-- has selected a unit from the fetch menu.
	-- It sets the variable unit_fetch accordingly.
	-- Extracts from the selected menu item the unit name.
	procedure cb_fetch_menu_unit_select (
		menu : access gtk.menu_item.gtk_menu_item_record'class);

	
	
	-- This procedure is to be called when the operator wants
	-- to fetch a unit at the given point on the current sheet.
	-- 1. On the first call, it locates units in a certain zone around
	--    the given point. If more than one unit exists there, then
	--    the operator is requested to clarify.
	-- 2. If only one unit has been found or if the operator has
	--    clarified which unit was meant, then the fetch menu
	--    is opened so that the operator can choose an available unit.
	-- 3. On the next call the unit is dropped a the given point
	--    and inserted in the database:
	procedure fetch_unit (
		tool	: in type_tool;
		point	: in type_vector_model);




	
	
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
		absolute_position	: type_vector_model; -- before the move

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
		destination		: in type_vector_model;
		category		: in type_placeholder_meaning;
		log_threshold	: in type_log_level);

	procedure move_placeholder (
		tool		: in type_tool;
		position	: in type_vector_model;
		category	: in type_placeholder_meaning);

	
	-- Locates all placeholders in the vicinity of given point.
	-- If more than one placeholder near point found, then it sets the
	-- cursor selected_placeholder to the first placeholder and requests
	-- for clarification.
	procedure find_placeholders (
		point		: in type_vector_model;
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
		point 		: in type_vector_model;
		category	: in type_placeholder_meaning);



-- SET PROPERTIES (value, partcode, purpose, name)

	-- to be output in the status bar:
	status_rename_device : constant string := 
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
	procedure set_property (point : in type_vector_model);

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
	procedure find_units_for_show (point : in type_vector_model);

	
end et_canvas_schematic_units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
