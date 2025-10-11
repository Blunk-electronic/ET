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
with et_device_value;				use et_device_value;
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
	-- for the new value in the value window:
	procedure cb_new_value_entered (
		self : access gtk.gentry.gtk_entry_record'class);

	
	-- This procedure shows the window where the
	-- operator sees the old value of the targeted device
	-- and where he can enter the new name of the device:
	procedure show_value_window;
	
	
	procedure set_value (
		point	: in type_vector_model);
							



	-- Called when the "on_activate" signal is emitted
	-- (usually when ENTER pressed) by the entry field
	-- for the new purpose in the purpose window:
	procedure cb_new_purpose_entered (
		self : access gtk.gentry.gtk_entry_record'class);

	
	-- This procedure shows the window where the
	-- operator sees the old purpose of the targeted device
	-- and where he can enter the new purpose of the device:
	procedure show_purpose_window;

	
	procedure set_purpose (
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
		value		: pac_device_value.bounded_string; -- 100k
		
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


	-- Rotates the unit_fetch by 90 degrees counter-clockwise
	-- if it is valid:	
	procedure rotate_unit_add;


	
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
	procedure show_fetch_menu;


	
	-- This type is required when a unit has been
	-- selected from the fetch menu. This type is indicated
	-- to store temporarily information for a preview
	-- of the unit before it dropped at its final position:
	type type_unit_fetch is record
		-- The cursor to the device model:
		device		: pac_devices_lib.cursor;

		-- The name of the unit:
		name		: pac_unit_name.bounded_string; -- A, B, PWR_IO3

		value		: pac_device_value.bounded_string; -- 100k
		
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


	-- Rotates the unit_fetch by 90 degrees counter-clockwise
	-- if it is valid:	
	procedure rotate_unit_fetch;

	
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




	status_show_device : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to select device." 
		& status_hint_for_abort;

	
end et_canvas_schematic_units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
