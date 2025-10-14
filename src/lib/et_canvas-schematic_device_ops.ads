------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   CANVAS SCHEMATIC DEVICE OPERATIONS                     --
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


with gtk.list_store;				use gtk.list_store;

with et_package_variant;			use et_package_variant;
with et_devices_electrical;			use et_devices_electrical;
with et_device_name;				use et_device_name;

generic

	
package et_canvas.schematic_device_ops is





-- RENAME WINDOW:

	-- The window to rename devices is used in all domains
	-- and in various situations. For this reason we make it
	-- a generic object:
	rename_window : gtk.window.gtk_window;

	-- This flag indicates that the rename window is open:
	rename_window_open : boolean := false;
	
	-- This is the field inside the rename_window 
	-- where the operator enters the new name of a device:
	rename_new : gtk_gentry;
	
	-- This is the field inside the rename_window 
	-- where the old name of a device is shown:
	rename_old : gtk_gentry;

	
	-- This procedure assembles the rename_window with 
	-- all its basic properties.
	-- It connects the "on_key_press_event" with the
	-- callback function cb_rename_window_key_pressed (see below).
	-- This procedure DOES NOT show the rename window. It just prepares
	-- basic things. The actual showing is preformed by a procedure in
	-- the package where the canvas is instantiated (see
	-- for example procedure show_rename_window 
	-- in et_canvas_schematic_units):
	procedure build_rename_window;






-- VALUE WINDOW:

	-- The window to set the value of devices is used in all domains
	-- and in various situations. For this reason we make it
	-- a generic object:
	value_window : gtk.window.gtk_window;

	-- This flag indicates that the value window is open:
	value_window_open : boolean := false;
	
	-- This is the field inside the value_window 
	-- where the operator enters the new value of a device:
	value_new : gtk_gentry;
	
	-- This is the field inside the value_window 
	-- where the old value of a device is shown:
	value_old : gtk_gentry;

	
	-- This procedure assembles the value_window with 
	-- all its basic properties. It sets the title of the 
	-- window with the targeted device name so that the operator
	-- knows what device it is about.
	-- It connects the "on_key_press_event" with the
	-- callback function cb_value_window_key_pressed (see below).
	-- This procedure DOES NOT show the value window. It just prepares
	-- basic things. The actual showing is preformed by a procedure in
	-- the package where the canvas is instantiated (see
	-- for example procedure show_value_window 
	-- in et_canvas_schematic_units):
	procedure build_value_window (
		device_name : in type_device_name);







-- PURPOSE WINDOW:

	-- The window to set the value of devices is used in all domains
	-- and in various situations. For this reason we make it
	-- a generic object:
	purpose_window : gtk.window.gtk_window;

	-- This flag indicates that the value window is open:
	purpose_window_open : boolean := false;
	
	-- This is the field inside the purpose_window 
	-- where the operator enters the new value of a device:
	purpose_new : gtk_gentry;
	
	-- This is the field inside the purpose_window 
	-- where the old value of a device is shown:
	purpose_old : gtk_gentry;

	
	-- This procedure assembles the purpose_window with 
	-- all its basic properties. It sets the title of the 
	-- window with the targeted device name so that the operator
	-- knows what device it is about.
	-- It connects the "on_key_press_event" with the
	-- callback function cb_purpose_window_key_pressed (see below).
	-- This procedure DOES NOT show the value window. It just prepares
	-- basic things. The actual showing is preformed by a procedure in
	-- the package where the canvas is instantiated (see
	-- for example procedure show_purpose_window 
	-- in et_canvas_schematic_units):
	procedure build_purpose_window (
		device_name : in type_device_name);

	




-- PARTCODE WINDOW:

	-- The window to set the value of devices is used in all domains
	-- and in various situations. For this reason we make it
	-- a generic object:
	partcode_window : gtk.window.gtk_window;

	-- This flag indicates that the value window is open:
	partcode_window_open : boolean := false;
	
	-- This is the field inside the partcode_window 
	-- where the operator enters the new value of a device:
	partcode_new : gtk_gentry;
	
	-- This is the field inside the partcode_window 
	-- where the old value of a device is shown:
	partcode_old : gtk_gentry;

	
	-- This procedure assembles the partcode_window with 
	-- all its basic properties. It sets the title of the 
	-- window with the targeted device name so that the operator
	-- knows what device it is about.
	-- It connects the "on_key_press_event" with the
	-- callback function cb_partcode_window_key_pressed (see below).
	-- This procedure DOES NOT show the value window. It just prepares
	-- basic things. The actual showing is preformed by a procedure in
	-- the package where the canvas is instantiated (see
	-- for example procedure show_partcode_window 
	-- in et_canvas_schematic_units):
	procedure build_partcode_window (
		device_name : in type_device_name);









-- PACKAGE VARIANT WINDOW:



	-- This procedure iterates through the given list of
	-- package variants and adds them one by one
	-- to a so called "store". The store is required
	-- to fill a combo box for the variants with content:
	procedure make_store_for_variants (
		variants	: in pac_package_variants.map;
		store 		: in out gtk_list_store);

	

	
	-- The window to set the package_variant of devices is used in all domains
	-- and in various situations. For this reason we make it
	-- a generic object:
	package_variant_window : gtk.window.gtk_window;

	-- This flag indicates that the package_variant window is open:
	package_variant_window_open : boolean := false;
	
	-- This is the box inside the package_variant_window 
	-- where the operator selectes the new package_variant of a device:
	package_variant_new : gtk_combo_box;
	
	-- This is the field inside the package_variant_window 
	-- where the old package_variant of a device is shown:
	package_variant_old : gtk_gentry;
	
	
	-- This procedure assembles the package_variant_window with 
	-- all its basic properties. It sets the title of the 
	-- window with the targeted device name so that the operator
	-- knows what device it is about.
	-- It connects the "on_key_press_event" with the
	-- callback function cb_package_variant_window_key_pressed (see below).
	-- This procedure DOES NOT show the package_variant window. It just prepares
	-- basic things. The actual showing is preformed by a procedure in
	-- the package where the canvas is instantiated (see
	-- for example procedure show_package_variant_window 
	-- in et_canvas_schematic_units):
	procedure build_package_variant_window (
		device_cursor : in pac_devices_sch.cursor);



	
private
	
	

-- RENAME WINDOW:

	-- See comments on rename window above.
	
	-- This callback function is called whenever
	-- the operator presses a key in the rename window.
	-- If ESC key pressed, then the window is destroyed
	-- by emitting the "destroy" signal. The connection
	-- to the "destroy" signal is estabilshed in the package
	-- where the canvas is instantiated.
	-- All other key-press events are propagated to the
	-- field where the new name is entered (see 
	-- variable "rename_new" above):
	function cb_rename_window_key_pressed (
		window	: access gtk_widget_record'class;
		event	: gdk_event_key)
		return boolean;
	

	access_cb_rename_window_key_pressed : constant
		cb_gtk_widget_gdk_event_key_boolean := cb_rename_window_key_pressed'access;


	



-- VALUE WINDOW:

	-- See comments on device value window above.
	
	-- This callback function is called whenever
	-- the operator presses a key in the value window.
	-- If ESC key pressed, then the window is destroyed
	-- by emitting the "destroy" signal. The connection
	-- to the "destroy" signal is estabilshed in the package
	-- where the canvas is instantiated.
	-- All other key-press events are propagated to the
	-- field where the new name is entered (see 
	-- variable "value_new" above):
	function cb_value_window_key_pressed (
		window	: access gtk_widget_record'class;
		event	: gdk_event_key)
		return boolean;
	

	access_cb_value_window_key_pressed : constant
		cb_gtk_widget_gdk_event_key_boolean := cb_value_window_key_pressed'access;

	




-- PURPOSE WINDOW:

	-- See comments on device purpose window above.
	
	-- This callback function is called whenever
	-- the operator presses a key in the value window.
	-- If ESC key pressed, then the window is destroyed
	-- by emitting the "destroy" signal. The connection
	-- to the "destroy" signal is estabilshed in the package
	-- where the canvas is instantiated.
	-- All other key-press events are propagated to the
	-- field where the new name is entered (see 
	-- variable "purpose_new" above):
	function cb_purpose_window_key_pressed (
		window	: access gtk_widget_record'class;
		event	: gdk_event_key)
		return boolean;
	

	access_cb_purpose_window_key_pressed : constant
		cb_gtk_widget_gdk_event_key_boolean := cb_purpose_window_key_pressed'access;







-- PARTCODE WINDOW:

	-- See comments on device partcode window above.
	
	-- This callback function is called whenever
	-- the operator presses a key in the value window.
	-- If ESC key pressed, then the window is destroyed
	-- by emitting the "destroy" signal. The connection
	-- to the "destroy" signal is estabilshed in the package
	-- where the canvas is instantiated.
	-- All other key-press events are propagated to the
	-- field where the new name is entered (see 
	-- variable "partcode_new" above):
	function cb_partcode_window_key_pressed (
		window	: access gtk_widget_record'class;
		event	: gdk_event_key)
		return boolean;
	

	access_cb_partcode_window_key_pressed : constant
		cb_gtk_widget_gdk_event_key_boolean := cb_partcode_window_key_pressed'access;





-- PACKAGE VARIANT WINDOW:

	-- See comments on device package_variant window above.
	
	-- This callback function is called whenever
	-- the operator presses a key in the value window.
	-- If ESC key pressed, then the window is destroyed
	-- by emitting the "destroy" signal. The connection
	-- to the "destroy" signal is estabilshed in the package
	-- where the canvas is instantiated.
	-- All other key-press events are propagated to the
	-- field where the new name is entered (see 
	-- variable "package_variant_new" above):
	function cb_package_variant_window_key_pressed (
		window	: access gtk_widget_record'class;
		event	: gdk_event_key)
		return boolean;
	

	access_cb_package_variant_window_key_pressed : constant
		cb_gtk_widget_gdk_event_key_boolean := cb_package_variant_window_key_pressed'access;

	
	
end et_canvas.schematic_device_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
