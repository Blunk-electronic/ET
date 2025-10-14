------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                     CANVAS SCHEMATIC DEVICE OPERATIONS                   --
--                                                                          --
--                               B o d y                                    --
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


package body et_canvas.schematic_device_ops is


	
	
	procedure build_rename_window is 
		box : gtk_vbox;
		label_old, label_new : gtk.label.gtk_label;
		label_status	: gtk.label.gtk_label;
	begin
		gtk_new (rename_window);

		rename_window.set_title ("Rename Device");

		rename_window.set_default_size (400, 100);
		rename_window.set_resizable (false);

		-- Connect the "on_key_press_event" signal:
		rename_window.on_key_press_event (access_cb_rename_window_key_pressed);
		
		
		gtk_new_vbox (box);
		add (rename_window, box);

		-- show the old name:
		gtk_new (label_old, "old:");
		pack_start (box, label_old);
		
		gtk_new (rename_old);
		pack_start (box, rename_old);


		-- show the new name (will be entered by the operator later):
		gtk_new (label_new, "new:");
		pack_start (box, label_new);

		gtk_new (rename_new);
		pack_start (box, rename_new);

		-- gtk_new (label_status);
		-- pack_start (box, label_status);

		
	end build_rename_window;





		
	
	procedure build_value_window (
		device_name : in type_device_name)
	is 
		box : gtk_vbox;
		label_old, label_new : gtk.label.gtk_label;
		label_status : gtk.label.gtk_label;
	begin
		gtk_new (value_window);

		value_window.set_title ("Set Value of Device " & to_string (device_name));

		value_window.set_default_size (500, 100);
		value_window.set_resizable (false);

		-- Connect the "on_key_press_event" signal:
		value_window.on_key_press_event (access_cb_value_window_key_pressed);
		
		
		gtk_new_vbox (box);
		add (value_window, box);

		-- show the old value:
		gtk_new (label_old, "old:");
		pack_start (box, label_old);
		
		gtk_new (value_old);
		pack_start (box, value_old);


		-- show the new value (will be entered by the operator later):
		gtk_new (label_new, "new:");
		pack_start (box, label_new);

		gtk_new (value_new);
		pack_start (box, value_new);

		-- gtk_new (label_status);
		-- pack_start (box, label_status);

		
	end build_value_window;


	



	
	
	procedure build_purpose_window (
		device_name : in type_device_name)
	is 
		box : gtk_vbox;
		label_old, label_new : gtk.label.gtk_label;
		label_status : gtk.label.gtk_label;
	begin
		gtk_new (purpose_window);

		purpose_window.set_title ("Set Purpose of Device " & to_string (device_name));

		purpose_window.set_default_size (500, 100);
		purpose_window.set_resizable (false);

		-- Connect the "on_key_press_event" signal:
		purpose_window.on_key_press_event (access_cb_purpose_window_key_pressed);
		
		
		gtk_new_vbox (box);
		add (purpose_window, box);

		-- show the old purpose:
		gtk_new (label_old, "old:");
		pack_start (box, label_old);
		
		gtk_new (purpose_old);
		pack_start (box, purpose_old);


		-- show the new purpose (will be entered by the operator later):
		gtk_new (label_new, "new:");
		pack_start (box, label_new);

		gtk_new (purpose_new);
		pack_start (box, purpose_new);

		-- gtk_new (label_status);
		-- pack_start (box, label_status);

		
	end build_purpose_window;


	





	procedure build_partcode_window (
		device_name : in type_device_name)
	is 
		box : gtk_vbox;
		label_old, label_new : gtk.label.gtk_label;
		label_status : gtk.label.gtk_label;
	begin
		gtk_new (partcode_window);

		partcode_window.set_title ("Set partcode of Device " & to_string (device_name));

		partcode_window.set_default_size (500, 100);
		partcode_window.set_resizable (false);

		-- Connect the "on_key_press_event" signal:
		partcode_window.on_key_press_event (access_cb_partcode_window_key_pressed);
		
		
		gtk_new_vbox (box);
		add (partcode_window, box);

		-- show the old partcode:
		gtk_new (label_old, "old:");
		pack_start (box, label_old);
		
		gtk_new (partcode_old);
		pack_start (box, partcode_old);


		-- show the new partcode (will be entered by the operator later):
		gtk_new (label_new, "new:");
		pack_start (box, label_new);

		gtk_new (partcode_new);
		pack_start (box, partcode_new);

		-- gtk_new (label_status);
		-- pack_start (box, label_status);

		
	end build_partcode_window;



	



	procedure build_package_variant_window (
		device_name : in type_device_name)
	is 
		box : gtk_vbox;
		label_old, label_new : gtk.label.gtk_label;
		label_status : gtk.label.gtk_label;
	begin
		gtk_new (package_variant_window);

		package_variant_window.set_title ("Set Package Variant of Device " & to_string (device_name));

		package_variant_window.set_default_size (500, 100);
		package_variant_window.set_resizable (false);

		-- Connect the "on_key_press_event" signal:
		package_variant_window.on_key_press_event (access_cb_package_variant_window_key_pressed);
		
		
		gtk_new_vbox (box);
		add (package_variant_window, box);

		-- show the old variant:
		gtk_new (label_old, "old:");
		pack_start (box, label_old);
		
		gtk_new (package_variant_old);
		pack_start (box, package_variant_old);


		-- show the new variante (will be entered by the operator later):
		gtk_new (label_new, "new:");
		pack_start (box, label_new);

		gtk_new (package_variant_new);
		pack_start (box, package_variant_new);

		-- gtk_new (label_status);
		-- pack_start (box, label_status);

		
	end build_package_variant_window;


	


	
	
	function cb_rename_window_key_pressed (
		window	: access gtk_widget_record'class;
		event	: gdk_event_key)
		return boolean
	is
		debug : boolean := false;
		
		event_handled : boolean;
		key : gdk_key_type := event.keyval;		
	begin
		if debug then
			put_line ("cb_rename_window_key_pressed");
		end if;

		
		case key is
			when GDK_ESCAPE =>
				if debug then
					put_line ("ESC");
				end if;

				-- Emit the "destroy" signal.
				-- The connection to a callback procedure
				-- is established in the package where
				-- the canvas is instantiated. For example see procedure
				-- show_rename_window in et_cnavas_schematic:
				rename_window.destroy;
				
				event_handled := true;

				
			when others =>
				if debug then
					put_line ("other key");
				end if;
				
				event_handled := false;
		end case;
		
		return event_handled;
	end cb_rename_window_key_pressed;
	





	
	
	function cb_value_window_key_pressed (
		window	: access gtk_widget_record'class;
		event	: gdk_event_key)
		return boolean
	is
		debug : boolean := false;
		
		event_handled : boolean;
		key : gdk_key_type := event.keyval;		
	begin
		if debug then
			put_line ("cb_value_window_key_pressed");
		end if;

		
		case key is
			when GDK_ESCAPE =>
				if debug then
					put_line ("ESC");
				end if;

				-- Emit the "destroy" signal.
				-- The connection to a callback procedure
				-- is established in the package where
				-- the canvas is instantiated. For example see procedure
				-- show_rename_window in et_cnavas_schematic:
				value_window.destroy;
				
				event_handled := true;

				
			when others =>
				if debug then
					put_line ("other key");
				end if;
				
				event_handled := false;
		end case;
		
		return event_handled;
	end cb_value_window_key_pressed;

	



	



	function cb_purpose_window_key_pressed (
		window	: access gtk_widget_record'class;
		event	: gdk_event_key)
		return boolean
	is
		debug : boolean := false;
		
		event_handled : boolean;
		key : gdk_key_type := event.keyval;		
	begin
		if debug then
			put_line ("cb_purpose_window_key_pressed");
		end if;

		
		case key is
			when GDK_ESCAPE =>
				if debug then
					put_line ("ESC");
				end if;

				-- Emit the "destroy" signal.
				-- The connection to a callback procedure
				-- is established in the package where
				-- the canvas is instantiated. For example see procedure
				-- show_rename_window in et_cnavas_schematic:
				purpose_window.destroy;
				
				event_handled := true;

				
			when others =>
				if debug then
					put_line ("other key");
				end if;
				
				event_handled := false;
		end case;
		
		return event_handled;
	end cb_purpose_window_key_pressed;








	function cb_partcode_window_key_pressed (
		window	: access gtk_widget_record'class;
		event	: gdk_event_key)
		return boolean
	is
		debug : boolean := false;
		
		event_handled : boolean;
		key : gdk_key_type := event.keyval;		
	begin
		if debug then
			put_line ("cb_partcode_window_key_pressed");
		end if;

		
		case key is
			when GDK_ESCAPE =>
				if debug then
					put_line ("ESC");
				end if;

				-- Emit the "destroy" signal.
				-- The connection to a callback procedure
				-- is established in the package where
				-- the canvas is instantiated. For example see procedure
				-- show_rename_window in et_cnavas_schematic:
				partcode_window.destroy;
				
				event_handled := true;

				
			when others =>
				if debug then
					put_line ("other key");
				end if;
				
				event_handled := false;
		end case;
		
		return event_handled;
	end cb_partcode_window_key_pressed;






	

	function cb_package_variant_window_key_pressed (
		window	: access gtk_widget_record'class;
		event	: gdk_event_key)
		return boolean
	is
		debug : boolean := false;
		
		event_handled : boolean;
		key : gdk_key_type := event.keyval;		
	begin
		if debug then
			put_line ("cb_package_variant_window_key_pressed");
		end if;

		
		case key is
			when GDK_ESCAPE =>
				if debug then
					put_line ("ESC");
				end if;

				-- Emit the "destroy" signal.
				-- The connection to a callback procedure
				-- is established in the package where
				-- the canvas is instantiated. For example see procedure
				-- show_rename_window in et_cnavas_schematic:
				package_variant_window.destroy;
				
				event_handled := true;

				
			when others =>
				if debug then
					put_line ("other key");
				end if;
				
				event_handled := false;
		end case;
		
		return event_handled;
	end cb_package_variant_window_key_pressed;

	
	
end et_canvas.schematic_device_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

