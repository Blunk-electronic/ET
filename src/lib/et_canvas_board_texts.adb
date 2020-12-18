------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD TEXTS                               --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

with ada.text_io;					use ada.text_io;

with glib;
with glib.values;

with gdk.types;						use gdk.types;
with gdk.event;						use gdk.event;
with gdk.types.keysyms;				use gdk.types.keysyms;


with gtk.widget;					use gtk.widget;
with gtk.box;

with gtk.combo_box;					use gtk.combo_box;
with gtk.cell_renderer_text;		
with gtk.cell_layout;        		
with gtk.list_store;				
with gtk.tree_model;

with gtk.combo_box_text;			use gtk.combo_box_text;
with gtk.label;
with gtk.gentry;					use gtk.gentry;
with gtk.button;					use gtk.button;
with gtk.text_buffer;
with gtk.text_iter;
--with gtk.menu;
--with gtk.menu_item;
--with gtk.menu_shell;


package body et_canvas_board_texts is

	procedure close_window_place_text is begin
		window_place_text.window.destroy;
		window_place_text.open := false;
		reset_text_place;
	end close_window_place_text;	
	
	function window_place_text_key_event (
		self	: access gtk_widget_record'class;
		event	: in gdk_event_key) 
		return boolean 
	is
		key : gdk_key_type := event.keyval;

		-- This is required in order to propagate the key-pressed event further.
		result : boolean; -- to be returned. Indicates that the event has been handled.
	begin
		case key is
			when GDK_ESCAPE =>
				--put_line ("key A");

				-- Close the window if operator hits ESC:
				close_window_place_text;
				result := true;

			when others =>
				--put_line ("key B");
				result := false;
		end case;
		
		return result;
	end window_place_text_key_event;


	
	procedure close_window_place_text (
		self	: access gtk_widget_record'class) 
	is begin
		window_place_text.open := false;
		reset_text_place;
	end close_window_place_text;

	procedure layer_category_changed (combo : access gtk_combo_box_record'class) is
		use glib;
		use gtk.tree_model;
		use gtk.list_store;

		-- Get the model and active iter from the combo box:
		model : constant gtk_tree_model := combo.get_model;
		iter : constant gtk_tree_iter := combo.get_active_iter;

		item_text : glib.values.gvalue;
	begin
		-- Get the actual text of the entry (column is 0):
		gtk.tree_model.get_value (model, iter, 0, item_text);

		text_place.category := to_layer_category (glib.values.get_string (item_text));
		put_line ("cat " & to_string (text_place.category));
	end layer_category_changed;

	procedure face_changed (combo : access gtk_combo_box_record'class) is
		use glib;
		use gtk.tree_model;
		use gtk.list_store;

		-- Get the model and active iter from the combo box:
		model : constant gtk_tree_model := combo.get_model;
		iter : constant gtk_tree_iter := combo.get_active_iter;

		item_text : glib.values.gvalue;
	begin
		-- Get the actual text of the entry (column is 0):
		gtk.tree_model.get_value (model, iter, 0, item_text);

		text_place.face := to_face (glib.values.get_string (item_text));
		put_line ("face " & to_string (text_place.face));
	end face_changed;

	procedure signal_layer_changed (combo : access gtk_combo_box_record'class) is
		use glib;
		use gtk.tree_model;
		use gtk.list_store;

		-- Get the model and active iter from the combo box:
		model : constant gtk_tree_model := combo.get_model;
		iter : constant gtk_tree_iter := combo.get_active_iter;

		item_text : glib.values.gvalue;
	begin
		-- Get the actual text of the entry (column is 0):
		gtk.tree_model.get_value (model, iter, 0, item_text);

		text_place.signal_layer := to_signal_layer (glib.values.get_string (item_text));
		put_line ("signal layer " & to_string (text_place.signal_layer));
	end signal_layer_changed;
	
	procedure size_entered (combo_entry : access gtk_entry_record'class) is 
		text : constant string := get_text (combo_entry);
		size : type_text_size;
	begin
		put_line ("size " & text);
		-- CS validate. output error in status bar
		size := to_distance (text);
		text_place.text.size := size;
		
	end size_entered;

	procedure line_width_entered (combo_entry : access gtk_entry_record'class) is 
		text : constant string := get_text (combo_entry);
		line_width : type_text_line_width;
	begin
		put_line ("line width " & text);
		-- CS validate. output error in status bar
		line_width := to_distance (text);
		text_place.text.line_width := line_width;
		
	end line_width_entered;

	procedure button_apply_clicked (button : access gtk_button_record'class) is
		use gtk.text_view;
		use gtk.text_buffer;
		use gtk.text_iter;
		use et_text;
		
		text_buffer : constant gtk_text_buffer := get_buffer (text_place.entry_content);
		lower_bound, upper_bound : gtk_text_iter;
	begin
		put_line ("button apply clicked");

		get_bounds (text_buffer, lower_bound, upper_bound);

		--put_line ("content: " & get_text (text_buffer, lower_bound, upper_bound));

		text_place.text.content := to_content (get_text (text_buffer, lower_bound, upper_bound));

		put_line ("content: " & enclose_in_quotes (to_string (text_place.text.content)));

		text_place.being_moved := true;
	end button_apply_clicked;

	procedure reset_text_place is begin
		text_place.being_moved := false;
	end reset_text_place;
	
	procedure place_text is
		use gtk.window;
		use gtk.box;
		use gtk.label;
		use gtk.gentry;

		box_main : gtk_vbox;
		
		box_layer_category, box_face, 
		box_signal_layer, box_content, box_button,
		box_size, box_line_width : gtk_hbox;
		
		label_layer_category, label_face, 
		label_signal_layer, label_content,
		label_size, label_line_width : gtk_label;
		
		cbox_category, cbox_face, cbox_signal_layer : gtk_combo_box;
		cbox_line_width, cbox_size : gtk_combo_box_text;
		
		--entry_content : gtk_text_view;
		button_apply : gtk_button;
		
		use glib;
		use gtk.cell_renderer_text;
		use gtk.cell_layout;
		use gtk.list_store;
		use gtk.tree_model;
		use gtk.text_view;

		spacing : constant natural := 10;

		procedure make_combo_for_categories is
			storage_model : gtk_list_store;

			-- An entry consists of just a single column:
			column_0 : constant := 0;

			-- The single column is to contain strings:
			entry_structure : glib.gtype_array := (column_0 => glib.gtype_string);

			iter : gtk_tree_iter;			
			render : gtk_cell_renderer_text;
		begin
			-- Create the storage model:
			gtk_new (list_store => storage_model, types => (entry_structure));

			-- Insert the layer categories in the storage model:
			for choice in 0 .. type_layer_category'pos (type_layer_category'last) loop
				storage_model.append (iter);
				gtk.list_store.set (storage_model, iter, column_0,
					to_string (type_layer_category'val (choice)));
			end loop;

			-- Create the combo box:
			gtk.combo_box.gtk_new_with_model (
				combo_box	=> cbox_category,
				model		=> +storage_model); -- ?

			-- Set the category used last:
			cbox_category.set_active (type_layer_category'pos (text_place.category));


			pack_start (box_layer_category, cbox_category, padding => guint (spacing));
			cbox_category.on_changed (layer_category_changed'access);

			-- The purpose of this stuff is unclear, but it
			-- is required to make the entries visible:
			gtk_new (render);
			pack_start (cbox_category, render, expand => true);
			add_attribute (cbox_category, render, "markup", column_0);
		end make_combo_for_categories;

		procedure make_combo_for_face is
			storage_model : gtk_list_store;

			-- An entry consists of just a single column:
			column_0 : constant := 0;

			-- The single column is to contain strings:
			entry_structure : glib.gtype_array := (column_0 => glib.gtype_string);

			iter : gtk_tree_iter;			
			render : gtk_cell_renderer_text;
		begin
			-- Create the storage model:
			gtk_new (list_store => storage_model, types => (entry_structure));

			-- Insert the "faces" in the storage model:
			for choice in 0 .. type_face'pos (type_face'last) loop
				storage_model.append (iter);
				gtk.list_store.set (storage_model, iter, column_0,
					type_face'image (type_face'val (choice)));
			end loop;

			-- Create the combo box:
			gtk.combo_box.gtk_new_with_model (
				combo_box	=> cbox_face,
				model		=> +storage_model); -- ?

			-- Set the face used last:
			cbox_face.set_active (type_face'pos (text_place.face));


			pack_start (box_face, cbox_face, padding => guint (spacing));
			cbox_face.on_changed (face_changed'access);

			-- The purpose of this stuff is unclear, but it
			-- is required to make the entries visible:
			gtk_new (render);
			pack_start (cbox_face, render, expand => true);
			add_attribute (cbox_face, render, "markup", column_0);

		end make_combo_for_face;

		procedure make_combo_for_signal_layer is
			storage_model : gtk_list_store;

			-- An entry consists of just a single column:
			column_0 : constant := 0;

			-- The single column is to contain strings:
			entry_structure : glib.gtype_array := (column_0 => glib.gtype_string);

			iter : gtk_tree_iter;			
			render : gtk_cell_renderer_text;

		begin
			-- Create the storage model:
			gtk_new (list_store => storage_model, types => (entry_structure));

			-- Insert the available signal layers in the storage model:
			for choice in 
				-- The top layer is always available:
				type_signal_layer'first .. 

				-- The deepest available layer depends on the stack configuration:
				deepest_conductor_layer (et_canvas_schematic.current_active_module) 
			loop
				storage_model.append (iter);
				gtk.list_store.set (storage_model, iter, column_0,
					type_signal_layer'image (choice));
			end loop;

			-- Create the combo box:
			gtk.combo_box.gtk_new_with_model (
				combo_box	=> cbox_signal_layer,
				model		=> +storage_model); -- ?

			-- Set the signal layer used last:
			cbox_signal_layer.set_active (gint (text_place.signal_layer) - 1);
			-- NOTE: The entries are numbered from 0 .. N.


			pack_start (box_signal_layer, cbox_signal_layer, padding => guint (spacing));
			cbox_signal_layer.on_changed (signal_layer_changed'access);

			-- The purpose of this stuff is unclear, but it
			-- is required to make the entries visible:
			gtk_new (render);
			pack_start (cbox_signal_layer, render, expand => true);
			add_attribute (cbox_signal_layer, render, "markup", column_0);

		end make_combo_for_signal_layer;

		
	begin -- place_text
		
		-- If it is already up, move it to the foreground.
		-- Otherwise build it:
		if window_place_text.open then
			window_place_text.window.present;
		else
			window_place_text.open := true;
		
			gtk_new (window_place_text.window);
			window_place_text.window.set_title ("Set text properties");
			window_place_text.window.on_destroy (close_window_place_text'access);
			window_place_text.window.on_key_press_event (window_place_text_key_event'access);

			gtk_new_vbox (box_main, homogeneous => false);
			window_place_text.window.add (box_main);

			-- LAYER CAT
			gtk_new_hbox (box_layer_category, homogeneous => false);
			pack_start (box_main, box_layer_category, padding => guint (spacing));

			gtk_new (label_layer_category, "LAYER CAT");
			pack_start (box_layer_category, label_layer_category, padding => guint (spacing));
			make_combo_for_categories;
			
			-- FACE
			gtk_new_hbox (box_face, homogeneous => false);
			pack_start (box_main, box_face, padding => guint (spacing));
			
			gtk_new (label_face, "FACE");
			pack_start (box_face, label_face, padding => guint (spacing));
			make_combo_for_face;

			-- SIGNAL LAYER
			gtk_new_hbox (box_signal_layer, homogeneous => false);
			pack_start (box_main, box_signal_layer, padding => guint (spacing));
			
			gtk_new (label_signal_layer, "SIGNAL LAYER");
			pack_start (box_signal_layer, label_signal_layer, padding => guint (spacing));
			make_combo_for_signal_layer;

			-- CS ROTATION

			
			-- SIZE
			gtk_new_hbox (box_size, homogeneous => false);
			pack_start (box_main, box_size, padding => guint (spacing));
			
			gtk_new (label_size, "SIZE");
			pack_start (box_size, label_size, padding => guint (spacing));

			gtk_new_with_entry (cbox_size);
			pack_start (box_size, cbox_size, padding => guint (spacing));
			gtk_entry (cbox_size.get_child).on_activate (size_entered'access);

			-- LINE WIDTH
			gtk_new_hbox (box_line_width, homogeneous => false);
			pack_start (box_main, box_line_width, padding => guint (spacing));

			gtk_new (label_line_width, "LINE WIDTH");
			pack_start (box_line_width, label_line_width, padding => guint (spacing));

			gtk_new_with_entry (cbox_line_width);
			pack_start (box_line_width, cbox_line_width, padding => guint (spacing));
			gtk_entry (cbox_line_width.get_child).on_activate (line_width_entered'access);
			
			-- CONTENT
			gtk_new_hbox (box_content, homogeneous => false);
			pack_start (box_main, box_content, padding => guint (spacing));

			gtk_new (label_content, "CONTENT");
			pack_start (box_content, label_content, padding => guint (spacing));

			gtk_new (text_place.entry_content);
			pack_start (box_content, text_place.entry_content, padding => guint (spacing));

			-- OK BUTTON
			gtk_new_hbox (box_button, homogeneous => false);
			pack_start (box_main, box_button, padding => guint (spacing));

			gtk_new (button_apply, "Apply");
			pack_start (box_button, button_apply, padding => guint (spacing));
			button_apply.on_clicked (button_apply_clicked'access);
			
			window_place_text.window.show_all;
		end if;
		
	end place_text;
	
end et_canvas_board_texts;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
