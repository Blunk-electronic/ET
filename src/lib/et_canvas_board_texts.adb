------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD TEXTS                               --
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
-- DESCRIPTION:
-- 

with ada.text_io;					use ada.text_io;
with ada.strings;					use ada.strings;
with ada.strings.fixed; 			use ada.strings.fixed;
--with ada.characters;				use ada.characters;
--with ada.characters.handling;		use ada.characters.handling;


with glib;							use glib;
with glib.values;
with gtk.box;						use gtk.box;
with gtk.label;						use gtk.label;
with gtk.combo_box;					use gtk.combo_box;
with gtk.combo_box_text;			use gtk.combo_box_text;
with gtk.button;					use gtk.button;



with gdk.types;						use gdk.types;
with gdk.event;						use gdk.event;
with gdk.types.keysyms;				use gdk.types.keysyms;

with gtk.widget;					use gtk.widget;

with gtk.cell_renderer_text;		
with gtk.cell_layout;        		
with gtk.list_store;				
with gtk.tree_model;

with gtk.gentry;					use gtk.gentry;
with gtk.container;					use gtk.container;

with gtk.text_buffer;
with gtk.text_iter;
--with gtk.menu;
--with gtk.menu_item;
--with gtk.menu_shell;

with et_generic_module;					use et_generic_module;
with et_pcb;
with et_canvas_board_2;
use et_canvas_board_2.pac_canvas;

with et_board_ops;						use et_board_ops;
with et_board_ops.assy_doc;
with et_board_ops.silkscreen;
with et_board_ops.stopmask;
with et_board_ops.conductors;
with et_board_ops.text;
with et_modes.board;

with et_display;						use et_display;
with et_display.board;					use et_display.board;

with et_logging;						use et_logging;
with et_string_processing;				use et_string_processing;
with et_exceptions;						use et_exceptions;

with et_undo_redo;
with et_commit;

with et_canvas_board_preliminary_object;	use et_canvas_board_preliminary_object;


package body et_canvas_board_texts is


	procedure make_affected_layer_categories is
		use pac_layer_categories;
	begin
		layer_categories.clear;
		layer_categories.append (LAYER_CAT_ASSY);
		layer_categories.append (LAYER_CAT_CONDUCTOR);
		layer_categories.append (LAYER_CAT_SILKSCREEN);
		layer_categories.append (LAYER_CAT_STOPMASK);
	end make_affected_layer_categories;

	

	procedure reset_preliminary_text is
	begin
		-- CS reset content only ?
		null;
		
	end reset_preliminary_text;
		

	
	
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

		object_layer_category := to_layer_category (glib.values.get_string (item_text));
		--put_line ("cat " & to_string (object_layer_category));

		et_canvas_board_2.redraw_board;
		
		-- CS display layer ?
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

		object_face := to_face (values.get_string (item_text));
		--put_line ("face " & to_string (object_face));

		
		-- Auto-enable the selected layer category:
		case object_layer_category is
			when LAYER_CAT_ASSY =>
				enable_assy_doc (object_face);
			
			when LAYER_CAT_SILKSCREEN =>
				enable_silkscreen (object_face);

			when LAYER_CAT_STOPMASK =>
				enable_stopmask (object_face);

			when others => null;
		end case;

		
		et_canvas_board_2.redraw_board;		
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

		object_signal_layer := to_signal_layer (values.get_string (item_text));
		--put_line ("signal layer " & to_string (object_signal_layer));

		-- Auto-enable the selected conductor layer:
		enable_conductor (object_signal_layer);
		
		et_canvas_board_2.redraw_board;
	end signal_layer_changed;



	

	
	procedure apply_size (text : in string) is
		size : type_text_size;
	begin
		size := to_distance (text);

		-- CS validate. output error in status bar
		preliminary_text.text.size := size;

		et_canvas_board_2.redraw_board;
	end apply_size;



	
	
	
	function size_key_pressed (
		combo_entry	: access gtk_widget_record'class;
		event		: gdk_event_key) 
		return boolean 
	is
		event_handled : boolean := false;
		
		use gdk.types;
		key : gdk_key_type := event.keyval;

		gentry : gtk_gentry := gtk_gentry (combo_entry);
		text : constant string := get_text (gentry);
	begin
		case key is
			when GDK_ESCAPE =>
				et_canvas_board_2.reset;
			
			when GDK_TAB => 
				--put_line ("size via tab " & text);
				apply_size (text);
				
			when others => nulL;
		end case;
		
		return event_handled;
	end size_key_pressed;


	

	
	procedure size_entered (combo_entry : access gtk_entry_record'class) is 
		text : constant string := get_text (combo_entry);
	begin
		--put_line ("size " & text);
		apply_size (text);
	end size_entered;


	

	
	procedure apply_line_width (text : in string) is
		width : type_text_line_width;
	begin
		width := to_distance (text);

		-- CS validate. output error in status bar
		preliminary_text.text.line_width := width;

		et_canvas_board_2.redraw_board;
	end apply_line_width;


	
	
	
	function line_width_key_pressed (
		combo_entry	: access gtk_widget_record'class;
		event		: gdk_event_key) 
		return boolean 
	is
		event_handled : boolean := false;
		
		use gdk.types;
		key : gdk_key_type := event.keyval;

		gentry : gtk_gentry := gtk_gentry (combo_entry);
		text : constant string := get_text (gentry);
	begin
		case key is
			when GDK_ESCAPE =>
				et_canvas_board_2.reset;

			when GDK_TAB => 
				--put_line ("line width via tab " & text);
				apply_line_width (text);

			when others => nulL;
		end case;
		
		return event_handled;
	end line_width_key_pressed;

	

	
	
	procedure line_width_entered (combo_entry : access gtk_entry_record'class) is 
		text : constant string := get_text (combo_entry);
	begin
		--put_line ("line width " & text);
		apply_line_width (text);
	end line_width_entered;



	
	
	procedure apply_rotation (text : in string) is
		rotation : type_rotation_model;
	begin
		rotation := to_rotation (text);
		--put_line (to_string (rotation));
		-- CS validate. output error in status bar

		set (preliminary_text.text.position, rotation);
		--put_line (to_string (preliminary_text.text.position));
		et_canvas_board_2.redraw_board;
	end apply_rotation;



	

	
	function rotation_key_pressed (
		combo_entry	: access gtk_widget_record'class;
		event		: gdk_event_key) 
		return boolean 
	is
		event_handled : boolean := false;
		
		use gdk.types;
		key : gdk_key_type := event.keyval;

		gentry : gtk_gentry := gtk_gentry (combo_entry);
		text : constant string := get_text (gentry);
	begin
		case key is
			when GDK_ESCAPE =>
				et_canvas_board_2.reset;
				
			when GDK_TAB => 
				--put_line ("rotation via tab " & text);
				apply_rotation (text);

			when others => nulL;
		end case;
		
		return event_handled;
	end rotation_key_pressed;


	
	
	
	procedure rotation_entered (combo_entry : access gtk_entry_record'class) is 
		text : constant string := get_text (combo_entry);
	begin
		--put_line ("rotation " & text);
		apply_rotation (text);
	end rotation_entered;



	
	
	procedure button_apply_clicked (button : access gtk_button_record'class) is
		use gtk.text_view;
		use gtk.text_buffer;
		use gtk.text_iter;
		use et_text;
		
		text_buffer : constant gtk_text_buffer := get_buffer (preliminary_text.entry_content);
		lower_bound, upper_bound : gtk_text_iter;
	begin
		--put_line ("button apply clicked");
		get_bounds (text_buffer, lower_bound, upper_bound);
		--put_line ("content: " & get_text (text_buffer, lower_bound, upper_bound));
		preliminary_text.text.content := to_content (get_text (text_buffer, lower_bound, upper_bound));
		-- CS check characters				
	end button_apply_clicked;



	
	

	procedure show_text_properties is
		use gtk.cell_renderer_text;
		use gtk.cell_layout;
		use gtk.list_store;
		use gtk.tree_model;
		use gtk.text_view;

		use et_canvas_board_2.pac_canvas;
		

		box_layer_category, box_face, 
		box_signal_layer, box_content, box_button,
		box_size, box_line_width, box_rotation : gtk_vbox;

		label_layer_category, label_face, 
		label_signal_layer, label_content,
		label_size, label_line_width, label_rotation : gtk_label;
		
		cbox_category, cbox_face, cbox_signal_layer : gtk_combo_box;
		-- Operator can choose between fixed menu entries.
		
		cbox_line_width, cbox_size, cbox_rotation : gtk_combo_box_text;
		-- Operator may enter an additional value in the menu.
		
		button_apply : gtk_button;



		-- These constants define the minimum and maximum of
		-- characters that can be entered in the fields for 
		-- text size and line width:
		text_size_length_min : constant gint := 1;
		text_size_length_max : constant gint := 6; 
		-- CS: adjust if necessary. see parameters 
		-- of et_board_shapes_and_text.pac_text_fab.
		
		line_width_length_min : constant gint := 1;
		line_width_length_max : constant gint := 5;
		-- CS: adjust if necessary. see parameters
		-- of et_board_shapes_and_text.pac_text_fab.
		
		rotation_length_min : constant gint := 1;
		rotation_length_max : constant gint := 5;
		-- CS: adjust if necessary. see et_pcb_coordinates type_rotation_model.
		
		-- The spacing between the boxes:
		spacing : constant natural := 5;


		
		procedure make_combo_category is
			storage_model : gtk_list_store;

			-- An entry consists of just a single column:
			column_0 : constant := 0;

			-- The single column is to contain strings:
			entry_structure : glib.gtype_array := (column_0 => glib.gtype_string);

			iter : gtk_tree_iter;			
			render : gtk_cell_renderer_text;


			-- Collects layer categories and inserts them
			-- in the storage model:
			procedure collect_layer_cats is

				procedure query_category (
					c : in pac_layer_categories.cursor) 
				is 
					use pac_layer_categories;
				begin
					storage_model.append (iter);
					gtk.list_store.set (storage_model, iter, column_0,
						to_string (element (c)));

				end query_category;
				
			begin
				make_affected_layer_categories;				
				layer_categories.iterate (query_category'access);
			end collect_layer_cats;


			
			procedure set_category_used_last is
				c : pac_layer_categories.cursor;
				use pac_layer_categories;
			begin
				-- Map from preliminary_zone.category to index:
				c := find (layer_categories, object_layer_category);
				cbox_category.set_active (gint (to_index (c)));
			end set_category_used_last;


			
			
		begin
			gtk_new_vbox (box_layer_category, homogeneous => false);
			pack_start (box_v4, box_layer_category, padding => guint (spacing));

			gtk_new (label_layer_category, "LAYER CAT");
			pack_start (box_layer_category, label_layer_category, padding => guint (spacing));

			
			-- Create the storage model:
			gtk_new (list_store => storage_model, types => (entry_structure));

			-- Insert the layer categories in the storage model:
			collect_layer_cats;			

			-- Create the combo box:
			gtk.combo_box.gtk_new_with_model (
				combo_box	=> cbox_category,
				model		=> +storage_model); -- ?

			set_category_used_last;

			pack_start (box_layer_category, cbox_category, padding => guint (spacing));
			cbox_category.on_changed (layer_category_changed'access);

			-- The purpose of this stuff is unclear, but it
			-- is required to make the entries visible:
			gtk_new (render);
			pack_start (cbox_category, render, expand => true);
			add_attribute (cbox_category, render, "markup", column_0);
		end make_combo_category;



		
		procedure make_combo_for_face is
			storage_model : gtk_list_store;

			-- An entry consists of just a single column:
			column_0 : constant := 0;

			-- The single column is to contain strings:
			entry_structure : glib.gtype_array := (column_0 => glib.gtype_string);

			iter : gtk_tree_iter;			
			render : gtk_cell_renderer_text;
		begin
			gtk_new_vbox (box_face, homogeneous => false);
			pack_start (box_v4, box_face, padding => guint (spacing));
			
			gtk_new (label_face, "FACE");
			pack_start (box_face, label_face, padding => guint (spacing));

			
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
			cbox_face.set_active (type_face'pos (object_face));


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
			gtk_new_vbox (box_signal_layer, homogeneous => false);
			pack_start (box_v4, box_signal_layer, padding => guint (spacing));
			
			gtk_new (label_signal_layer, "SIGNAL LAYER");
			pack_start (box_signal_layer, label_signal_layer, padding => guint (spacing));

			
			-- Create the storage model:
			gtk_new (list_store => storage_model, types => (entry_structure));

			-- Insert the available signal layers in the storage model:
			for choice in 
				-- The top layer is always available:
				type_signal_layer'first .. 

				-- The deepest available layer depends on the stack configuration:
				get_deepest_conductor_layer (active_module) 
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
			cbox_signal_layer.set_active (gint (object_signal_layer) - 1);
			-- NOTE: The entries are numbered from 0 .. N.


			pack_start (box_signal_layer, cbox_signal_layer, padding => guint (spacing));
			cbox_signal_layer.on_changed (signal_layer_changed'access);

			-- The purpose of this stuff is unclear, but it
			-- is required to make the entries visible:
			gtk_new (render);
			pack_start (cbox_signal_layer, render, expand => true);
			add_attribute (cbox_signal_layer, render, "markup", column_0);

		end make_combo_for_signal_layer;


		
		procedure make_combo_for_size is begin
			gtk_new_vbox (box_size, homogeneous => false);
			pack_start (box_v4, box_size, padding => guint (spacing));
			
			gtk_new (label_size, "SIZE");
			pack_start (box_size, label_size, padding => guint (spacing));

			gtk_new_with_entry (cbox_size);
			pack_start (box_size, cbox_size, padding => guint (spacing));
			gtk_entry (cbox_size.get_child).set_max_length (text_size_length_max);
			gtk_entry (cbox_size.get_child).set_width_chars (text_size_length_min);

			-- Set the text size according to the value used last:
			gtk_entry (cbox_size.get_child).set_text (trim (to_string (preliminary_text.text.size), left));
			
			-- The size is to be accepted by either pressing TAB or by pressing ENTER:
			gtk_entry (cbox_size.get_child).on_key_press_event (size_key_pressed'access);
			gtk_entry (cbox_size.get_child).on_activate (size_entered'access);
		end make_combo_for_size;

		
		
		procedure make_combo_for_line_width is begin
			gtk_new_vbox (box_line_width, homogeneous => false);
			pack_start (box_v4, box_line_width, padding => guint (spacing));

			gtk_new (label_line_width, "LINE WIDTH");
			pack_start (box_line_width, label_line_width, padding => guint (spacing));

			gtk_new_with_entry (cbox_line_width);
			pack_start (box_line_width, cbox_line_width, padding => guint (spacing));
			gtk_entry (cbox_line_width.get_child).set_max_length (line_width_length_max);
			gtk_entry (cbox_line_width.get_child).set_width_chars (line_width_length_min);

			-- Set the line width according to the value used last:
			gtk_entry (cbox_line_width.get_child).set_text (trim (to_string (preliminary_text.text.line_width), left));
			
			-- The width is to be accepted by either pressing TAB or by pressing ENTER:
			gtk_entry (cbox_line_width.get_child).on_key_press_event (line_width_key_pressed'access);
			gtk_entry (cbox_line_width.get_child).on_activate (line_width_entered'access);
		end make_combo_for_line_width;


		
		procedure make_combo_for_rotation is begin
			gtk_new_vbox (box_rotation, homogeneous => false);
			pack_start (box_v4, box_rotation, padding => guint (spacing));

			gtk_new (label_rotation, "ROTATION");
			pack_start (box_rotation, label_rotation, padding => guint (spacing));

			gtk_new_with_entry (cbox_rotation);
			pack_start (box_rotation, cbox_rotation, padding => guint (spacing));
			gtk_entry (cbox_rotation.get_child).set_max_length (rotation_length_max);
			gtk_entry (cbox_rotation.get_child).set_width_chars (rotation_length_min);

			-- Set the text size according to the value used last:
			gtk_entry (cbox_rotation.get_child).set_text (trim (to_string (get_rotation (preliminary_text.text.position)), left));
			
			-- The rotation is to be accepted by either pressing TAB or by pressing ENTER:
			gtk_entry (cbox_rotation.get_child).on_key_press_event (rotation_key_pressed'access);
			gtk_entry (cbox_rotation.get_child).on_activate (rotation_entered'access);
		end make_combo_for_rotation;


		
		procedure make_view_for_content is begin
			gtk_new_vbox (box_content, homogeneous => false);
			pack_start (box_v4, box_content, padding => guint (spacing));

			gtk_new (label_content, "CONTENT");
			pack_start (box_content, label_content, padding => guint (spacing));

			gtk_new (preliminary_text.entry_content);
			preliminary_text.entry_content.set_accepts_tab (false); -- TAB character not allowed
			pack_start (box_content, preliminary_text.entry_content, padding => guint (spacing));
		end make_view_for_content;


		
		procedure make_apply_button is begin
			gtk_new_vbox (box_button, homogeneous => false);
			pack_start (box_v4, box_button, padding => guint (spacing));

			gtk_new (button_apply, "Apply");
			pack_start (box_button, button_apply, padding => guint (spacing));
			button_apply.on_clicked (button_apply_clicked'access);
		end make_apply_button;


		
	begin -- show_text_properties
		-- put_line ("show_text_properties");

		-- Before inserting any widgets, the properties box
		-- must be cleared:
		clear_out_properties_box;

		
		-- Build the elements of the properties bar:
		make_combo_category;
		make_combo_for_face;
		make_combo_for_signal_layer;
		make_combo_for_size;
		make_combo_for_line_width;
		make_combo_for_rotation;
		make_view_for_content;
		make_apply_button;

		-- Redraw the right box of the window:
		box_v0.show_all;  -- CS box_v4 ?
	end show_text_properties;


	
	
	
	
-- PLACE:
	
	procedure place_text (
		point : in type_vector_model) 
	is begin
		move_to (preliminary_text.text.position.place, point);
		
		case object_layer_category is
			when LAYER_CAT_ASSY =>

				et_board_ops.assy_doc.add_text (
					module_cursor 	=> active_module,
					face			=> object_face,
					text			=> preliminary_text.text,
					log_threshold	=> log_threshold + 1);

				

			when LAYER_CAT_SILKSCREEN =>

				et_board_ops.silkscreen.add_text (
					module_cursor 	=> active_module,
					face			=> object_face,
					text			=> preliminary_text.text,
					log_threshold	=> log_threshold + 1);


			when LAYER_CAT_STOPMASK =>
				
				et_board_ops.stopmask.add_text (
					module_cursor 	=> active_module,
					face			=> object_face,
					text			=> preliminary_text.text,
					log_threshold	=> log_threshold + 1);

				
			when LAYER_CAT_CONDUCTOR =>
			
				et_board_ops.conductors.add_text (
					module_cursor 	=> active_module,
					signal_layer	=> object_signal_layer,
					text			=> preliminary_text.text,
					log_threshold	=> log_threshold + 1);

			when others => null; -- CS raise exception ?
		end case;

	end place_text;
	
	
end et_canvas_board_texts;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
