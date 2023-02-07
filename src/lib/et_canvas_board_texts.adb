------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD TEXTS                               --
--                                                                          --
--                               B o d y                                    --
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

with ada.text_io;					use ada.text_io;
with ada.strings;					use ada.strings;
with ada.strings.fixed; 			use ada.strings.fixed;
--with ada.characters;				use ada.characters;
--with ada.characters.handling;		use ada.characters.handling;

with glib;
with glib.values;

with gdk.types;						use gdk.types;
with gdk.event;						use gdk.event;
with gdk.types.keysyms;				use gdk.types.keysyms;

with gtk.window;					use gtk.window;
with gtk.widget;					use gtk.widget;

with gtk.combo_box;					use gtk.combo_box;
with gtk.cell_renderer_text;		
with gtk.cell_layout;        		
with gtk.list_store;				
with gtk.tree_model;

with gtk.combo_box_text;			use gtk.combo_box_text;
with gtk.label;
with gtk.gentry;					use gtk.gentry;
with gtk.container;					use gtk.container;
with gtk.button;					use gtk.button;
with gtk.text_buffer;
with gtk.text_iter;
--with gtk.menu;
--with gtk.menu_item;
--with gtk.menu_shell;

with et_project.modules;				use et_project.modules;
with et_canvas_board;					--use et_canvas_board;
use et_canvas_board.pac_canvas;

with et_board_ops;						use et_board_ops;
with et_board_ops.assy_doc;
with et_board_ops.silkscreen;
with et_board_ops.stop_mask;
with et_modes.board;
with et_logging;						use et_logging;
with et_string_processing;				use et_string_processing;
with et_exceptions;						use et_exceptions;


package body et_canvas_board_texts is

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

		preliminary_text.category := to_layer_category (glib.values.get_string (item_text));
		--put_line ("cat " & to_string (preliminary_text.category));

		et_canvas_board.redraw_board;
		
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

		preliminary_text.face := to_face (glib.values.get_string (item_text));
		--put_line ("face " & to_string (preliminary_text.face));

		et_canvas_board.redraw_board;
		
		-- CS display layer ?
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

		preliminary_text.signal_layer := to_signal_layer (glib.values.get_string (item_text));
		--put_line ("signal layer " & to_string (preliminary_text.signal_layer));

		et_canvas_board.redraw_board;
		
		-- CS display layer ?
	end signal_layer_changed;

	
	procedure apply_size (text : in string) is
		size : type_text_size;
	begin
		size := to_distance (text);

		-- CS validate. output error in status bar
		preliminary_text.text.size := size;

		et_canvas_board.redraw_board;
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
				reset_preliminary_text;
			
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

		et_canvas_board.redraw_board;
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
				reset_preliminary_text;

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
		rotation : type_rotation;
	begin
		rotation := to_rotation (text);
		--put_line (to_string (rotation));
		-- CS validate. output error in status bar

		set (preliminary_text.text.position, rotation);
		--put_line (to_string (preliminary_text.text.position));
		et_canvas_board.redraw_board;
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
				reset_preliminary_text;
				
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

		-- CS check length and characters
		
		if not is_empty (preliminary_text.text.content) then
			--put_line ("content: " & enclose_in_quotes (to_string (preliminary_text.text.content)));
			preliminary_text.ready := true;
			canvas.grab_focus;
		end if;
		
	end button_apply_clicked;



	procedure reset_preliminary_text is begin
		preliminary_text.ready := false;
		preliminary_text.tool := MOUSE;
		clear_proposed_texts;

		-- Remove the text properties bar from the window:
		if box_properties.displayed then
			remove (box_right, box_properties.box_main);
			box_properties.displayed := false;
		end if;
	end reset_preliminary_text;

	
	procedure remove_text_properties is 
		use et_modes.board;
	begin
		if verb /= VERB_PLACE then
			reset_preliminary_text;
		end if;
	end remove_text_properties;		



	procedure show_text_properties is
		use glib;

		use gtk.window;
		use gtk.box;
		use gtk.label;
		use gtk.gentry;
		use gtk.cell_renderer_text;
		use gtk.cell_layout;
		use gtk.list_store;
		use gtk.tree_model;
		use gtk.text_view;

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
		-- CS: adjust if necessary. see et_pcb_coordinates type_rotation.
		
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
		begin
			gtk_new_vbox (box_layer_category, homogeneous => false);
			pack_start (box_properties.box_main, box_layer_category, padding => guint (spacing));

			gtk_new (label_layer_category, "LAYER CAT");
			pack_start (box_layer_category, label_layer_category, padding => guint (spacing));

			
			-- Create the storage model:
			gtk_new (list_store => storage_model, types => (entry_structure));

			-- Insert the layer categories in the storage model:
			for choice in 0 .. type_text_layer'pos (type_text_layer'last) loop
				storage_model.append (iter);
				gtk.list_store.set (storage_model, iter, column_0,
					to_string (type_layer_category'val (choice)));
			end loop;

			-- Create the combo box:
			gtk.combo_box.gtk_new_with_model (
				combo_box	=> cbox_category,
				model		=> +storage_model); -- ?

			-- Set the category used last:
			cbox_category.set_active (type_layer_category'pos (preliminary_text.category));


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
			pack_start (box_properties.box_main, box_face, padding => guint (spacing));
			
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
			cbox_face.set_active (type_face'pos (preliminary_text.face));


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
			pack_start (box_properties.box_main, box_signal_layer, padding => guint (spacing));
			
			gtk_new (label_signal_layer, "SIGNAL LAYER");
			pack_start (box_signal_layer, label_signal_layer, padding => guint (spacing));

			
			-- Create the storage model:
			gtk_new (list_store => storage_model, types => (entry_structure));

			-- Insert the available signal layers in the storage model:
			for choice in 
				-- The top layer is always available:
				type_signal_layer'first .. 

				-- The deepest available layer depends on the stack configuration:
				deepest_conductor_layer (current_active_module) 
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
			cbox_signal_layer.set_active (gint (preliminary_text.signal_layer) - 1);
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
			pack_start (box_properties.box_main, box_size, padding => guint (spacing));
			
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
			pack_start (box_properties.box_main, box_line_width, padding => guint (spacing));

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
			pack_start (box_properties.box_main, box_rotation, padding => guint (spacing));

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
			pack_start (box_properties.box_main, box_content, padding => guint (spacing));

			gtk_new (label_content, "CONTENT");
			pack_start (box_content, label_content, padding => guint (spacing));

			gtk_new (preliminary_text.entry_content);
			preliminary_text.entry_content.set_accepts_tab (false); -- TAB character not allowed
			pack_start (box_content, preliminary_text.entry_content, padding => guint (spacing));
		end make_view_for_content;

		procedure make_apply_button is begin
			gtk_new_vbox (box_button, homogeneous => false);
			pack_start (box_properties.box_main, box_button, padding => guint (spacing));

			gtk_new (button_apply, "Apply");
			pack_start (box_button, button_apply, padding => guint (spacing));
			button_apply.on_clicked (button_apply_clicked'access);
		end make_apply_button;
		
	begin -- show_text_properties
		
		-- If the box is already shown, do nothing.
		-- Otherwise build it:
		if not box_properties.displayed then
			box_properties.displayed := true;
		
			gtk_new_hbox (box_properties.box_main);
			pack_start (et_canvas_board.pac_canvas.box_right, box_properties.box_main,
						expand	=> false);

			-- The properties bar is to be displayed in the right box
			-- below the console:
			reorder_child (box_right, box_properties.box_main, 1);

			-- build the elements of the properties bar:
			make_combo_category;
			make_combo_for_face;
			make_combo_for_signal_layer;
			make_combo_for_size;
			make_combo_for_line_width;
			make_combo_for_rotation;
			make_view_for_content;
			make_apply_button;

			-- Redraw the right box of the window:
			box_right.show_all;
		end if;
		
	end show_text_properties;



	use pac_doc_texts;
	use pac_silk_texts;
	use pac_stop_texts;
	use pac_conductor_texts;

	

	function is_selected (
		text_cursor	: in pac_doc_texts.cursor;
		face		: in type_face)
		return boolean
	is begin
		-- If there is no proposed assy doc text at all, then
		-- there is nothing to do:
		case face is
			when TOP =>
				if proposed_texts.assy_doc.top.is_empty then
					return false;
				else
					-- If there is no selected text, then there is nothing to do:
					if selected_text.assy_doc.top /= pac_doc_texts.no_element then
						if element (text_cursor) = element (selected_text.assy_doc.top) then
							return true;
						else
							return false;
						end if;
					else
						return false;
					end if;
				end if;			

			when BOTTOM =>
				if proposed_texts.assy_doc.bottom.is_empty then
					return false;
				else
					-- If there is no selected text, then there is nothing to do:
					if selected_text.assy_doc.bottom /= pac_doc_texts.no_element then
						if element (text_cursor) = element (selected_text.assy_doc.bottom) then
							return true;
						else
							return false;
						end if;
					else
						return false;
					end if;
				end if;			

		end case;
	end is_selected;
	

	function is_selected (
		text_cursor	: in pac_silk_texts.cursor;
		face		: in type_face)
		return boolean
	is begin
		case face is
			when TOP =>
				-- If there is no proposed silkscreen text at all, then
				-- there is nothing to do:
				if proposed_texts.silkscreen.top.is_empty then
					return false;
				else
					-- If there is no selected text, then there is nothing to do:
					if selected_text.silkscreen.top /= pac_silk_texts.no_element then
						if element (text_cursor) = element (selected_text.silkscreen.top) then
							return true;
						else
							return false;
						end if;
					else
						return false;
					end if;
				end if;			

			when BOTTOM =>
				-- If there is no proposed silkscreen text at all, then
				-- there is nothing to do:
				if proposed_texts.silkscreen.bottom.is_empty then
					return false;
				else
					-- If there is no selected text, then there is nothing to do:
					if selected_text.silkscreen.bottom /= pac_silk_texts.no_element then
						if element (text_cursor) = element (selected_text.silkscreen.bottom) then
							return true;
						else
							return false;
						end if;
					else
						return false;
					end if;
				end if;			

		end case;
	end is_selected;


	function is_selected (
		text_cursor	: in pac_stop_texts.cursor;
		face		: in type_face)
		return boolean
	is begin
		case face is
			when TOP =>
				-- If there is no proposed stop_mask text at all, then
				-- there is nothing to do:
				if proposed_texts.stop_mask.top.is_empty then
					return false;
				else
					-- If there is no selected text, then there is nothing to do:
					if selected_text.stop_mask.top /= pac_stop_texts.no_element then
						if element (text_cursor) = element (selected_text.stop_mask.top) then
							return true;
						else
							return false;
						end if;
					else
						return false;
					end if;
				end if;			

			when BOTTOM =>
				-- If there is no proposed stop_mask text at all, then
				-- there is nothing to do:
				if proposed_texts.stop_mask.bottom.is_empty then
					return false;
				else
					-- If there is no selected text, then there is nothing to do:
					if selected_text.stop_mask.bottom /= pac_stop_texts.no_element then
						if element (text_cursor) = element (selected_text.stop_mask.bottom) then
							return true;
						else
							return false;
						end if;
					else
						return false;
					end if;
				end if;			
		end case;
	end is_selected;


	function is_selected (
		text_cursor	: in pac_conductor_texts.cursor)
		return boolean
	is begin
		-- If there is no proposed conductor text at all, then
		-- there is nothing to do:
		if proposed_texts.conductors.is_empty then
			return false;
		else
			-- If there is no selected text, then there is nothing to do:
			if selected_text.conductors /= pac_conductor_texts.no_element then
				if element (text_cursor) = element (selected_text.conductors) then
					return true;
				else
					return false;
				end if;
			else
				return false;
			end if;
		end if;			
	end is_selected;


	
	
	procedure clear_proposed_texts is begin
		proposed_texts := (others => <>);
		selected_text := (others => <>);
	end clear_proposed_texts;


	
	procedure select_text is 
		end_reached : boolean := false;
		position : type_position;
	begin
		-- ASSY DOC
		if next (selected_text.assy_doc.top) /= pac_doc_texts.no_element then
			next (selected_text.assy_doc.top);
			position := get_position (element (selected_text.assy_doc.top));
			end_reached := false;
		else
			-- selected_text.assy_doc := pac_doc_texts.no_element;
			end_reached := true;
		end if;

		-- if end_reached and next (selected_text.assy_doc.bottom) /= pac_doc_texts.no_element then
		-- 	next (selected_text.assy_doc.bottom);
		-- 	end_reached := false;
		-- else
		-- 	-- selected_text.assy_doc := pac_doc_texts.no_element;
		-- 	end_reached := true;
		-- end if;
  -- 
  -- 
		-- -- SILKSCREEN
		-- if end_reached and next (selected_text.silkscreen.top) /= pac_silk_texts.no_element then
		-- 	next (selected_text.silkscreen.top);
		-- 	end_reached := false;
		-- else
		-- 	-- selected_text.silkscreen := pac_silk_texts.no_element;
		-- 	end_reached := true;
		-- end if;
  -- 
		-- if end_reached and next (selected_text.silkscreen.bottom) /= pac_silk_texts.no_element then
		-- 	next (selected_text.silkscreen.bottom);
		-- 	end_reached := false;
		-- else
		-- 	-- selected_text.silkscreen := pac_silk_texts.no_element;
		-- 	end_reached := true;
		-- end if;
  -- 
  -- 
		-- -- STOP MASK
		-- if end_reached and next (selected_text.stop_mask.top) /= pac_stop_texts.no_element then
		-- 	next (selected_text.stop_mask.top);
		-- 	end_reached := false;
		-- else
		-- 	-- selected_text.stop_mask := pac_stop_texts.no_element;
		-- 	end_reached := true;
		-- end if;
  -- 
		-- if end_reached and next (selected_text.stop_mask.bottom) /= pac_stop_texts.no_element then
		-- 	next (selected_text.stop_mask.bottom);
		-- 	end_reached := false;
		-- else
		-- 	-- selected_text.stop_mask := pac_stop_texts.no_element;
		-- 	end_reached := true;
		-- end if;
  -- 
  -- 
		-- -- CONDUCTORS
		-- if end_reached and next (selected_text.conductors) /= pac_conductor_texts.no_element then
		-- 	next (selected_text.conductors);
		-- 	end_reached := false;
		-- else			
		-- 	-- selected_text.conductors := pac_conductor_texts.no_element;
		-- 	end_reached := true;
		-- end if;
  -- 
		if end_reached then
			null;
			selected_text.assy_doc.top := proposed_texts.assy_doc.top.first;
			position := get_position (element (selected_text.assy_doc.top));
		end if;
  -- 
		-- show the selected text in the status bar
		set_status ("selected text " & to_string (position)
			& ". " & status_next_object_clarification);

	end select_text;
	

	
	function get_number_of_proposed_texts
		return count_type
	is begin
		return 
			  proposed_texts.assy_doc.top.length
			+ proposed_texts.assy_doc.bottom.length
			  
			+ proposed_texts.silkscreen.top.length
			+ proposed_texts.silkscreen.bottom.length
			
			+ proposed_texts.stop_mask.top.length
			+ proposed_texts.stop_mask.bottom.length
			
			+ proposed_texts.conductors.length;
	end get_number_of_proposed_texts;


	
	function get_first_proposed
		return type_selected_text
	is
		result : type_selected_text;
	begin
		-- ASSY DOC
		if not proposed_texts.assy_doc.top.is_empty then
			result.assy_doc.top := proposed_texts.assy_doc.top.first;
			return result;
		end if;

		if not proposed_texts.assy_doc.bottom.is_empty then
			result.assy_doc.bottom := proposed_texts.assy_doc.bottom.first;
			return result;
		end if;

		-- SILKSCREEN
		if not proposed_texts.silkscreen.top.is_empty then
			result.silkscreen.top := proposed_texts.silkscreen.top.first;
			return result;
		end if;

		if not proposed_texts.silkscreen.bottom.is_empty then
			result.silkscreen.bottom := proposed_texts.silkscreen.bottom.first;
			return result;
		end if;

		-- STOP MASK
		if not proposed_texts.stop_mask.top.is_empty then
			result.stop_mask.top := proposed_texts.stop_mask.top.first;
			return result;
		end if;

		if not proposed_texts.stop_mask.bottom.is_empty then
			result.stop_mask.bottom := proposed_texts.stop_mask.bottom.first;
			return result;
		end if;

		-- CONDUCTORS
		if not proposed_texts.conductors.is_empty then
			result.conductors := proposed_texts.conductors.first;
			return result;
		end if;
		
		return result;
	end get_first_proposed;

	
	
	procedure find_texts (
		point : in type_point)
	is 
		use et_board_ops.assy_doc;
		use et_board_ops.silkscreen;
		use et_board_ops.stop_mask;
	begin
		log (text => "locating texts ...", level => log_threshold);
		log_indentation_up;

		-- Collect all texts in the vicinity of the given point:
		proposed_texts.assy_doc.top   	:= get_texts (current_active_module, TOP, point, catch_zone_default, log_threshold + 1);
		proposed_texts.assy_doc.bottom  := get_texts (current_active_module, BOTTOM, point, catch_zone_default, log_threshold + 1);
		
		proposed_texts.silkscreen.top		:= get_texts (current_active_module, TOP, point, catch_zone_default, log_threshold + 1);
		proposed_texts.silkscreen.bottom	:= get_texts (current_active_module, BOTTOM, point, catch_zone_default, log_threshold + 1);
		
		proposed_texts.stop_mask.top	:= get_texts (current_active_module, TOP, point, catch_zone_default, log_threshold + 1);
		proposed_texts.stop_mask.bottom	:= get_texts (current_active_module, BOTTOM, point, catch_zone_default, log_threshold + 1);
		-- CS conductors
		
		--put_line (count_type'image (proposed_vias.length));
		
		-- evaluate the number of vias found here:
		case get_number_of_proposed_texts is
			when 0 =>
				reset_request_clarification;
				reset_preliminary_text;
				
			when 1 =>
				preliminary_text.ready := true;
				selected_text := get_first_proposed;
				reset_request_clarification;
				
			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;

				-- preselect the text
				selected_text := get_first_proposed;
		end case;
		
		log_indentation_down;
	end find_texts;


	
-- PLACE:
	
	procedure place_text (
		point : in type_point) 
	is begin
		if preliminary_text.ready then
			move_to (preliminary_text.text.position.place, point);
			
			case preliminary_text.category is
				when LAYER_CAT_SILKSCREEN .. LAYER_CAT_STOP =>

					place_text_in_non_conductor_layer (
						module_cursor 	=> current_active_module,
						layer_category	=> preliminary_text.category,
						face			=> preliminary_text.face,
						text			=> preliminary_text.text,
						log_threshold	=> log_threshold + 1);

				
				when LAYER_CAT_CONDUCTOR =>
				
					place_text_in_conductor_layer (
						module_cursor 	=> current_active_module,
						signal_layer	=> preliminary_text.signal_layer,
						text			=> preliminary_text.text,
						log_threshold	=> log_threshold + 1);

			end case;
		end if;	
	end place_text;



	
-- MOVE:

	procedure move_text (
		tool	: in type_tool;
		point	: in type_point)
	is 

		-- Assigns the final position after the move to the selected text.
		-- Resets variable preliminary_text:
		procedure finalize is 

		begin
			log (text => "finalizing move ...", level => log_threshold);
			log_indentation_up;
-- 
-- 			if selected_via /= pac_proposed_vias.no_element then
-- 
-- 				move_via (
-- 					module_cursor	=> current_active_module,
-- 					via				=> element (selected_via),
-- 					coordinates		=> ABSOLUTE,
-- 					point			=> point,
-- 					log_threshold	=> log_threshold);
-- 				
-- 			else
-- 				log (text => "nothing to do", level => log_threshold);
-- 			end if;
-- 				
			log_indentation_down;			
			set_status (status_move_text);
			reset_preliminary_text;
		end finalize;


	begin
		-- Initially the preliminary_text is not ready.
		if not preliminary_text.ready then

			-- Set the tool being used:
			preliminary_text.tool := tool;
			
			if not clarification_pending then
				-- Locate all texts in the vicinity of the given point:
				find_texts (point);
				
				-- NOTE: If many texts have been found, then
				-- clarification is now pending.

				-- If find_texts has found only one text
				-- then the flag preliminary_text.read is set true.

			else
				-- Here the clarification procedure ends.
				-- A text has been selected (indicated by select_text)
				-- via procedure select_text.
				-- By setting preliminary_text.ready, the selected
				-- text will be drawn at the tool position
				-- when texts are drawn on the canvas.
				-- Furtheron, on the next call of this procedure
				-- the selected text will be assigned its final position.
				preliminary_text.ready := true;
				reset_request_clarification;
			end if;
			
		else
			-- Finally move the selected text:
			finalize;
		end if;
	end move_text;

	
	
end et_canvas_board_texts;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
