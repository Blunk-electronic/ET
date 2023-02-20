------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD LINES                               --
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

with et_project.modules;				use et_project.modules;
with et_canvas_board;

with et_board_ops;						use et_board_ops;
with et_board_ops.assy_doc;
with et_board_ops.silkscreen;
with et_board_ops.stop_mask;
with et_board_ops.conductors;
with et_modes.board;
with et_logging;						use et_logging;
with et_string_processing;				use et_string_processing;
with et_exceptions;						use et_exceptions;


package body et_canvas_board_lines is

	use et_canvas_board.pac_canvas;


	procedure reset_preliminary_line is begin
		preliminary_line.ready := false;
		-- preliminary_line.complete := false;
		preliminary_line.tool := MOUSE;
		-- preliminary_line.line := (others => <>);

		-- Remove the text properties bar from the window:
		if box_properties.displayed then
			remove (box_right, box_properties.box_main);
			box_properties.displayed := false;
		end if;
	end reset_preliminary_line;


	procedure reset_path is begin
		preliminary_line.path := (
			bend_style	=> preliminary_line.path.bend_style, -- no change
			others 		=> <>);
	end reset_path;

	
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

		preliminary_line.category := to_layer_category (glib.values.get_string (item_text));
		--put_line ("cat " & to_string (preliminary_line.category));

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

		preliminary_line.face := to_face (glib.values.get_string (item_text));
		--put_line ("face " & to_string (preliminary_line.face));

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

		preliminary_line.signal_layer := to_signal_layer (glib.values.get_string (item_text));
		--put_line ("signal layer " & to_string (preliminary_line.signal_layer));

		et_canvas_board.redraw_board;
		
		-- CS display layer ?
	end signal_layer_changed;

	



	
	
	procedure apply_line_width (text : in string) is
		width : type_distance_positive;
	begin
		width := to_distance (text);

		-- CS validate. output error in status bar
		preliminary_line.width := width;

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
				reset_preliminary_line;

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




	procedure show_line_properties is
		use glib;

		use gtk.window;
		use gtk.box;
		use gtk.label;
		use gtk.gentry;
		use gtk.cell_renderer_text;
		use gtk.cell_layout;
		use gtk.list_store;
		use gtk.tree_model;

		box_layer_category, box_face, 
		box_signal_layer, --box_button,
		box_line_width : gtk_vbox;
		
		label_layer_category, label_face, 
		label_signal_layer, label_line_width : gtk_label;
		
		cbox_category, cbox_face, cbox_signal_layer : gtk_combo_box;
		-- Operator can choose between fixed menu entries.
		
		cbox_line_width : gtk_combo_box_text;
		-- Operator may enter an additional value in the menu.
		
		-- button_apply : gtk_button;

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
			cbox_category.set_active (type_layer_category'pos (preliminary_line.category));


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
			cbox_face.set_active (type_face'pos (preliminary_line.face));


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
			cbox_signal_layer.set_active (gint (preliminary_line.signal_layer) - 1);
			-- NOTE: The entries are numbered from 0 .. N.


			pack_start (box_signal_layer, cbox_signal_layer, padding => guint (spacing));
			cbox_signal_layer.on_changed (signal_layer_changed'access);

			-- The purpose of this stuff is unclear, but it
			-- is required to make the entries visible:
			gtk_new (render);
			pack_start (cbox_signal_layer, render, expand => true);
			add_attribute (cbox_signal_layer, render, "markup", column_0);

		end make_combo_for_signal_layer;


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
			gtk_entry (cbox_line_width.get_child).set_text (trim (to_string (preliminary_line.width), left));
			
			-- The width is to be accepted by either pressing TAB or by pressing ENTER:
			gtk_entry (cbox_line_width.get_child).on_key_press_event (line_width_key_pressed'access);
			gtk_entry (cbox_line_width.get_child).on_activate (line_width_entered'access);
		end make_combo_for_line_width;


		-- procedure make_apply_button is begin
		-- 	gtk_new_vbox (box_button, homogeneous => false);
		-- 	pack_start (box_properties.box_main, box_button, padding => guint (spacing));
  -- 
		-- 	gtk_new (button_apply, "Apply");
		-- 	pack_start (box_button, button_apply, padding => guint (spacing));
		-- 	button_apply.on_clicked (button_apply_clicked'access);
		-- end make_apply_button;
		
	begin -- show_line_properties
		
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
			make_combo_for_line_width;
			-- make_apply_button;

			-- Redraw the right box of the window:
			box_right.show_all;
		end if;
		
	end show_line_properties;



	procedure make_line (
		tool	: in type_tool;
		point	: in type_point)
	is
		PL : type_preliminary_line renames preliminary_line;
		line : type_line;

		procedure add_by_category is begin
			case PL.category is
				when LAYER_CAT_ASSY =>
					
					et_board_ops.assy_doc.draw_line (
						module_name	=> pac_generic_modules.key (current_active_module),
						face		=> PL.face,
						line		=> (line with PL.width),
						log_threshold	=> log_threshold);

				when others =>
					null;
			end case;

		end add_by_category;
		
	begin
		-- case preliminary_line.counter is
		-- 	when 0 =>
		-- 		preliminary_line.line.start_point := point;
		-- 		preliminary_line.ready := true;
  -- 
		-- 	when 1 =>
		-- 		preliminary_line.line.end_point := point;
		-- 		preliminary_line.complete := true;
		-- 		preliminary_line.ready := false;
		-- 		preliminary_line.counter := 0;
  -- 
		-- 	when others =>
		-- 		raise constraint_error;  -- CS should never happen
		-- end case;
  -- 
		-- preliminary_line.counter := preliminary_line.counter + 1;

		PL.tool := tool;


		if not PL.ready then
			PL.path.start_point := point;
			preliminary_line.ready := true;

			set_status (status_start_point & to_string (PL.path.start_point) & ". " &
				status_press_space & status_set_end_point & status_hint_for_abort);

		else
			if PL.path.bended = NO then
				PL.path.end_point := point;

				-- CS insert a single line
				line.start_point := PL.path.start_point;
				line.end_point   := PL.path.end_point;
				add_by_category;
				
			else
				-- path is bended

				-- insert first line:
				line.start_point := PL.path.start_point;
				line.end_point   := PL.path.bend_point;
				add_by_category;

				
				-- insert second line:
				PL.path.end_point := point;
				line.start_point := PL.path.bend_point;
				line.end_point   := PL.path.end_point;
				add_by_category;

			end if;

			reset_preliminary_line;
		end if;
			
	end make_line;
		

	
end et_canvas_board_lines;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
