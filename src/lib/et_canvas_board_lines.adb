------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD LINES                               --
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

with glib;								use glib;
with glib.values;
with gtk.box;							use gtk.box;
with gtk.label;							use gtk.label;
with gtk.combo_box;						use gtk.combo_box;
with gtk.combo_box_text;				use gtk.combo_box_text;
with gtk.button;						use gtk.button;


with gdk.types;							use gdk.types;
with gdk.event;							use gdk.event;
with gdk.types.keysyms;					use gdk.types.keysyms;

with gtk.widget;						use gtk.widget;

with gtk.cell_renderer_text;		
with gtk.cell_layout;        		
with gtk.list_store;				
with gtk.tree_model;

with gtk.gentry;						use gtk.gentry;
with gtk.container;						use gtk.container;

with et_generic_module;					use et_generic_module;
with et_canvas_board_2;

with et_board_ops;						use et_board_ops;
with et_board_ops.board_contour;
with et_board_ops.assy_doc;
with et_board_ops.keepout;
with et_board_ops.silkscreen;
with et_board_ops.stopmask;
with et_board_ops.stencil;
with et_board_ops.conductors;
with et_board_ops.route_restrict;
with et_board_ops.via_restrict;
with et_modes.board;

with et_display;						use et_display;
with et_display.board;					use et_display.board;

with et_logging;						use et_logging;
with et_string_processing;				use et_string_processing;
with et_exceptions;						use et_exceptions;

with et_undo_redo;
with et_commit;

with et_canvas_board_preliminary_object;	use et_canvas_board_preliminary_object;


package body et_canvas_board_lines is


	procedure make_affected_layer_categories is
		use pac_layer_categories;
	begin
		layer_categories.clear;
		layer_categories.append (LAYER_CAT_ASSY);
		layer_categories.append (LAYER_CAT_CONDUCTOR);
		layer_categories.append (LAYER_CAT_HOLE);
		layer_categories.append (LAYER_CAT_KEEPOUT);
		layer_categories.append (LAYER_CAT_OUTLINE);
		layer_categories.append (LAYER_CAT_SILKSCREEN);
		layer_categories.append (LAYER_CAT_ROUTE_RESTRICT);
		layer_categories.append (LAYER_CAT_STENCIL);
		layer_categories.append (LAYER_CAT_STOPMASK);
		layer_categories.append (LAYER_CAT_VIA_RESTRICT);
	end make_affected_layer_categories;
	

	

	
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

		object_layer_category := to_layer_category (values.get_string (item_text));
		--put_line ("cat " & to_string (object_layer_category));

		-- Auto-enable the selected layer category:
		case object_layer_category is
			when LAYER_CAT_OUTLINE | LAYER_CAT_HOLE =>
				enable_board_contour;

			when LAYER_CAT_ASSY =>
				enable_assy_doc (object_face);

			when LAYER_CAT_CONDUCTOR =>
				enable_conductor (object_signal_layer);

			when LAYER_CAT_SILKSCREEN =>
				enable_silkscreen (object_face);

			when LAYER_CAT_ROUTE_RESTRICT =>
				enable_route_restrict (object_signal_layer);

			when LAYER_CAT_VIA_RESTRICT =>
				enable_via_restrict (object_signal_layer);
				
			when LAYER_CAT_STENCIL =>
				enable_stencil (object_face);

			when LAYER_CAT_STOPMASK =>
				enable_stopmask (object_face);
				
			when others => null;
		end case;
		
		et_canvas_board_2.redraw_board;
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

			when LAYER_CAT_STENCIL =>
				enable_stencil (object_face);

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

		-- Auto-enable the affected conductor and restrict layers:
		enable_conductor (object_signal_layer);
		enable_via_restrict (object_signal_layer);
		
		et_canvas_board_2.redraw_board;		
	end signal_layer_changed;

	



	
	
	procedure apply_line_width (text : in string) is
		width : type_distance_positive;
	begin
		width := to_distance (text);

		-- CS validate. output error in status bar
		object_linewidth := width;

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


	


	procedure show_line_properties is

		use et_canvas_board_2.pac_canvas;
		
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
		
		line_width_length_min : constant gint := 1;
		line_width_length_max : constant gint := 5;
		
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
			gtk_entry (cbox_line_width.get_child).set_text (trim (to_string (object_linewidth), left));
			
			-- The width is to be accepted by either pressing TAB or by pressing ENTER:
			gtk_entry (cbox_line_width.get_child).on_key_press_event (line_width_key_pressed'access);
			gtk_entry (cbox_line_width.get_child).on_activate (line_width_entered'access);
		end make_combo_for_line_width;


		-- procedure make_apply_button is begin
		-- 	gtk_new_vbox (box_button, homogeneous => false);
		-- 	pack_start (box_v4, box_button, padding => guint (spacing));
  -- 
		-- 	gtk_new (button_apply, "Apply");
		-- 	pack_start (box_button, button_apply, padding => guint (spacing));
		-- 	button_apply.on_clicked (button_apply_clicked'access);
		-- end make_apply_button;
		
	begin
		--put_line ("build line properties");

		-- Before inserting any widgets, the properties box
		-- must be cleared:
		clear_out_properties_box;
		
		-- Build the elements of the properties bar:
		make_combo_category;
		make_combo_for_face;
		make_combo_for_signal_layer;
		make_combo_for_line_width;
		-- make_apply_button;

		-- Redraw the right box of the window:
		box_v0.show_all; -- CS box_v4 ?
	end show_line_properties;





	procedure add_by_category (
		line : in type_line)
	is 
		use et_modes.board;
		use et_undo_redo;
		use et_commit;
		use et_board_coordinates.pac_contours;

		-- A temporary contour consisting of a single segment,
		-- used in case it is about a contour segment:
		c : type_contour; 

		
		-- This procedure adds the given line
		-- to the outer board contour:
		procedure add_to_outer_contour is
			use et_board_ops.board_contour;
		begin
			-- Add the line to the temporary contour:
			append_segment (c, to_segment (line));

			-- Add the temporary contour to the board:
			add_outline (
				module_cursor	=> active_module,
				outline			=> (c with null record),
				log_threshold	=> log_threshold);
			
		end add_to_outer_contour;



		-- This procedure adds the given line
		-- to the holes of the board:
		procedure add_to_holes is
			use et_board_ops.board_contour;
		begin
			-- Add the line to the temporary contour:
			append_segment (c, to_segment (line));

			-- Add the temporary contour to the board:
			add_hole (
				module_cursor	=> active_module,
				hole			=> (c with null record),
				log_threshold	=> log_threshold);
			
		end add_to_holes;


		
		
		-- This procedure adds the given line to either
		-- the lines (which hava a certain width) or to a zone
		-- in the assembly documentation:
		procedure add_to_assy_doc is
			use et_board_ops.assy_doc;
		begin
			-- If the linewidth (given by the operator via the GUI)
			-- is non-zero, then the given line is to be added as
			-- a single line.
			-- If the linewidth is zero, then the line is 
			-- regarded as part of the contour of a zone:
			if object_linewidth > 0.0 then
				add_line (
					module_name	=> pac_generic_modules.key (active_module),
					face		=> object_face,
					line		=> (line with object_linewidth),
					log_threshold	=> log_threshold);

			else				
				-- Add the line to the temporary contour:
				append_segment (c, to_segment (line));

				-- Add the temporary zone to the board:
				add_zone (
					module_cursor	=> active_module,
					zone			=> (c with null record),
					face			=> object_face,
					log_threshold	=> log_threshold);
				
			end if;
		end add_to_assy_doc;



		-- This procedure adds the given line to either
		-- the lines (which hava a certain width) or to a zone
		-- in the silkscreen:
		procedure add_to_silkscreen is
			use et_board_ops.silkscreen;
		begin
			-- If the linewidth (given by the operator via the GUI)
			-- is non-zero, then the given line is to be added as
			-- a single line.
			-- If the linewidth is zero, then the line is 
			-- regarded as part of the contour of a zone:
			if object_linewidth > 0.0 then
				add_line (
					module_name	=> pac_generic_modules.key (active_module),
					face		=> object_face,
					line		=> (line with object_linewidth),
					log_threshold	=> log_threshold);

			else				
				-- Add the line to the temporary contour:
				append_segment (c, to_segment (line));

				-- Add the temporary zone to the board:
				add_zone (
					module_cursor	=> active_module,
					zone			=> (c with null record),
					face			=> object_face,
					log_threshold	=> log_threshold);
				
			end if;
		end add_to_silkscreen;

		

		-- This procedure adds the given line to either
		-- the lines (which hava a certain width) or to a zone
		-- in the stopmask:
		procedure add_to_stopmask is
			use et_board_ops.stopmask;
		begin
			-- If the linewidth (given by the operator via the GUI)
			-- is non-zero, then the given line is to be added as
			-- a single line.
			-- If the linewidth is zero, then the line is 
			-- regarded as part of the contour of a zone:
			if object_linewidth > 0.0 then
				add_line (
					module_name	=> pac_generic_modules.key (active_module),
					face		=> object_face,
					line		=> (line with object_linewidth),
					log_threshold	=> log_threshold);

			else				
				-- Add the line to the temporary contour:
				append_segment (c, to_segment (line));

				-- Add the temporary zone to the board:
				add_zone (
					module_cursor	=> active_module,
					zone			=> (c with null record),
					face			=> object_face,
					log_threshold	=> log_threshold);
				
			end if;
		end add_to_stopmask;

		

		-- This procedure adds the given line to either
		-- the lines (which hava a certain width) or to a zone
		-- in the stencil:
		procedure add_to_stencil is
			use et_board_ops.stencil;
		begin
			-- If the linewidth (given by the operator via the GUI)
			-- is non-zero, then the given line is to be added as
			-- a single line.
			-- If the linewidth is zero, then the line is 
			-- regarded as part of the contour of a zone:
			if object_linewidth > 0.0 then
				add_line (
					module_name	=> pac_generic_modules.key (active_module),
					face		=> object_face,
					line		=> (line with object_linewidth),
					log_threshold	=> log_threshold);

			else				
				-- Add the line to the temporary contour:
				append_segment (c, to_segment (line));

				-- Add the temporary zone to the board:
				draw_zone (
					module_cursor	=> active_module,
					zone			=> (c with null record),
					face			=> object_face,
					log_threshold	=> log_threshold);
				
			end if;
		end add_to_stencil;


		-- This procedure adds the given line to 
		-- the route restrict layers:
		procedure add_to_route_restrict is
			use et_board_ops.route_restrict;
		begin
			draw_route_restrict_line (
				module_name	=> pac_generic_modules.key (active_module),
				line		=> (line with to_layers (object_signal_layer)),
				log_threshold	=> log_threshold);

				-- CS: Currently only a single signal layer can be assigned
				-- to the line. Multi-layer assignment is possible via
				-- commandline only.
		end add_to_route_restrict;
		

		
		-- This procedure adds the given line to either
		-- the lines (which hava a certain width) or to a zone
		-- in a via restrict layer:
		procedure add_to_via_restrict is
			use et_board_ops.via_restrict;
		begin
			-- Add the line to the temporary contour:
			append_segment (c, to_segment (line));

			-- Add the temporary contour to the board:
			draw_zone (
				module_cursor	=> active_module,
				zone			=> (c with to_layers (object_signal_layer)),
				log_threshold	=> log_threshold);

		end add_to_via_restrict;


		-- This procedure adds the given line
		-- to the keepout layer:
		procedure add_to_keepout is
			use et_board_ops.keepout;
		begin
			-- Add the line to the temporary contour:
			append_segment (c, to_segment (line));

			-- Add the temporary contour to the board:
			add_zone (
				module_cursor	=> active_module,
				zone			=> (c with null record),
				face			=> object_face,
				log_threshold	=> log_threshold);
			
		end add_to_keepout;


		-- This procedure adds the given line to 
		-- a conductor layer:
		procedure add_to_conductors is
			use et_board_ops.conductors;
		begin
			-- Because we do not pass a net name, this is going
			-- to be a freetrack:
			add_line (
				module_name	=> pac_generic_modules.key (active_module),
				line		=> (line with object_linewidth, object_signal_layer),
				log_threshold	=> log_threshold);
		end add_to_conductors;

		
		
	begin
		-- Commit the current state of the design:
		commit (PRE, verb, noun, log_threshold + 1);

		case object_layer_category is
			when LAYER_CAT_OUTLINE =>
				add_to_outer_contour;

			when LAYER_CAT_HOLE =>
				add_to_holes;
				
			when LAYER_CAT_ASSY =>
				add_to_assy_doc;
				
			when LAYER_CAT_CONDUCTOR =>
				add_to_conductors;	
				
			when LAYER_CAT_SILKSCREEN =>
				add_to_silkscreen;
				
			when LAYER_CAT_ROUTE_RESTRICT =>
				add_to_route_restrict;

			when LAYER_CAT_VIA_RESTRICT =>
				add_to_via_restrict;
				
			when LAYER_CAT_STENCIL =>
				add_to_stencil;				

			when LAYER_CAT_KEEPOUT =>
				add_to_keepout;				
				
			when LAYER_CAT_STOPMASK =>
				add_to_stopmask;

			when others =>
				null; -- CS
		end case;

		-- Commit the new state of the design:
		commit (POST, verb, noun, log_threshold + 1);			
	end add_by_category;
		

	
end et_canvas_board_lines;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
