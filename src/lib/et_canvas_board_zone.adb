------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD ZONE                                --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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

with glib.values;

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

with et_board_shapes_and_text;

with et_board_ops;						use et_board_ops;
with et_board_ops.assy_doc;
with et_board_ops.silkscreen;
with et_board_ops.stop_mask;
with et_board_ops.conductors;
with et_board_ops.keepout;
with et_board_ops.stencil;
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


package body et_canvas_board_zone is


	procedure make_affected_layer_categories is
		use pac_affected_layer_categories;
	begin
		affected_layer_categories.clear;
		affected_layer_categories.append (LAYER_CAT_ASSY);
		affected_layer_categories.append (LAYER_CAT_KEEPOUT);
		affected_layer_categories.append (LAYER_CAT_SILKSCREEN);
		affected_layer_categories.append (LAYER_CAT_STOP);
		affected_layer_categories.append (LAYER_CAT_STENCIL);
		affected_layer_categories.append (LAYER_CAT_VIA_RESTRICT);
	end make_affected_layer_categories;

	

	
	
	procedure reset_preliminary_zone is begin
		preliminary_zone.ready := false;
		preliminary_zone.tool := MOUSE;

		-- Clear the content of the properties bar:
		if box_properties.displayed then
			-- put_line ("clear track properties box");
			
			-- Clear out the properties box:
			remove (box_v4, box_layer_category);
			remove (box_v4, box_face);
			remove (box_v4, box_signal_layer);

			-- CS use an iterator to remove widgets like
			-- container.foreach ((element) => container.remove (element));
			--
			-- See <https://stackoverflow.com/questions/36215425/vala-how-do-you-delete-all-the-children-of-a-gtk-container>
			-- See package gtk.container
			
			box_properties.displayed := false;
		end if;
	end reset_preliminary_zone;



	
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

		preliminary_zone.category := to_layer_category (glib.values.get_string (item_text));
		--put_line ("cat " & to_string (preliminary_zone.category));

		-- display the objects in the selected layer category:
		case preliminary_zone.category is
			when LAYER_CAT_CONDUCTOR =>
				-- display the affected conductor layer:
				enable_conductor (preliminary_zone.signal_layer);
  
			when others =>
				null;
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

		preliminary_zone.face := to_face (glib.values.get_string (item_text));
		--put_line ("face " & to_string (preliminary_zone.face));

		et_canvas_board_2.redraw_board;
		
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

		preliminary_zone.signal_layer := to_signal_layer (glib.values.get_string (item_text));
		--put_line ("signal layer " & to_string (preliminary_zone.signal_layer));

		-- Display the affected conductor layers:
		enable_conductor (preliminary_zone.signal_layer);
		enable_via_restrict (preliminary_zone.signal_layer);
		
		et_canvas_board_2.redraw_board;		
	end signal_layer_changed;

	





	procedure show_zone_properties is

		use et_canvas_board_2.pac_canvas;
		
		use gtk.gentry;
		use gtk.cell_renderer_text;
		use gtk.cell_layout;
		use gtk.list_store;
		use gtk.tree_model;


		
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
					c : in pac_affected_layer_categories.cursor) 
				is 
					use pac_affected_layer_categories;
				begin
					storage_model.append (iter);
					gtk.list_store.set (storage_model, iter, column_0,
						to_string (element (c)));

				end query_category;
				
			begin
				make_affected_layer_categories;				
				affected_layer_categories.iterate (query_category'access);
			end collect_layer_cats;


			
			procedure set_category_used_last is
				c : pac_affected_layer_categories.cursor;
				use pac_affected_layer_categories;
			begin
				-- Map from preliminary_zone.category to index:
				c := find (affected_layer_categories, preliminary_zone.category);
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
			cbox_face.set_active (type_face'pos (preliminary_zone.face));


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
			cbox_signal_layer.set_active (gint (preliminary_zone.signal_layer) - 1);
			-- NOTE: The entries are numbered from 0 .. N.


			pack_start (box_signal_layer, cbox_signal_layer, padding => guint (spacing));
			cbox_signal_layer.on_changed (signal_layer_changed'access);

			-- The purpose of this stuff is unclear, but it
			-- is required to make the entries visible:
			gtk_new (render);
			pack_start (cbox_signal_layer, render, expand => true);
			add_attribute (cbox_signal_layer, render, "markup", column_0);

		end make_combo_for_signal_layer;

		
	begin		
		-- If the properties are already displayed, do nothing.
		-- Otherwise show them:
		if not box_properties.displayed then
			--put_line ("build line properties");
			
			box_properties.displayed := true;
		
			-- Build the elements of the properties bar:
			make_combo_category;
			make_combo_for_face;
			make_combo_for_signal_layer;

			-- Redraw the right box of the window:
			box_v0.show_all; -- CS box_v4 ?
		end if;		
	end show_zone_properties;


	

	procedure make_path (
		tool	: in type_tool;
		point	: in type_vector_model)
	is
		PZ : type_preliminary_zone renames preliminary_zone;
		line : type_line;

		
		procedure add_by_category is 
			use et_modes.board;
			use et_undo_redo;
			use et_commit;
			
			use et_board_shapes_and_text;
			use et_board_shapes_and_text.pac_contours;

			-- A temporary contour consisting of a single 
			-- segment:
			c : type_contour;
		begin
			-- Add the line to the temporary contour:
			append_segment (c, to_segment (line));
							
			-- Commit the current state of the design:
			commit (PRE, verb, noun, log_threshold + 1);

			case PZ.category is
				when LAYER_CAT_ASSY =>

					-- Add the temporary contour to the board:
					et_board_ops.assy_doc.draw_zone (
						module_cursor	=> active_module,
						zone			=> (c with null record),
						face			=> PZ.face,
						log_threshold	=> log_threshold);

					
				when LAYER_CAT_SILKSCREEN =>
					
					-- Add the temporary contour to the board:					
					et_board_ops.silkscreen.draw_zone (
						module_cursor	=> active_module,
						zone			=> (c with null record),
						face			=> PZ.face,
						log_threshold	=> log_threshold);


				when LAYER_CAT_STOP =>

					-- Add the temporary contour to the board:
					et_board_ops.stop_mask.draw_zone (
						module_cursor	=> active_module,
						zone			=> (c with null record),
						face			=> PZ.face,
						log_threshold	=> log_threshold);


				when LAYER_CAT_STENCIL =>

					-- Add the temporary contour to the board:
					et_board_ops.stencil.draw_zone (
						module_cursor	=> active_module,
						zone			=> (c with null record),
						face			=> PZ.face,
						log_threshold	=> log_threshold);

					
				when LAYER_CAT_KEEPOUT =>

					-- Add the temporary contour to the board:
					et_board_ops.keepout.draw_zone (
						module_cursor	=> active_module,
						zone			=> (c with null record),
						face			=> PZ.face,
						log_threshold	=> log_threshold);
					
					
				when LAYER_CAT_VIA_RESTRICT =>

					-- Add the temporary contour to the board:
					et_board_ops.via_restrict.draw_zone (
						module_cursor	=> active_module,
						zone			=> (c with to_layers (PZ.signal_layer)),
						log_threshold	=> log_threshold);


				when others => null; -- CS
			end case;

			-- Commit the new state of the design:
			commit (POST, verb, noun, log_threshold + 1);			
		end add_by_category;

		
	begin -- make_path
		-- put_line ("make_path");
		
		-- Set the tool being used for this path so that procedure
		-- draw_path_live (for example in et_canvas_board_2-draw_assy_doc)
		-- knows where to get the end point from.
		PZ.tool := tool;

		-- Initally the preliminary_zone is NOT ready. Nothing will be drawn.
		-- Upon the first calling of this procedure the start point of the
		-- path will be set.
		
		if not PZ.ready then
			-- set start point:
			PZ.path.start_point := point;

			-- Allow drawing of the path:
			preliminary_zone.ready := true;

			set_status (status_start_point & to_string (PZ.path.start_point) & ". " &
				status_press_space & status_set_end_point & status_hint_for_abort);

		else -- preliminary_zone IS ready

			-- Start a new path only if the given point differs from 
			-- the start point of the current path:
			if point /= PZ.path.start_point then

				-- Complete the path by setting its end point.
				-- The the current bend point (if there is one) into account:
				
				if PZ.path.bended = NO then
					PZ.path.end_point := point;

					-- insert a single line:
					line.start_point := PZ.path.start_point;
					line.end_point   := PZ.path.end_point;
					add_by_category;
					
				else
					-- The path is bended. The bend point has been computed
					-- interactively while moving the mouse or the cursor.
					-- See for example procedure draw_path in et_canvas_board_2-draw_assy_doc.

					-- insert first line of the path:
					line.start_point := PZ.path.start_point;
					line.end_point   := PZ.path.bend_point;
					add_by_category;

					
					-- insert second line of the path:
					PZ.path.end_point := point;
					line.start_point := PZ.path.bend_point;
					line.end_point   := PZ.path.end_point;
					add_by_category;
				end if;

				-- Set start point of path so that a new
				-- path can be drawn:
				PZ.path.start_point := point;
				
			else
				reset_preliminary_zone;
			end if;
		end if;			
	end make_path;
		

	
end et_canvas_board_zone;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
