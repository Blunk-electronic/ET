------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD VIAS                                --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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

with glib;							use glib;
with glib.values;

with gdk.types;						use gdk.types;
with gdk.event;						use gdk.event;
with gdk.types.keysyms;				use gdk.types.keysyms;


with gtk.widget;					use gtk.widget;
with gtk.box;

with gtk.combo_box;					use gtk.combo_box;
with gtk.cell_renderer_text;		
with gtk.cell_layout;        		
with gtk.list_store;				use gtk.list_store;
with gtk.tree_model;				use gtk.tree_model;

with gtk.combo_box_text;			use gtk.combo_box_text;
with gtk.label;
with gtk.gentry;					use gtk.gentry;
with gtk.container;					use gtk.container;
with gtk.text_buffer;
with gtk.text_iter;

with et_canvas_board;
use et_canvas_board.pac_canvas;

with et_modes.board;

package body et_canvas_board_vias is

-- CATEGORY
	procedure category_changed (combo : access gtk_combo_box_record'class) is
		-- Get the model and active iter from the combo box:
		model : constant gtk_tree_model := combo.get_model;
		iter : constant gtk_tree_iter := combo.get_active_iter;

		item_text : glib.values.gvalue;
	begin
		-- Get the actual text of the entry (column is 0):
		gtk.tree_model.get_value (model, iter, 0, item_text);

		via_place.category := to_via_category (glib.values.get_string (item_text));
		--put_line ("cat " & to_string (text_place.category));

		et_canvas_board.redraw_board;
		
		-- CS display layer ?
	end category_changed;


-- DESTINATION LAYER (FOR BLIND VIAS ONLY)
	procedure destination_changed (combo : access gtk_combo_box_record'class) is
		-- Get the model and active iter from the combo box:
		model : constant gtk_tree_model := combo.get_model;
		iter : constant gtk_tree_iter := combo.get_active_iter;

		item_text : glib.values.gvalue;
	begin
		-- Get the actual text of the entry (column is 0):
		gtk.tree_model.get_value (model, iter, 0, item_text);

		via_place.destination_blind := to_signal_layer (glib.values.get_string (item_text));
		--put_line ("signal layer " & to_string (via_place.destination_blind));

		et_canvas_board.redraw_board;
		
		-- CS display layer ?
	end destination_changed;


-- BURIED VIA
	procedure upper_layer_changed (combo : access gtk_combo_box_record'class) is
		-- Get the model and active iter from the combo box:
		model : constant gtk_tree_model := combo.get_model;
		iter : constant gtk_tree_iter := combo.get_active_iter;

		item_text : glib.values.gvalue;
	begin
		-- Get the actual text of the entry (column is 0):
		gtk.tree_model.get_value (model, iter, 0, item_text);

		via_place.layers_buried.upper := to_signal_layer (glib.values.get_string (item_text));
		--put_line ("signal layer " & to_string (via_place.destination_blind));

		et_canvas_board.redraw_board;
		
		-- CS display layer ?
	end upper_layer_changed;

	procedure lower_layer_changed (combo : access gtk_combo_box_record'class) is
		-- Get the model and active iter from the combo box:
		model : constant gtk_tree_model := combo.get_model;
		iter : constant gtk_tree_iter := combo.get_active_iter;

		item_text : glib.values.gvalue;
	begin
		-- Get the actual text of the entry (column is 0):
		gtk.tree_model.get_value (model, iter, 0, item_text);

		via_place.layers_buried.lower := to_signal_layer (glib.values.get_string (item_text));
		--put_line ("signal layer " & to_string (via_place.destination_blind));

		et_canvas_board.redraw_board;
		
		-- CS display layer ?
	end lower_layer_changed;

	
	
-- DRILL SIZE
	procedure apply_drill_size (text : in string) is
		size : type_drill_size;
	begin
		size := to_distance (text);

		-- CS validate. output error in status bar
		via_place.drill.diameter := size;

		et_canvas_board.redraw_board;
	end apply_drill_size;
	
	function drill_key_pressed (
		combo_entry	: access gtk_widget_record'class;
		event		: gdk_event_key) 
		return boolean 
	is
		event_handled : boolean := false;
		key		: gdk_key_type := event.keyval;
		gentry	: gtk_gentry := gtk_gentry (combo_entry);
		text	: constant string := get_text (gentry);
	begin
		case key is
			when GDK_ESCAPE =>
				reset_via_place;
			
			when GDK_TAB => 
				--put_line ("size via tab " & text);
				apply_drill_size (text);
				
			when others => nulL;
		end case;
		
		return event_handled;
	end drill_key_pressed;
	
	procedure drill_entered (combo_entry : access gtk_entry_record'class) is 
		text : constant string := get_text (combo_entry);
	begin
		--put_line ("size " & text);
		apply_drill_size (text);
	end drill_entered;

	
-- RESTRING INNER
	procedure apply_restring_inner (text : in string) is
		width : type_restring_width;
	begin
		width := to_distance (text);

		-- CS validate. output error in status bar
		via_place.restring_inner := width;

		et_canvas_board.redraw_board;
	end apply_restring_inner;
	
	function restring_inner_key_pressed (
		combo_entry	: access gtk_widget_record'class;
		event		: gdk_event_key) 
		return boolean 
	is
		event_handled : boolean := false;
		key		: gdk_key_type := event.keyval;
		gentry	: gtk_gentry := gtk_gentry (combo_entry);
		text	: constant string := get_text (gentry);
	begin
		case key is
			when GDK_ESCAPE =>
				reset_via_place;

			when GDK_TAB => 
				--put_line ("line width via tab " & text);
				apply_restring_inner (text);

			when others => nulL;
		end case;
		
		return event_handled;
	end restring_inner_key_pressed;
	
	procedure restring_inner_entered (combo_entry : access gtk_entry_record'class) is 
		text : constant string := get_text (combo_entry);
	begin
		--put_line ("restring width " & text);
		apply_restring_inner (text);
	end restring_inner_entered;

	
-- RESTRING OUTER
	procedure apply_restring_outer (text : in string) is
		width : type_restring_width;
	begin
		width := to_distance (text);

		-- CS validate. output error in status bar
		via_place.restring_outer := width;

		et_canvas_board.redraw_board;
	end apply_restring_outer;
	
	function restring_outer_key_pressed (
		combo_entry	: access gtk_widget_record'class;
		event		: gdk_event_key) 
		return boolean 
	is
		event_handled : boolean := false;
		key		: gdk_key_type := event.keyval;
		gentry	: gtk_gentry := gtk_gentry (combo_entry);
		text	: constant string := get_text (gentry);
	begin
		case key is
			when GDK_ESCAPE =>
				reset_via_place;

			when GDK_TAB => 
				--put_line ("line width via tab " & text);
				apply_restring_outer (text);

			when others => nulL;
		end case;
		
		return event_handled;
	end restring_outer_key_pressed;
	
	procedure restring_outer_entered (combo_entry : access gtk_entry_record'class) is 
		text : constant string := get_text (combo_entry);
	begin
		--put_line ("restring width " & text);
		apply_restring_outer (text);
	end restring_outer_entered;

	
-- NET NAME
	procedure net_name_changed (combo : access gtk_combo_box_record'class) is
		-- Get the model and active iter from the combo box:
		model : constant gtk_tree_model := combo.get_model;
		iter : constant gtk_tree_iter := combo.get_active_iter;

		name	: glib.values.gvalue;
		index	: glib.values.gvalue;
	begin
		-- Get the net name of the entry column 0:
		gtk.tree_model.get_value (model, iter, 0, name);

		-- Get the net index of the entry column 1:
		gtk.tree_model.get_value (model, iter, 1, index);
		
		--via_place.net_name := to_net_name (glib.values.get_string (name));
		--via_place.net_idx := positive'value (glib.values.get_string (index));

		set (
			net		=> via_place.net,
			name	=> to_net_name (glib.values.get_string (name)),
			idx 	=> positive'value (glib.values.get_string (index)));
		
		--put_line ("net " & to_string (via_place.net_name) & " idx " & positive'image (via_place.net_idx));

		et_canvas_board.redraw_board;
		
		-- CS display layer ?
	end net_name_changed;


	
	procedure reset_via_place is begin
		via_place.being_moved := false;

		-- Remove the via properties bar from the window:
		if box_properties.displayed then
			remove (box_right, box_properties.box_main);
			box_properties.displayed := false;
		end if;
	end reset_via_place;

	
	
	procedure show_via_properties is
		use gtk.window;
		use gtk.box;
		use gtk.label;
		use gtk.gentry;
		use gtk.cell_renderer_text;
		use gtk.cell_layout;

		box_net_name,
		box_category, box_destination_blind, 
		box_buried_upper, box_buried_lower, box_drill,
		box_restring_inner, box_restring_outer : gtk_vbox;

		label_net_name,
		label_category, label_destination_blind, 
		
		label_buried_upper, label_buried_lower, label_drill,
		label_restring_inner, label_restring_outer : gtk_label;

		cbox_net_name,
		cbox_category, cbox_destination_blind,
		cbox_buried_upper, cbox_buried_lower : gtk_combo_box;
		-- Operator can choose between fixed menu entries.
		
		cbox_drill, cbox_restring_inner, cbox_restring_outer : gtk_combo_box_text;
		-- Operator may enter an additional value in the menu.

		-- These constants define the minimum and maximum of
		-- characters that can be entered in the fields for 
		-- drill size and restring width:
		
		drill_size_length_min : constant gint := 1;
		drill_size_length_max : constant gint := 4; 
		-- CS: adjust if necessary. see parameters of type_drill_size.
		
		restring_size_length_min : constant gint := 1;
		restring_size_length_max : constant gint := 5;
		--CS: adjust if necessary. see parameters of type_restring_width.
		
		-- The spacing between the boxes:
		spacing : constant natural := 5;

		-- NET NAME
		procedure make_combo_net is
			storage_model : gtk_list_store;

			-- An entry consists of two columns.
			-- But only the first will be displayed in the combo box. The first entry
			-- will contain the net name.
			-- The second entry will not be displayed but is necessary to convey
			-- the net index.
			column_0 : constant := 0; -- for the net name
			column_1 : constant := 1; -- for the net index

			-- As an indermediate format, the columns will contain 
			-- the data type string:
			entry_structure : glib.gtype_array := (
					column_0 => glib.gtype_string,
					column_1 => glib.gtype_string);

			iter	: gtk_tree_iter;			
			render	: gtk_cell_renderer_text;

			-- We need a list of all net names of the current module:
			use pac_net_names_indexed;
			net_names : constant pac_net_names_indexed.vector := 
				get_indexed_nets (et_canvas_schematic.current_active_module);

			procedure query_net (n : in pac_net_names_indexed.cursor) is 
				use gtk.list_store;
			begin
				storage_model.append (iter);

				-- set the net name in the first column:
				set (storage_model, iter, column_0, to_string (element (n)));

				-- set the net name in the second column:
				set (storage_model, iter, column_1, positive'image (to_index (n)));
			end query_net;
			
		begin -- make_combo_net
			
			gtk_new_vbox (box_net_name, homogeneous => false);
			pack_start (box_properties.box_main, box_net_name, padding => guint (spacing));

			gtk_new (label_net_name, "NET");
			pack_start (box_net_name, label_net_name, padding => guint (spacing));

			
			-- Create the storage model:
			gtk_new (list_store => storage_model, types => (entry_structure));

			-- Insert the net names and indexes in the storage model:
			net_names.iterate (query_net'access);
			
			-- Create the combo box:
			gtk_new_with_model (
				combo_box	=> cbox_net_name,
				model		=> +storage_model); -- ?

			-- Preset the net used last:
			cbox_net_name.set_active (gint (get_index (via_place.net)) - 1);


			pack_start (box_net_name, cbox_net_name, padding => guint (spacing));
			cbox_net_name.on_changed (net_name_changed'access);

			-- The purpose of this stuff is unclear, but it
			-- is required to make the entries visible:
			gtk_new (render);
			pack_start (cbox_net_name, render, expand => true);
			add_attribute (cbox_net_name, render, "markup", column_0);
		end make_combo_net;

		
		-- CATEGORY
		procedure make_combo_category is
			storage_model : gtk_list_store;

			-- An entry consists of just a single column:
			column_0 : constant := 0;

			-- The single column is to contain strings:
			entry_structure : glib.gtype_array := (column_0 => glib.gtype_string);

			iter : gtk_tree_iter;			
			render : gtk_cell_renderer_text;
		begin
			gtk_new_vbox (box_category, homogeneous => false);
			pack_start (box_properties.box_main, box_category, padding => guint (spacing));

			gtk_new (label_category, "VIA CAT");
			pack_start (box_category, label_category, padding => guint (spacing));

			
			-- Create the storage model:
			gtk_new (list_store => storage_model, types => (entry_structure));

			-- Insert the via categories in the storage model:
			for choice in 0 .. type_via_category'pos (type_via_category'last) loop
				storage_model.append (iter);
				gtk.list_store.set (storage_model, iter, column_0,
					to_string (type_via_category'val (choice)));
			end loop;

			-- Create the combo box:
			gtk.combo_box.gtk_new_with_model (
				combo_box	=> cbox_category,
				model		=> +storage_model); -- ?

			-- Set the category used last:
			cbox_category.set_active (type_via_category'pos (via_place.category));


			pack_start (box_category, cbox_category, padding => guint (spacing));
			cbox_category.on_changed (category_changed'access);

			-- The purpose of this stuff is unclear, but it
			-- is required to make the entries visible:
			gtk_new (render);
			pack_start (cbox_category, render, expand => true);
			add_attribute (cbox_category, render, "markup", column_0);
		end make_combo_category;

		-- BLIND
		procedure make_combo_destination is -- for BLIND vias !
			storage_model : gtk_list_store;

			-- An entry consists of just a single column:
			column_0 : constant := 0;

			-- The single column is to contain strings:
			entry_structure : glib.gtype_array := (column_0 => glib.gtype_string);

			iter : gtk_tree_iter;			
			render : gtk_cell_renderer_text;
		begin
			gtk_new_vbox (box_destination_blind, homogeneous => false);
			pack_start (box_properties.box_main, box_destination_blind, padding => guint (spacing));
			
			gtk_new (label_destination_blind, "BL DST");
			pack_start (box_destination_blind, label_destination_blind, padding => guint (spacing));

			
			-- Create the storage model:
			gtk_new (list_store => storage_model, types => (entry_structure));

			-- Insert the available destination layers in the storage model:
			for choice in 
				-- The topmost layer is number 2 because this is about BLIND vias:
				type_via_layer'first .. 

				-- The deepest available layer depends on the stack configuration.
				-- The bottom layer must not be the destination because this 
				-- is about BLIND vias:	
				deepest_conductor_layer (et_canvas_schematic.current_active_module) - 1
			loop
				storage_model.append (iter);
				gtk.list_store.set (storage_model, iter, column_0,
					type_via_layer'image (choice));
			end loop;

			-- Create the combo box:
			gtk.combo_box.gtk_new_with_model (
				combo_box	=> cbox_destination_blind,
				model		=> +storage_model); -- ?

			-- Set the signal layer used last:
			cbox_destination_blind.set_active (gint (via_place.destination_blind) - 2);
			-- NOTE: The entries are numbered from 0 .. N.


			pack_start (box_destination_blind, cbox_destination_blind, padding => guint (spacing));
			cbox_destination_blind.on_changed (destination_changed'access);

			-- The purpose of this stuff is unclear, but it
			-- is required to make the entries visible:
			gtk_new (render);
			pack_start (cbox_destination_blind, render, expand => true);
			add_attribute (cbox_destination_blind, render, "markup", column_0);

		end make_combo_destination;

		-- BURIED
		procedure make_combo_buried_upper is
			storage_model : gtk_list_store;

			-- An entry consists of just a single column:
			column_0 : constant := 0;

			-- The single column is to contain strings:
			entry_structure : glib.gtype_array := (column_0 => glib.gtype_string);

			iter : gtk_tree_iter;			
			render : gtk_cell_renderer_text;
		begin
			gtk_new_vbox (box_buried_upper, homogeneous => false);
			pack_start (box_properties.box_main, box_buried_upper, padding => guint (spacing));
			
			gtk_new (label_buried_upper, "BUR UPPER");
			pack_start (box_buried_upper, label_buried_upper, padding => guint (spacing));

			
			-- Create the storage model:
			gtk_new (list_store => storage_model, types => (entry_structure));

			-- Insert the available destination layers in the storage model:
			for choice in 
				-- The topmost layer is number 2 because this is about BURIED vias:
				type_via_layer'first .. 

				-- The deepest available layer depends on the stack configuration.
				-- The bottom layer must not be the destination because this 
				-- is about BURIED vias:	
				deepest_conductor_layer (et_canvas_schematic.current_active_module) - 1
			loop
				storage_model.append (iter);
				gtk.list_store.set (storage_model, iter, column_0,
					type_via_layer'image (choice));
			end loop;

			-- Create the combo box:
			gtk.combo_box.gtk_new_with_model (
				combo_box	=> cbox_buried_upper,
				model		=> +storage_model); -- ?

			-- Set the signal layer used last:
			cbox_buried_upper.set_active (gint (via_place.layers_buried.upper) - 2);
			-- NOTE: The entries are numbered from 0 .. N.


			pack_start (box_buried_upper, cbox_buried_upper, padding => guint (spacing));
			cbox_buried_upper.on_changed (upper_layer_changed'access);

			-- The purpose of this stuff is unclear, but it
			-- is required to make the entries visible:
			gtk_new (render);
			pack_start (cbox_buried_upper, render, expand => true);
			add_attribute (cbox_buried_upper, render, "markup", column_0);

		end make_combo_buried_upper;

		procedure make_combo_buried_lower is
			storage_model : gtk_list_store;

			-- An entry consists of just a single column:
			column_0 : constant := 0;

			-- The single column is to contain strings:
			entry_structure : glib.gtype_array := (column_0 => glib.gtype_string);

			iter : gtk_tree_iter;			
			render : gtk_cell_renderer_text;
		begin
			gtk_new_vbox (box_buried_lower, homogeneous => false);
			pack_start (box_properties.box_main, box_buried_lower, padding => guint (spacing));
			
			gtk_new (label_buried_lower, "BUR LOWER");
			pack_start (box_buried_lower, label_buried_lower, padding => guint (spacing));

			
			-- Create the storage model:
			gtk_new (list_store => storage_model, types => (entry_structure));

			-- Insert the available destination layers in the storage model:
			for choice in 
				-- The topmost layer is number 2 because this is about BURIED vias:
				type_via_layer'first .. 

				-- The deepest available layer depends on the stack configuration.
				-- The bottom layer must not be the destination because this 
				-- is about BURIED vias:	
				deepest_conductor_layer (et_canvas_schematic.current_active_module) - 1
			loop
				storage_model.append (iter);
				gtk.list_store.set (storage_model, iter, column_0,
					type_via_layer'image (choice));
			end loop;

			-- Create the combo box:
			gtk.combo_box.gtk_new_with_model (
				combo_box	=> cbox_buried_lower,
				model		=> +storage_model); -- ?

			
			-- Set the signal layer used last:
			cbox_buried_lower.set_active (gint (via_place.layers_buried.lower) - 2);
			-- NOTE: The entries are numbered from 0 .. N.


			pack_start (box_buried_lower, cbox_buried_lower, padding => guint (spacing));
			cbox_buried_lower.on_changed (lower_layer_changed'access);

			-- The purpose of this stuff is unclear, but it
			-- is required to make the entries visible:
			gtk_new (render);
			pack_start (cbox_buried_lower, render, expand => true);
			add_attribute (cbox_buried_lower, render, "markup", column_0);

		end make_combo_buried_lower;

		
		
		procedure make_combo_drill is begin
			gtk_new_vbox (box_drill, homogeneous => false);
			pack_start (box_properties.box_main, box_drill, padding => guint (spacing));
			
			gtk_new (label_drill, "DRL SIZE");
			pack_start (box_drill, label_drill, padding => guint (spacing));

			gtk_new_with_entry (cbox_drill);
			pack_start (box_drill, cbox_drill, padding => guint (spacing));
			gtk_entry (cbox_drill.get_child).set_max_length (drill_size_length_max);
			gtk_entry (cbox_drill.get_child).set_width_chars (drill_size_length_min);

			-- Set the text size according to the value used last:
			gtk_entry (cbox_drill.get_child).set_text (trim (to_string (via_place.drill.diameter), left));
			
			-- The size is to be accepted by either pressing TAB or by pressing ENTER:
			gtk_entry (cbox_drill.get_child).on_key_press_event (drill_key_pressed'access);
			gtk_entry (cbox_drill.get_child).on_activate (drill_entered'access);
		end make_combo_drill;

		procedure make_combo_restring_inner is begin
			gtk_new_vbox (box_restring_inner, homogeneous => false);
			pack_start (box_properties.box_main, box_restring_inner, padding => guint (spacing));
			
			gtk_new (label_restring_inner, "RESTR IN");
			pack_start (box_restring_inner, label_restring_inner, padding => guint (spacing));

			gtk_new_with_entry (cbox_restring_inner);
			pack_start (box_restring_inner, cbox_restring_inner, padding => guint (spacing));
			gtk_entry (cbox_restring_inner.get_child).set_max_length (restring_size_length_max);
			gtk_entry (cbox_restring_inner.get_child).set_width_chars (restring_size_length_min);

			-- Set the restring width according to the value used last:
			gtk_entry (cbox_restring_inner.get_child).set_text (trim (to_string (via_place.restring_inner), left));
			
			-- The width is to be accepted by either pressing TAB or by pressing ENTER:
			gtk_entry (cbox_restring_inner.get_child).on_key_press_event (restring_inner_key_pressed'access);
			gtk_entry (cbox_restring_inner.get_child).on_activate (restring_inner_entered'access);
		end make_combo_restring_inner;

		procedure make_combo_restring_outer is begin
			gtk_new_vbox (box_restring_outer, homogeneous => false);
			pack_start (box_properties.box_main, box_restring_outer, padding => guint (spacing));
			
			gtk_new (label_restring_outer, "RESTR OUT");
			pack_start (box_restring_outer, label_restring_outer, padding => guint (spacing));

			gtk_new_with_entry (cbox_restring_outer);
			pack_start (box_restring_outer, cbox_restring_outer, padding => guint (spacing));
			gtk_entry (cbox_restring_outer.get_child).set_max_length (restring_size_length_max);
			gtk_entry (cbox_restring_outer.get_child).set_width_chars (restring_size_length_min);

			-- Set the restring width according to the value used last:
			gtk_entry (cbox_restring_outer.get_child).set_text (trim (to_string (via_place.restring_outer), left));
			
			-- The width is to be accepted by either pressing TAB or by pressing ENTER:
			gtk_entry (cbox_restring_outer.get_child).on_key_press_event (restring_outer_key_pressed'access);
			gtk_entry (cbox_restring_outer.get_child).on_activate (restring_outer_entered'access);
		end make_combo_restring_outer;

			
	begin -- show_via_properties
		
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
			make_combo_net;
			make_combo_destination;
			make_combo_buried_upper;
			make_combo_buried_lower;			
			make_combo_drill;
			make_combo_restring_inner;
			make_combo_restring_outer;
			
			-- Redraw the right box of the window:
			box_right.show_all;
		end if;
		
	end show_via_properties;
	
end et_canvas_board_vias;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
