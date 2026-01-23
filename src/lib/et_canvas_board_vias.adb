------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD VIAS                                --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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


with glib;							use glib;
with glib.values;
with gtk.box;						use gtk.box;
with gtk.combo_box;					use gtk.combo_box;
with gtk.combo_box_text;			use gtk.combo_box_text;
with gtk.label;						use gtk.label;


with gdk.types;						use gdk.types;
with gdk.event;						use gdk.event;
with gdk.types.keysyms;				use gdk.types.keysyms;

-- with gtk.window;					use gtk.window;
with gtk.widget;					use gtk.widget;


with gtk.cell_renderer_text;		
with gtk.cell_layout;        		
with gtk.list_store;				use gtk.list_store;
with gtk.tree_model;				use gtk.tree_model;

with gtk.gentry;					use gtk.gentry;
with gtk.container;					use gtk.container;
with gtk.text_buffer;
with gtk.text_iter;

with et_generic_modules;			use et_generic_modules;
with et_schematic_ops_nets;
with et_board_ops_vias;				use et_board_ops_vias;
with et_board_ops_signal_layers;	use et_board_ops_signal_layers;
with et_canvas_board;
with et_canvas_board_tracks;

with et_logging;					use et_logging;
with et_modes.board;
with et_display;					use et_display;
with et_display.board;				use et_display.board;

with et_exceptions;					use et_exceptions;
with et_nets;
with et_net_names;					use et_net_names;
with et_canvas_board_preliminary_object;	use et_canvas_board_preliminary_object;
with et_object_status;				use et_object_status;
with et_undo_redo;
with et_commit;

with et_module_board;				use et_module_board;

with et_module_board_user_settings;
with et_board_ops_user_settings;	use et_board_ops_user_settings;

with et_fill_zones.boards;
with et_pcb_stack;					use et_pcb_stack;
with et_pcb_signal_layers;			use et_pcb_signal_layers;



package body et_canvas_board_vias is

	use et_canvas_board.pac_canvas;
	

	
-- CATEGORY
	
	procedure category_changed (combo : access gtk_combo_box_record'class) is
		-- Get the model and active iter from the combo box:
		model : constant gtk_tree_model := combo.get_model;
		iter : constant gtk_tree_iter := combo.get_active_iter;

		item_text : glib.values.gvalue;
	begin
		-- Get the actual text of the entry (column is 0):
		gtk.tree_model.get_value (model, iter, 0, item_text);

		preliminary_via.category := to_via_category (glib.values.get_string (item_text));
		--put_line ("cat " & to_string (text_place.category));

		et_canvas_board.redraw_board;
		
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

		preliminary_via.destination_blind := to_signal_layer (glib.values.get_string (item_text));
		--put_line ("signal layer " & to_string (preliminary_via.destination_blind));

		-- Auto-enable the affected conductor layer:
		enable_conductor (preliminary_via.destination_blind);
		
		et_canvas_board.redraw_board;
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

		preliminary_via.layers_buried.upper := to_signal_layer (glib.values.get_string (item_text));
		--put_line ("signal layer " & to_string (preliminary_via.destination_blind));

		-- CS Auto-enable the affected conductor layer
		
		et_canvas_board.redraw_board;		
	end upper_layer_changed;
	


	
	procedure lower_layer_changed (combo : access gtk_combo_box_record'class) is
		-- Get the model and active iter from the combo box:
		model : constant gtk_tree_model := combo.get_model;
		iter : constant gtk_tree_iter := combo.get_active_iter;

		item_text : glib.values.gvalue;
	begin
		-- Get the actual text of the entry (column is 0):
		gtk.tree_model.get_value (model, iter, 0, item_text);

		preliminary_via.layers_buried.lower := to_signal_layer (glib.values.get_string (item_text));
		--put_line ("signal layer " & to_string (preliminary_via.destination_blind));

		-- CS Auto-enable the affected conductor layer
		
		et_canvas_board.redraw_board;		
	end lower_layer_changed;

	

	
-- DRILL SIZE

	procedure apply_drill_size (text : in string) is
		size : type_drill_size;
	begin
		size := to_distance (text);

		-- CS validate. output error in status bar
		preliminary_via.drill.diameter := size;

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
				et_canvas_board.reset;
			
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
		preliminary_via.restring_inner := width;

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
				et_canvas_board.reset;

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
		preliminary_via.restring_outer := width;

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
		-- put_line ("restring_outer_key_pressed");
		
		case key is
			when GDK_ESCAPE =>
				et_canvas_board.reset;

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
	begin
		-- Get the net name of the entry column 0:
		gtk.tree_model.get_value (model, iter, 0, name);
		object_net_name := to_net_name (glib.values.get_string (name));
		
		-- CS Auto-enable the affected conductor layer ?
	end net_name_changed;


	

	procedure init_preliminary_via is
		use et_module_board_user_settings;

		rules : constant type_design_rules_board := get_pcb_design_rules (active_module);

		-- get the user specific settings of the board
		settings : constant type_user_settings := get_user_settings (active_module);
		
	begin
		-- Set the drill size and restring according to user specific values:
		-- If user has not specified defaults, use values given in DRU data set:

		-- set drill size:
		if settings.vias.drill.active then
			preliminary_via.drill.diameter	:= settings.vias.drill.size;
		else
			preliminary_via.drill.diameter	:= rules.sizes.drills;
		end if;

		-- set outer restring:
		if settings.vias.restring_outer.active then
			preliminary_via.restring_outer	:= settings.vias.restring_outer.width;
		else
			preliminary_via.restring_outer	:= auto_set_restring (
				OUTER, preliminary_via.drill.diameter);
		end if;
		
		-- set inner restring:
		if settings.vias.restring_inner.active then
			preliminary_via.restring_inner	:= settings.vias.restring_inner.width;
		else
			preliminary_via.restring_inner	:= auto_set_restring (
				INNER, preliminary_via.drill.diameter, rules.sizes.restring.delta_size);
		end if;
		
	end init_preliminary_via;



	

	procedure show_via_properties is
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
		

		
		-- NET NAME
		procedure make_combo_net is
			use et_canvas_board_tracks;
			use et_schematic_ops_nets;
			use pac_net_name;
			
			store : gtk_list_store;			
			render	: gtk_cell_renderer_text;			
		begin			
			gtk_new_vbox (box_net_name, homogeneous => false);
			pack_start (box_v4, box_net_name, padding => box_properties_spacing);

			gtk_new (label_net_name, "NET");
			pack_start (box_net_name, label_net_name, padding => box_properties_spacing);

			
			-- Create the storage model:
			make_store_for_net_names (store);
			
			-- Create the combo box:
			gtk_new_with_model (
				combo_box	=> cbox_net_name,
				model		=> +store); -- ?

			-- Initially, on the first call of this procedure, there is no net name
			-- specified in preliminary_via. In this case the first net of the 
			-- module is assumed and the net index set accordingly.
			-- NOTE: The net index is numbered from 0 .. N.
			if object_net_name = no_name then
				object_net_name := get_first_net (active_module);
			end if;
			
			-- Set the acive net (in the box) via its index:
			cbox_net_name.set_active (gint (
				get_net_index (active_module, object_net_name, log_threshold + 1)));
			

			pack_start (box_net_name, cbox_net_name, padding => box_properties_spacing);
			cbox_net_name.on_changed (net_name_changed'access);

			-- The purpose of this stuff is unclear, but it
			-- is required to make the entries visible:
			gtk_new (render);
			pack_start (cbox_net_name, render, expand => true);
			add_attribute (cbox_net_name, render, "markup", 0); -- column 0
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
			pack_start (box_v4, box_category, padding => box_properties_spacing);

			gtk_new (label_category, "VIA CAT");
			pack_start (box_category, label_category, padding => box_properties_spacing);

			
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
			cbox_category.set_active (type_via_category'pos (preliminary_via.category));


			pack_start (box_category, cbox_category, padding => box_properties_spacing);
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
			pack_start (box_v4, box_destination_blind, padding => box_properties_spacing);
			
			gtk_new (label_destination_blind, "BL DST");
			pack_start (box_destination_blind, label_destination_blind, padding => box_properties_spacing);

			
			-- Create the storage model:
			gtk_new (list_store => storage_model, types => (entry_structure));

			-- Insert the available destination layers in the storage model:
			for choice in 
				-- The topmost layer is number 2 because this is about BLIND vias:
				type_via_layer'first .. 

				-- The deepest available layer depends on the stack configuration.
				-- The bottom layer must not be the destination because this 
				-- is about BLIND vias:	
				get_deepest_conductor_layer (active_module) - 1
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
			cbox_destination_blind.set_active (gint (preliminary_via.destination_blind) - 2);
			-- NOTE: The entries are numbered from 0 .. N.


			pack_start (box_destination_blind, cbox_destination_blind, padding => box_properties_spacing);
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
			pack_start (box_v4, box_buried_upper, padding => box_properties_spacing);
			
			gtk_new (label_buried_upper, "BUR UPPER");
			pack_start (box_buried_upper, label_buried_upper, padding => box_properties_spacing);

			
			-- Create the storage model:
			gtk_new (list_store => storage_model, types => (entry_structure));

			-- Insert the available destination layers in the storage model:
			for choice in 
				-- The topmost layer is number 2 because this is about BURIED vias:
				type_via_layer'first .. 

				-- The deepest available layer depends on the stack configuration.
				-- The bottom layer must not be the destination because this 
				-- is about BURIED vias:	
				get_deepest_conductor_layer (active_module) - 1
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
			cbox_buried_upper.set_active (gint (preliminary_via.layers_buried.upper) - 2);
			-- NOTE: The entries are numbered from 0 .. N.


			pack_start (box_buried_upper, cbox_buried_upper, padding => box_properties_spacing);
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
			pack_start (box_v4, box_buried_lower, padding => box_properties_spacing);
			
			gtk_new (label_buried_lower, "BUR LOWER");
			pack_start (box_buried_lower, label_buried_lower, padding => box_properties_spacing);

			
			-- Create the storage model:
			gtk_new (list_store => storage_model, types => (entry_structure));

			-- Insert the available destination layers in the storage model:
			for choice in 
				-- The topmost layer is number 2 because this is about BURIED vias:
				type_via_layer'first .. 

				-- The deepest available layer depends on the stack configuration.
				-- The bottom layer must not be the destination because this 
				-- is about BURIED vias:	
				get_deepest_conductor_layer (active_module) - 1
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
			cbox_buried_lower.set_active (gint (preliminary_via.layers_buried.lower) - 2);
			-- NOTE: The entries are numbered from 0 .. N.


			pack_start (box_buried_lower, cbox_buried_lower, padding => box_properties_spacing);
			cbox_buried_lower.on_changed (lower_layer_changed'access);

			-- The purpose of this stuff is unclear, but it
			-- is required to make the entries visible:
			gtk_new (render);
			pack_start (cbox_buried_lower, render, expand => true);
			add_attribute (cbox_buried_lower, render, "markup", column_0);

		end make_combo_buried_lower;

		
		
		procedure make_combo_drill is begin
			gtk_new_vbox (box_drill, homogeneous => false);
			pack_start (box_v4, box_drill, padding => box_properties_spacing);
			
			gtk_new (label_drill, "DRL SIZE");
			pack_start (box_drill, label_drill, padding => box_properties_spacing);

			gtk_new_with_entry (cbox_drill);
			pack_start (box_drill, cbox_drill, padding => box_properties_spacing);
			gtk_entry (cbox_drill.get_child).set_max_length (drill_size_length_max);
			gtk_entry (cbox_drill.get_child).set_width_chars (drill_size_length_min);

			-- Set the text size according to the value used last:
			gtk_entry (cbox_drill.get_child).set_text (trim (to_string (preliminary_via.drill.diameter), left));
			
			-- The size is to be accepted by either pressing TAB or by pressing ENTER:
			gtk_entry (cbox_drill.get_child).on_key_press_event (drill_key_pressed'access);
			gtk_entry (cbox_drill.get_child).on_activate (drill_entered'access);
		end make_combo_drill;


		
		procedure make_combo_restring_inner is begin
			gtk_new_vbox (box_restring_inner, homogeneous => false);
			pack_start (box_v4, box_restring_inner, padding => box_properties_spacing);
			
			gtk_new (label_restring_inner, "RESTR IN");
			pack_start (box_restring_inner, label_restring_inner, padding => box_properties_spacing);

			gtk_new_with_entry (cbox_restring_inner);
			pack_start (box_restring_inner, cbox_restring_inner, padding => box_properties_spacing);
			gtk_entry (cbox_restring_inner.get_child).set_max_length (restring_size_length_max);
			gtk_entry (cbox_restring_inner.get_child).set_width_chars (restring_size_length_min);

			-- Set the restring width according to the value used last:
			gtk_entry (cbox_restring_inner.get_child).set_text (trim (to_string (preliminary_via.restring_inner), left));
			
			-- The width is to be accepted by either pressing TAB or by pressing ENTER:
			gtk_entry (cbox_restring_inner.get_child).on_key_press_event (restring_inner_key_pressed'access);
			gtk_entry (cbox_restring_inner.get_child).on_activate (restring_inner_entered'access);
		end make_combo_restring_inner;

		
		procedure make_combo_restring_outer is begin
			gtk_new_vbox (box_restring_outer, homogeneous => false);
			pack_start (box_v4, box_restring_outer, padding => box_properties_spacing);
			
			gtk_new (label_restring_outer, "RESTR OUT");
			pack_start (box_restring_outer, label_restring_outer, padding => box_properties_spacing);

			gtk_new_with_entry (cbox_restring_outer);
			pack_start (box_restring_outer, cbox_restring_outer, padding => box_properties_spacing);
			gtk_entry (cbox_restring_outer.get_child).set_max_length (restring_size_length_max);
			gtk_entry (cbox_restring_outer.get_child).set_width_chars (restring_size_length_min);

			-- Set the restring width according to the value used last:
			gtk_entry (cbox_restring_outer.get_child).set_text (trim (to_string (preliminary_via.restring_outer), left));
			
			-- The width is to be accepted by either pressing TAB or by pressing ENTER:
			gtk_entry (cbox_restring_outer.get_child).on_key_press_event (restring_outer_key_pressed'access);
			gtk_entry (cbox_restring_outer.get_child).on_activate (restring_outer_entered'access);
		end make_combo_restring_outer;


		use et_nets;
	begin

		-- Show the properties bar if there are nets in the module:
		if et_schematic_ops_nets.get_net_count (active_module) > 0 then
			
			-- Output adivse in status bar:
			set_status (status_place_via);

			-- Before inserting any widgets, the properties box
			-- must be cleared:
			clear_out_properties_box;

			init_preliminary_via; 
			-- CS should be skipped after placing the first via
			-- so that the last settings remain.
				

			-- Build the elements of the properties bar:
			make_combo_category;
			make_combo_net;
			make_combo_destination;
			make_combo_buried_upper;
			make_combo_buried_lower;			
			make_combo_drill;
			make_combo_restring_inner;
			make_combo_restring_outer;

			-- Redraw the right box of the window:
			box_v0.show_all;  -- CS box_v4 ?

		else
			-- Output error message in status bar:
			set_status ("ERROR: The module has no nets. So no vias can be placed !");
		end if;
	end show_via_properties;



	
	
	
	


	
-- PLACE:

	procedure place_via (
		point	: in type_vector_model) 
	is
		use et_modes.board;
		use et_undo_redo;
		use et_commit;
					
		via : type_via (category => preliminary_via.category);
	begin
		via.position := preliminary_via.drill.position;
		via.diameter := preliminary_via.drill.diameter;
		
		via.restring_inner := preliminary_via.restring_inner;
		
		move_to (via.position, point);

		case preliminary_via.category is
			when THROUGH =>
				via.restring_outer := preliminary_via.restring_outer;
								
			when BURIED =>
				via.layers := preliminary_via.layers_buried;
										
			when BLIND_DRILLED_FROM_TOP =>
				via.restring_top := preliminary_via.restring_outer;
				via.lower := preliminary_via.destination_blind;
				
			when BLIND_DRILLED_FROM_BOTTOM =>
				via.restring_bottom := preliminary_via.restring_outer;
				via.upper := preliminary_via.destination_blind;

		end case;


		-- Commit the current state of the design:
		commit (PRE, verb, noun, log_threshold + 1);
		
		place_via (
			module_cursor	=> active_module,
			net_name		=> object_net_name,
			via				=> via,
			log_threshold	=> log_threshold + 1);

		-- Commit the new state of the design:
		commit (POST, verb, noun, log_threshold + 1);
		
	end place_via;

	



	

-- OBJECTS:

	procedure show_selected_object (
		selected : in type_object_via)
	is 
		praeamble : constant string := "selected: ";
	begin
		set_status (praeamble 
			& "net " & et_nets.get_net_name (selected.net_cursor)
			& " " & to_string (selected.via_cursor) & ". " 
			& status_next_object_clarification);
	end show_selected_object;

	
	


	procedure clarify_object is 

		procedure do_it is
			use pac_objects;
			
			-- Gather all proposed objects:
			proposed_objects : constant pac_objects.list := 
				get_objects (active_module, PROPOSED, log_threshold + 1);

			proposed_object : pac_objects.cursor;

			-- We start with the first object that is currently selected:
			selected_object : type_object_via := 
				get_first_object (active_module, SELECTED, log_threshold + 1);

		begin
			log (text => "proposed objects total " 
				& natural'image (get_count (proposed_objects)),
				level => log_threshold + 2);

			
			-- Locate the selected object among the proposed objects:
			proposed_object := proposed_objects.find (selected_object);

			-- Deselect the proposed object:
			modify_status (
				module_cursor	=> active_module, 
				operation		=> to_operation (CLEAR, SELECTED),
				object_cursor	=> proposed_object, 
				log_threshold	=> log_threshold + 1);

			-- Advance to the next proposed object:
			next (proposed_object);

			-- If end of list reached, then proceed at 
			-- the begin of the list:
			if proposed_object = pac_objects.no_element then
				proposed_object := proposed_objects.first;
			end if;
			
			-- Select the proposed object:
			modify_status (
				module_cursor	=> active_module, 
				operation		=> to_operation (SET, SELECTED),
				object_cursor	=> proposed_object, 
				log_threshold	=> log_threshold + 1);

			-- Display the object in the status bar:
			show_selected_object (element (proposed_object));
		end do_it;
		
		
	begin
		log (text => "clarify_object", level => log_threshold + 1);

		log_indentation_up;
		
		do_it;
		
		log_indentation_down;
	end clarify_object;
	



	
	

	-- This procedure searches for the first selected object
	-- and sets its status to "moving":
	procedure set_first_selected_object_moving is
		
		procedure do_it is
			-- Get the first selected object:
			selected_object : constant type_object_via := 
				get_first_object (active_module, SELECTED, log_threshold + 1);

			-- Gather all selected objects:
			objects : constant pac_objects.list :=
				get_objects (active_module, SELECTED, log_threshold + 1);

			c : pac_objects.cursor;
		begin
			-- Get a cursor to the candidate object
			-- among all selected objects:
			c := objects.find (selected_object);
			
			modify_status (active_module, c, to_operation (SET, MOVING), log_threshold + 1);
		end do_it;
		
		
	begin
		log (text => "set_first_selected_object_moving ...", level => log_threshold);
		log_indentation_up;
		do_it;
		log_indentation_down;
	end set_first_selected_object_moving;



	


	procedure find_objects (
		point : in type_vector_model)
	is 
		use et_modes.board;
		
		-- The number of proposed objects:
		count_total : natural := 0;

		
		-- This procedure searches for the first proposed
		-- object and marks it as "selected":
		procedure select_first_proposed is
			object : type_object_via := get_first_object (
						active_module, PROPOSED, log_threshold + 1);
		begin
			modify_status (
				active_module, object, to_operation (SET, SELECTED), log_threshold + 1);

			-- If only one object found, then show it in the status bar:
			if count_total = 1 then
				show_selected_object (object);
			end if;
		end select_first_proposed;


		-- This procedure proposes objects in the vicinity of the given point:
		procedure propose_objects is begin

			-- CS test whether vias are displayed
			
			propose_vias (
				module_cursor	=> active_module, 
				catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
				count			=> count_total,
				log_threshold	=> log_threshold + 1);
			
		end propose_objects;

		
	begin		
		log (text => "proposing vias ...", level => log_threshold);
		log_indentation_up;

		propose_objects;

		log (text => "proposed vias total" & natural'image (count_total),
			 level => log_threshold + 1);

		
		-- evaluate the number of vias found here:
		case count_total is
			when 0 =>
				null; -- nothing to do
				
			when 1 =>
				set_edit_process_running;
				select_first_proposed;
				
				if verb = VERB_MOVE then
					set_first_selected_object_moving;
				end if;
				
				reset_request_clarification;
				
			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;
				select_first_proposed;
		end case;
		
		log_indentation_down;
	end find_objects;



	
	
	
-- MOVE:

	procedure move_object (
		tool	: in type_tool;
		point	: in type_vector_model)
	is 

		procedure finalize is
			use et_modes.board;
			use et_undo_redo;
			use et_commit;
			use pac_vias;

			object : constant type_object_via := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize move", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if has_element (object.via_cursor) then

				reset_proposed_vias (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				move_object (
					module_cursor	=> active_module,
					object			=> object,
					coordinates		=> ABSOLUTE,
					destination		=> point,
					log_threshold	=> log_threshold);
				
				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

			else
				log (text => "nothing to do", level => log_threshold);
			end if;
			
			log_indentation_down;			
			
			set_status (status_move_via);			
			-- CS clear ?

			reset_editing_process; -- prepare for a new editing process
		end finalize;


	begin
		-- Initially the editing process is not running.
		if not edit_process_running then

			-- Set the tool being used:
			object_tool := tool;
			
			if not clarification_pending then
				-- Locate all vias in the vicinity of the given point:
				find_objects (point);
				
				-- NOTE: If many vias have been found, then
				-- clarification is now pending.

				-- If find_objects has found only one via
				-- then the flag edit_process.running is set true.

			else
				-- Here the clarification procedure ends.
				-- A via has been selected via procedure clarify_object.
				-- By setting the status of the selected
				-- via as "moving", the selected via
				-- will be drawn according to the tool position.
				set_first_selected_object_moving;

				-- Furtheron, on the next call of this procedure
				-- the selected segment will be assigned its final position.
				
				set_edit_process_running;
				reset_request_clarification;
			end if;
			
		else
			finalize;
		end if;
	end move_object;
	


	

-- DELETE:

	procedure delete_via (
		tool	: in type_tool;
		point	: in type_vector_model)
	is 
		-- Deletes the selected object.
		-- Resets variable preliminary_object:
		procedure finalize is 
			use et_modes.board;
			use et_undo_redo;
			use et_commit;
			use pac_vias;

			object : constant type_object_via := get_first_object (
				active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize delete", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if has_element (object.via_cursor) then
			
				reset_proposed_vias (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				delete_object (
					module_cursor	=> active_module, 
					object			=> object, 
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			
			set_status (status_delete_via);
			-- CS clear ?

			reset_editing_process; -- prepare for a new editing process			
		end finalize;
		

	begin
		-- Set the tool being used:
		object_tool := tool;

		-- Initially there is no clarification pending:
		if not clarification_pending then

			-- Locate all vias in the vicinity of the given point:
			find_objects (point);
			-- NOTE: If many vias have been found, then
			-- clarification is now pending.

			-- If find_objects has found only one via
			-- then delete that via immediately.
			if edit_process_running then
				finalize;
			end if;
			
		else
			-- Here the clarification procedure ends.
			-- A via has been selected 
			-- via procedure clarify_object.
			
			finalize;
		end if;
	end delete_via;
	
	
end et_canvas_board_vias;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
