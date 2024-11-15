------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD VIAS                                --
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

with et_generic_module;				use et_generic_module;
with et_schematic_ops.nets;
with et_canvas_board_2;

with et_exceptions;					use et_exceptions;


package body et_canvas_board_vias is

	use et_canvas_board_2.pac_canvas;
	
	
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

		-- CS et_canvas_board_2.redraw_board;
		
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

		preliminary_via.destination_blind := to_signal_layer (glib.values.get_string (item_text));
		--put_line ("signal layer " & to_string (preliminary_via.destination_blind));

		-- CS et_canvas_board_2.redraw_board;
		
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

		preliminary_via.layers_buried.upper := to_signal_layer (glib.values.get_string (item_text));
		--put_line ("signal layer " & to_string (preliminary_via.destination_blind));

		-- CS et_canvas_board_2.redraw_board;
		
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

		preliminary_via.layers_buried.lower := to_signal_layer (glib.values.get_string (item_text));
		--put_line ("signal layer " & to_string (preliminary_via.destination_blind));

		-- CS et_canvas_board_2.redraw_board;
		
		-- CS display layer ?
	end lower_layer_changed;

	

	
-- DRILL SIZE

	procedure apply_drill_size (text : in string) is
		size : type_drill_size;
	begin
		size := to_distance (text);

		-- CS validate. output error in status bar
		preliminary_via.drill.diameter := size;

		-- CS et_canvas_board_2.redraw_board;
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
				reset_preliminary_via;
			
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

		-- CS et_canvas_board_2.redraw_board;
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
				reset_preliminary_via;

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

		-- CS et_canvas_board_2.redraw_board;
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
				reset_preliminary_via;

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
		preliminary_via.net_name := to_net_name (glib.values.get_string (name));
		
		-- CS display layer ?
	end net_name_changed;



	procedure init_preliminary_via is
		use et_pcb;
		use et_board_ops;

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

		

		-- NET NAME
		procedure make_combo_net is
			use et_canvas_board_tracks;
			use et_schematic_ops.nets;
			use pac_net_name;
			
			store : gtk_list_store;			
			render	: gtk_cell_renderer_text;			
		begin			
			gtk_new_vbox (box_net_name, homogeneous => false);
			pack_start (box_v4, box_net_name, padding => guint (spacing));

			gtk_new (label_net_name, "NET");
			pack_start (box_net_name, label_net_name, padding => guint (spacing));

			
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
			if preliminary_via.net_name = no_name then
				preliminary_via.net_name := get_first_net (active_module);
			end if;
			
			-- Set the acive net (in the box) via its index:
			cbox_net_name.set_active (gint (
				get_net_index (active_module, preliminary_via.net_name, log_threshold + 1)));
			

			pack_start (box_net_name, cbox_net_name, padding => guint (spacing));
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
			pack_start (box_v4, box_category, padding => guint (spacing));

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
			cbox_category.set_active (type_via_category'pos (preliminary_via.category));


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

			use et_board_ops;
		begin
			gtk_new_vbox (box_destination_blind, homogeneous => false);
			pack_start (box_v4, box_destination_blind, padding => guint (spacing));
			
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

			use et_board_ops;
		begin
			gtk_new_vbox (box_buried_upper, homogeneous => false);
			pack_start (box_v4, box_buried_upper, padding => guint (spacing));
			
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

			use et_board_ops;
		begin
			gtk_new_vbox (box_buried_lower, homogeneous => false);
			pack_start (box_v4, box_buried_lower, padding => guint (spacing));
			
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
			pack_start (box_v4, box_drill, padding => guint (spacing));
			
			gtk_new (label_drill, "DRL SIZE");
			pack_start (box_drill, label_drill, padding => guint (spacing));

			gtk_new_with_entry (cbox_drill);
			pack_start (box_drill, cbox_drill, padding => guint (spacing));
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
			pack_start (box_v4, box_restring_inner, padding => guint (spacing));
			
			gtk_new (label_restring_inner, "RESTR IN");
			pack_start (box_restring_inner, label_restring_inner, padding => guint (spacing));

			gtk_new_with_entry (cbox_restring_inner);
			pack_start (box_restring_inner, cbox_restring_inner, padding => guint (spacing));
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
			pack_start (box_v4, box_restring_outer, padding => guint (spacing));
			
			gtk_new (label_restring_outer, "RESTR OUT");
			pack_start (box_restring_outer, label_restring_outer, padding => guint (spacing));

			gtk_new_with_entry (cbox_restring_outer);
			pack_start (box_restring_outer, cbox_restring_outer, padding => guint (spacing));
			gtk_entry (cbox_restring_outer.get_child).set_max_length (restring_size_length_max);
			gtk_entry (cbox_restring_outer.get_child).set_width_chars (restring_size_length_min);

			-- Set the restring width according to the value used last:
			gtk_entry (cbox_restring_outer.get_child).set_text (trim (to_string (preliminary_via.restring_outer), left));
			
			-- The width is to be accepted by either pressing TAB or by pressing ENTER:
			gtk_entry (cbox_restring_outer.get_child).on_key_press_event (restring_outer_key_pressed'access);
			gtk_entry (cbox_restring_outer.get_child).on_activate (restring_outer_entered'access);
		end make_combo_restring_outer;

			
	begin -- show_via_properties

		-- Show the properties bar if there are nets in the module:
		if et_schematic_ops.nets.get_net_count (active_module) > 0 then
			
			-- Output adivse in status bar:
			set_status (status_place_via);
			
			-- If the properties are already displayed, do nothing.
			-- Otherwise show them:
			if not box_properties.displayed then
				init_preliminary_via;
				
				--put_line ("build via properties");
				
				box_properties.displayed := true;

				-- Build the elements of the properties bar:
				make_combo_category;
				make_combo_net;
				make_combo_destination;
				make_combo_buried_upper;
				make_combo_buried_lower;			
				make_combo_drill;
				make_combo_restring_inner;
				make_combo_restring_outer;

				-- Signal the GUI to draw the via:
				preliminary_via.ready := true;
				
				-- Redraw the right box of the window:
				box_v0.show_all;  -- CS box_v4 ?
			end if;

		else
			-- Output error message in status bar:
			set_status ("ERROR: The module has no nets. So no vias can be placed !");
		end if;
	end show_via_properties;


	
	
	procedure reset_preliminary_via is begin
		preliminary_via.ready := false;
		preliminary_via.tool := MOUSE;
		clear_proposed_vias;

		-- Clear the content of the properties bar:
		if box_properties.displayed then
			-- put_line ("clear via properties box");
			
			-- Clear out the properties box:
			remove (box_v4, box_net_name);
			remove (box_v4, box_category);
			remove (box_v4, box_destination_blind);
			remove (box_v4, box_buried_upper);
			remove (box_v4, box_buried_lower);
			remove (box_v4, box_drill);
			remove (box_v4, box_restring_inner);
			remove (box_v4, box_restring_outer);

			-- CS use an iterator to remove widgets like
			-- container.foreach ((element) => container.remove (element));
			--
			-- See <https://stackoverflow.com/questions/36215425/vala-how-do-you-delete-all-the-children-of-a-gtk-container>
			-- See package gtk.container
			
			box_properties.displayed := false;
		end if;
	end reset_preliminary_via;


	

	function via_is_selected (
		via_cursor	: in pac_vias.cursor;
		net_name	: in pac_net_name.bounded_string)
		return boolean
	is 
		use pac_vias;
		via : type_via renames element (via_cursor);
	begin
		-- If there are no proposed vias at all, then there is nothing to do:
		if is_empty (proposed_vias) then
			return false;
		else
			-- If there is no selected via, then there is nothing to do:
			if selected_via /= pac_proposed_vias.no_element then
				if element (selected_via).net = net_name and element (selected_via).via = via then
					return true;
				else 
					return false;
				end if;
			else
				return false;
			end if;
		end if;
	end via_is_selected;

	

	procedure clear_proposed_vias is begin
		proposed_vias.clear;
		selected_via := pac_proposed_vias.no_element;
	end;

	
	
	procedure select_via is begin
		-- On every call of this procedure we advance from one
		-- proposed via to the next in a circular manner. So if the end 
		-- of the list is reached, then the cursor selected_via
		-- moves back to the start of the list of proposed vias:
		if next (selected_via) /= pac_proposed_vias.no_element then
			next (selected_via);
		else
			selected_via := proposed_vias.first;
		end if;

		-- show the selected via in the status bar
		set_status ("selected via " & to_string (selected_via) 
			& ". " & status_next_object_clarification);
		
	end select_via;



	
	procedure find_vias (
		point : in type_vector_model)
	is 
		vias : pac_vias.list;
	begin
		log (text => "locating vias ...", level => log_threshold);
		log_indentation_up;

		-- Collect all vias in the vicinity of the given point:
		proposed_vias := get_vias (
			module_cursor	=> active_module, 
			point			=> point, 
			zone			=> get_catch_zone (et_canvas_board_2.catch_zone),
			log_threshold	=> log_threshold + 1);

		--put_line (count_type'image (proposed_vias.length));
		
		-- evaluate the number of vias found here:
		case length (proposed_vias) is
			when 0 =>
				reset_request_clarification;
				reset_preliminary_via;
				
			when 1 =>
				preliminary_via.ready := true;
				selected_via := proposed_vias.first;
				reset_request_clarification;
				
			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;

				-- preselect the first via
				selected_via := proposed_vias.first;
		end case;
		
		log_indentation_down;
	end find_vias;

	
	


	
-- PLACE:

	procedure place_via (
		point	: in type_vector_model) 
	is
		via : type_via (category => preliminary_via.category);
	begin
		if preliminary_via.ready then
			
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

			place_via (
				module_cursor	=> active_module,
				net_name		=> preliminary_via.net_name,
				via				=> via,
				log_threshold	=> log_threshold + 1);

		end if;
	end place_via;

	


	
-- MOVE:

	procedure move_via (
		tool	: in type_tool;
		point	: in type_vector_model)
	is 

		-- Assigns the final position after the move to the selected via.
		-- Resets variable preliminary_via:
		procedure finalize is 
			use pac_proposed_vias;
		begin
			log (text => "finalizing move ...", level => log_threshold);
			log_indentation_up;

			if selected_via /= pac_proposed_vias.no_element then

				move_via (
					module_cursor	=> active_module,
					via				=> element (selected_via),
					coordinates		=> ABSOLUTE,
					point			=> point,
					log_threshold	=> log_threshold);
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			set_status (status_move_via);			
			reset_preliminary_via;
		end finalize;


	begin
		-- Initially the preliminary_via is not ready.
		if not preliminary_via.ready then

			-- Set the tool being used:
			preliminary_via.tool := tool;
			
			if not clarification_pending then
				-- Locate all vias in the vicinity of the given point:
				find_vias (point);
				-- NOTE: If many vias have been found, then
				-- clarification is now pending.

				-- If find_vias has found only one via
				-- then the flag preliminary_via.read is set true.

			else
				-- Here the clarification procedure ends.
				-- A via has been selected (indicated by cursor selected_via)
				-- via procedure select_via.
				-- By setting preliminary_via.ready, the selected
				-- via will be drawn at the tool position
				-- when conductor objects are drawn on the canvas.
				-- Furtheron, on the next call of this procedure
				-- the selected via will be assigned its final position.
				preliminary_via.ready := true;
				reset_request_clarification;
			end if;
			
		else
			-- Finally move the selected via:
			finalize;
		end if;
	end move_via;


	

-- DELETE:

	procedure delete_via (
		tool	: in type_tool;
		point	: in type_vector_model)
	is 

		procedure finalize is 
			use pac_proposed_vias;
		begin
			log (text => "finalizing deletion ...", level => log_threshold);
			log_indentation_up;

			if selected_via /= pac_proposed_vias.no_element then

				delete_via (
					module_cursor	=> active_module,
					via				=> element (selected_via),
					log_threshold	=> log_threshold + 1);
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			set_status (status_delete_via);			
			reset_preliminary_via;
		end finalize;
		

	begin
		-- Set the tool being used:
		preliminary_via.tool := tool;

		-- Initially there is no clarification pending:
		if not clarification_pending then

			-- Locate all vias in the vicinity of the given point:
			find_vias (point);
			-- NOTE: If many vias have been found, then
			-- clarification is now pending.

			-- If find_vias has found only one via
			-- then delete that via immediately.
			if preliminary_via.ready then
				finalize;
			end if;
			
		else
			-- Here the clarification procedure ends.
			-- A via has been selected (indicated by cursor selected_via)
			-- via procedure select_via.
			reset_request_clarification;
			finalize;
		end if;
	end delete_via;


	
	
	
end et_canvas_board_vias;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
