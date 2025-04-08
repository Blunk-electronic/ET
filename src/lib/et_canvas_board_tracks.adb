------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD TRACKS                              --
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

with ada.text_io;						use ada.text_io;
with ada.strings;						use ada.strings;
with ada.strings.fixed; 				use ada.strings.fixed;

with glib;								use glib;
with glib.values;
with gtk.box;							use gtk.box;
with gtk.label;							use gtk.label;
with gtk.combo_box;						use gtk.combo_box;
with gtk.combo_box_text;				use gtk.combo_box_text;


with gdk.types;							use gdk.types;
with gdk.event;							use gdk.event;
with gdk.types.keysyms;					use gdk.types.keysyms;

with gtk.widget;						use gtk.widget;

with gtk.cell_renderer_text;		
with gtk.cell_layout;        		
with gtk.tree_model;

with gtk.gentry;						use gtk.gentry;
with gtk.container;						use gtk.container;
with gtk.button;						use gtk.button;

with et_generic_module;					use et_generic_module;

with et_board_ops.conductors;			use et_board_ops.conductors;
with et_modes.board;

with et_nets;
with et_net_names;
with et_schematic_ops.nets;

with et_display;						use et_display;
with et_display.board;					use et_display.board;

with et_logging;						use et_logging;
with et_string_processing;				use et_string_processing;
with et_exceptions;						use et_exceptions;

with et_undo_redo;
with et_commit;
with et_object_status;

with et_canvas_board_preliminary_object;	use et_canvas_board_preliminary_object;
with et_ripup;


package body et_canvas_board_tracks is
	
	

	function to_string (
		mode	: in type_snap_mode)
		return string
	is begin
		return " " & type_snap_mode'image (mode);
	end to_string;





	procedure net_name_changed (combo : access gtk_combo_box_record'class) is
		use gtk.tree_model;
		use gtk.list_store;

		-- Get the model and active iter from the combo box:
		model : constant gtk_tree_model := combo.get_model;
		iter : constant gtk_tree_iter := combo.get_active_iter;

		name : glib.values.gvalue;
	begin
		-- Get the net name of the entry (column is 0):
		gtk.tree_model.get_value (model, iter, 0, name);
		object_net_name := to_net_name (glib.values.get_string (name));
		
		-- put_line ("selected net " & pac_net_name.to_string (object_net_name));
	end net_name_changed;


	
	
	
	procedure signal_layer_changed (combo : access gtk_combo_box_record'class) is
		use gtk.tree_model;
		use gtk.list_store;

		-- Get the model and active iter from the combo box:
		model : constant gtk_tree_model := combo.get_model;
		iter : constant gtk_tree_iter := combo.get_active_iter;

		item_text : glib.values.gvalue;
	begin
		-- Get the actual text of the entry (column is 0):
		gtk.tree_model.get_value (model, iter, 0, item_text);

		object_signal_layer := to_signal_layer (glib.values.get_string (item_text));
		--put_line ("signal layer " & to_string (object_signal_layer));

		-- display the affected conductor layer:
		enable_conductor (object_signal_layer);
		
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
		use et_canvas_board_2;
		
		event_handled : boolean := false;
		
		use gdk.types;
		key : gdk_key_type := event.keyval;

		gentry : gtk_gentry := gtk_gentry (combo_entry);
		text : constant string := get_text (gentry);
	begin
		case key is
			when GDK_ESCAPE =>
				reset;

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


	

	
	procedure make_store_for_net_names (
		store : in out gtk_list_store)
	is
		use gtk.list_store;
		
		column_0 : constant := 0; -- for the net name
		column_1 : constant := 1; -- for the net index

		entry_structure : glib.gtype_array := (
				column_0 => glib.gtype_string,
				column_1 => glib.gtype_string);

		use gtk.tree_model;
		iter : gtk_tree_iter;			

		use et_schematic_ops.nets;
		use pac_net_name;
		use pac_net_names;

		-- Fetch the net names of all nets of the current module:
		nets : pac_net_names.list := get_nets (active_module, log_threshold + 1);
		
		index : type_net_index := 0;

		-- Enters the name and index of a net into the storage model:
		procedure query_net (c : in pac_net_names.cursor) is begin
			store.append (iter);
			set (store, iter, column_0, to_string (c));
			set (store, iter, column_1, type_net_index'image (index));
			index := index + 1;
		end query_net;

	begin
		-- Create the storage model:
		gtk_new (list_store => store, types => (entry_structure));

		-- Insert the available net names in the storage model:
		nets.iterate (query_net'access);	
	end make_store_for_net_names;
									 


	
	
	
	procedure show_track_properties is

		use et_canvas_board_2.pac_canvas;
		
		use gtk.cell_renderer_text;
		use gtk.cell_layout;
		use gtk.tree_model;



		box_net_name, box_signal_layer, box_line_width : gtk_vbox;
		
		label_net_name, label_signal_layer, label_line_width : gtk_label;
		
		cbox_net_name : gtk_combo_box;
		cbox_signal_layer : gtk_combo_box;
		-- Operator can choose between fixed menu entries.
		
		cbox_line_width : gtk_combo_box_text;
		-- Operator may enter an additional value in the menu.
		
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
		-- CS: adjust if necessary. see et_pcb_coordinates_2 type_rotation.
		
		-- The spacing between the boxes:
		spacing : constant natural := 5;
			

	

		procedure make_combo_for_net_name is
			store : gtk_list_store; -- will contain net names
			render : gtk_cell_renderer_text;

			use et_schematic_ops.nets;
			use pac_net_name;

		begin
			gtk_new_vbox (box_net_name, homogeneous => false);
			pack_start (box_v4, box_net_name, padding => guint (spacing));
			
			gtk_new (label_net_name, "NET NAME");
			pack_start (box_net_name, label_net_name, padding => guint (spacing));
			
			-- Create the storage for the net names:
			make_store_for_net_names (store);
			
			-- Create the combo box with the net names inside:
			gtk.combo_box.gtk_new_with_model (
				combo_box	=> cbox_net_name,
				model		=> +store); -- ?

			-- Initally, on the first call of this procedure, there is no net name
			-- specified. In this case the first net of the 
			-- module is assumed and the net index set accordingly.
			-- NOTE: The net index is numbered from 0 .. N.
			if object_net_name = no_name then
				object_net_name := get_first_net (active_module);
			end if;

			-- Set the acive net (in the box) via its index:
			cbox_net_name.set_active (gint (
				get_net_index (active_module, object_net_name, log_threshold + 1)));

									  
			pack_start (box_net_name, cbox_net_name, padding => guint (spacing));
			cbox_net_name.on_changed (net_name_changed'access);

			-- The purpose of this stuff is unclear, but it
			-- is required to make the entries visible:
			gtk_new (render);
			pack_start (cbox_net_name, render, expand => true);
			add_attribute (cbox_net_name, render, "markup", 0); -- column 0
		end make_combo_for_net_name;



		
		procedure make_combo_for_signal_layer is
			storage_model : gtk_list_store;

			-- An entry consists of just a single column:
			column_0 : constant := 0;

			-- The single column is to contain strings:
			entry_structure : glib.gtype_array := (column_0 => glib.gtype_string);

			iter : gtk_tree_iter;			
			render : gtk_cell_renderer_text;

			use et_board_ops;
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

			gtk_new (label_line_width, "TRACK WIDTH");
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


		
	begin -- show_track_properties

		-- Before inserting any widgets, the properties box
		-- must be cleared:
		clear_out_properties_box;

		-- Build the elements of the properties bar:
		make_combo_for_net_name;
		make_combo_for_signal_layer;
		make_combo_for_line_width;

		-- Redraw the right box of the window:
		box_v0.show_all;  -- CS box_v4 ?
	end show_track_properties;

	

	

	procedure next_snap_mode is 
		i : constant natural := type_snap_mode'pos (snap_mode);
		-- i points now to the current snap mode

		-- get the index of the last available snap mode:
		max : constant natural := type_snap_mode'pos (type_snap_mode'last);
	begin
		if i < max then
			-- jump to next snap mode
			snap_mode := type_snap_mode'succ (type_snap_mode'val (i));
		else 
			-- After the last snap mode, jump back to the first snap mode:
			snap_mode := type_snap_mode'first;
		end if;

		-- put_line ("snap mode " & to_string (PT.snap_mode));
		-- CS show in toolbar or status bar
	end next_snap_mode;
	


	


	procedure show_selected_object (
		selected		: in type_object_airwire)
	is 
		praeamble : constant string := "selected: ";
	begin
		set_status (praeamble 
			& "net " & et_nets.to_string (selected.net_cursor)
			& " " & to_string (selected.wire_cursor) & ". " 
			& status_next_object_clarification);
	end show_selected_object;



	
	

	procedure clarify_airwire is
		
		procedure do_it is
			use et_object_status;
			use et_board_ops.ratsnest.pac_objects;
			
			-- Gather all proposed airwire objects:
			proposed_objects : constant et_board_ops.ratsnest.pac_objects.list := 
				get_objects (active_module, PROPOSED, log_threshold + 1);

			proposed_object : et_board_ops.ratsnest.pac_objects.cursor;

			-- We start with the first airwire that is currently selected:
			selected_object : type_object_airwire := 
				get_first_object (active_module, SELECTED, log_threshold + 1);

		begin
			log (text => "proposed airwires total " 
				& natural'image (get_count (proposed_objects)),
				level => log_threshold + 2);

			
			-- Locate the selected object among the proposed objects:
			proposed_object := proposed_objects.find (selected_object);

			-- Deselect the the proposed object:
			modify_status (
				module_cursor	=> active_module, 
				operation		=> to_operation (CLEAR, SELECTED),
				object_cursor	=> proposed_object, 
				log_threshold	=> log_threshold + 1);

			-- The proposed object is no longer moving:
			modify_status (
				module_cursor	=> active_module, 
				operation		=> to_operation (CLEAR, MOVING),
				object_cursor	=> proposed_object, 
				log_threshold	=> log_threshold + 1);
			
			-- Advance to the next proposed object:
			next (proposed_object);

			-- If end of list reached, then proceed at 
			-- the begin of the list:
			if proposed_object = et_board_ops.ratsnest.pac_objects.no_element then
				proposed_object := proposed_objects.first;
			end if;
			
			-- Select the proposed object:
			modify_status (
				module_cursor	=> active_module, 
				operation		=> to_operation (SET, SELECTED),
				object_cursor	=> proposed_object, 
				log_threshold	=> log_threshold + 1);

			-- Set the proposed object as moving:
			modify_status (
				module_cursor	=> active_module, 
				operation		=> to_operation (SET, MOVING),
				object_cursor	=> proposed_object, 
				log_threshold	=> log_threshold + 1);

			
			-- Display the object in the status bar:
			show_selected_object (element (proposed_object));

			-- Get the name of the affected net:
			object_net_name := get_net_name (proposed_object);
		end do_it;
		
		
	begin
		log (text => "clarify_airwire", level => log_threshold + 1);
		log_indentation_up;		
		do_it;		
		log_indentation_down;
	end clarify_airwire;


	
	

	
	procedure make_path (
		tool	: in type_tool;
		point	: in type_vector_model)
	is
		line : type_line;


		-- This procedure sets the start point of the path:
		procedure set_start_point is
			use et_object_status;
			use et_pcb_coordinates_2.pac_geometry_brd;

			use et_nets;
			use et_net_names;
			
			praeamble : constant string := "net ";
			
			
			procedure arbitrary_start_point is begin
				live_path.start_point := point;
				status_bar_path_show_start_point (praeamble & net_name_to_string (object_net_name));
				set_edit_process_running;
			end arbitrary_start_point;

			

			procedure start_with_nearest_airwire is 
				count : natural := 0; -- the number of proposed airwires
				aw : type_object_airwire;

				
				use pac_airwires;
				

				procedure one_airwire_proposed is begin
					aw := get_first_object (active_module, PROPOSED, log_threshold + 1);

					-- Get the name of the affected net:
					object_net_name := get_net_name (aw.net_cursor);

					live_path.start_point := get_nearest (element (aw.wire_cursor), point);
					status_bar_path_show_start_point (praeamble & net_name_to_string (object_net_name));
					
					modify_status (active_module, aw, to_operation (SET, SELECTED), log_threshold + 1);
					modify_status (active_module, aw, to_operation (SET, MOVING), log_threshold + 1);
				
					set_edit_process_running;
				end one_airwire_proposed;
	

				
				procedure many_airwires_proposed is begin
					set_request_clarification;
					aw := get_first_object (active_module, PROPOSED, log_threshold + 1);

					-- Get the name of the affected net:
					object_net_name := get_net_name (aw.net_cursor);
					
					modify_status (active_module, aw, to_operation (SET, SELECTED), log_threshold + 1);
					modify_status (active_module, aw, to_operation (SET, MOVING), log_threshold + 1);
				end many_airwires_proposed;
	
				
			begin
				if not clarification_pending then

					propose_airwires (
						module_cursor	=> active_module, 
						catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
						count			=> count,
						log_threshold	=> log_threshold + 1);

											
					-- Depending on how many airwires have been proposed
					-- three cases arise:
					case count is	
						when 0 =>
							arbitrary_start_point;

						when 1 =>
							one_airwire_proposed;

						when others =>
							many_airwires_proposed;
															
					end case;
					

				else -- clarification_pending
					aw := get_first_object (active_module, SELECTED, log_threshold + 1);

					live_path.start_point := get_nearest (element (aw.wire_cursor), point);
					
					set_edit_process_running;

					reset_request_clarification;
				end if;

			end start_with_nearest_airwire;

			
			
		begin
			case snap_mode is
				when NO_SNAP =>
					arbitrary_start_point;
					
				when NEAREST_AIRWIRE =>
					start_with_nearest_airwire;

				when NEAREST_OBJECT =>
					null; -- CS
			end case;
		

		end set_start_point;
		

		
		
		procedure add_to_net is
			use et_board_ops.conductors;
			use et_modes.board;
			use et_undo_redo;
			use et_commit;
		begin
			-- Commit the current state of the design:
			commit (PRE, verb, noun, log_threshold + 1);

			add_line (
				module_cursor	=> active_module, 
				net_name		=> object_net_name,
				line			=> (line with object_linewidth, object_signal_layer),
				log_threshold	=> log_threshold + 1);
			
			-- Commit the new state of the design:
			commit (POST, verb, noun, log_threshold + 1);
		end add_to_net;


		
	begin -- make_path
		--put_line ("make path"); --to_string (PT.snap_mode));
		
		-- Set the tool being used for this path so that procedure
		-- draw_track (in et_canvas_board_2-draw_conductors)
		-- knows where to get the end point from.
		-- Procedure draw_track computes the bend point
		-- once the edit process has started:
		object_tool := tool;

		-- On the first call of this procedure the start point of the
		-- path will be set.
		
		if not edit_process_running then
			set_start_point;
		else

			-- CASE 1: 
			--  Start a new path only if the given point differs from 
			--  the start point of the current path.
			-- CASE 2:
			--  If the given point is the same as the start point of the current path
			--  then we assume the operator wants to terminate the routing operation.
			if point /= live_path.start_point then -- CASE 1

				-- Complete the path by setting its end point.
				-- The the current bend point (if there is one) into account:
				
				if live_path.bended = NO then
					live_path.end_point := point;

					-- insert a single line:
					line.start_point := live_path.start_point;
					line.end_point   := live_path.end_point;
					add_to_net;
					
				else
					-- The path is bended. The bend point has been computed
					-- interactively while moving the mouse or the cursor.
					-- See for example procedure draw_path in et_canvas_board_2-draw_assy_doc.

					-- insert first line of the path:
					line.start_point := live_path.start_point;
					line.end_point   := live_path.bend_point;
					add_to_net;

					
					-- insert second line of the path:
					live_path.end_point := point;
					line.start_point := live_path.bend_point;
					line.end_point   := live_path.end_point;
					add_to_net;
				end if;

				-- Set start point of path so that a new
				-- path can be drawn:
				live_path.start_point := point;
				
			else -- CASE 2
				reset_edit_process_running;
			end if;
		end if;			
	end make_path;
		

	
end et_canvas_board_tracks;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
