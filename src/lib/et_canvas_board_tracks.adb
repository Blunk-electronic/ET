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

with ada.containers;


with glib.values;

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
with et_canvas_board_2;

with et_board_ops.conductors;			use et_board_ops.conductors;
with et_modes.board;

with et_nets;
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
		event_handled : boolean := false;
		
		use gdk.types;
		key : gdk_key_type := event.keyval;

		gentry : gtk_gentry := gtk_gentry (combo_entry);
		text : constant string := get_text (gentry);
	begin
		case key is
			when GDK_ESCAPE =>
				reset_object;
				clear_out_properties_box;

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
			-- specified in preliminary_track. In this case the first net of the 
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
	


	
	function get_nearest (
		airwire	: in pac_proposed_airwires.cursor;
		point	: in type_vector_model)
		return type_vector_model
	is
		use pac_proposed_airwires;
		use pac_geometry_brd;
		wire : type_airwire renames element (airwire).wire;
	begin
		return to_point (get_nearest (wire, to_vector (point)));
	end get_nearest;


	

	procedure reset_airwires is begin
		selected_airwire := pac_proposed_airwires.no_element;
		proposed_airwires.clear;
	end reset_airwires;
	

	
	
	function airwire_is_selected (
		airwire_cursor	: in pac_airwires.cursor;
		net_name		: in pac_net_name.bounded_string)
		return boolean
	is 
		use pac_airwires;
		use pac_net_name;
		airwire : type_airwire renames element (airwire_cursor);
	begin
		-- If there are no proposed airwires at all, then there is nothing to do:
		if is_empty (proposed_airwires) then
			return false;
		else
			-- If there is no selected airwire, then there is nothing to do:
			if selected_airwire /= pac_proposed_airwires.no_element then
				if element (selected_airwire).net_name = net_name and element (selected_airwire).wire = airwire then
					return true;
				else 
					return false;
				end if;
			else
				return false;
			end if;
		end if;
	end airwire_is_selected;

	

	
	procedure select_airwire is 
		use pac_net_name;
	begin
		-- On every call of this procedure we advance from one
		-- proposed airwire to the next in a circular manner. So if the end 
		-- of the list is reached, then the cursor selected_airwire
		-- moves back to the start of the list of proposed airwires:
		if next (selected_airwire) /= pac_proposed_airwires.no_element then
			next (selected_airwire);
		else
			selected_airwire := proposed_airwires.first;
		end if;

		-- show the net name of the selected airwire in the status bar
		set_status ("selected net " & to_string (element (selected_airwire).net_name) 
			& ". " & status_next_object_clarification);
		
	end select_airwire;



	
	procedure make_path (
		tool	: in type_tool;
		point	: in type_vector_model)
	is
		line : type_line;

		
		procedure set_start_point is
			use et_pcb_coordinates_2.pac_geometry_brd;
		begin
			case snap_mode is
				when NO_SNAP =>
					-- set start point:
					live_path.start_point := point;

					-- Allow drawing of the path:
					object_ready := true;

				when NEAREST_AIRWIRE =>
					if not clarification_pending then

						proposed_airwires := get_airwires (
							module_cursor	=> active_module, 
							catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
							log_threshold	=> log_threshold + 1);

						case proposed_airwires.length is
							when 0 =>
								-- set start point:
								live_path.start_point := point;

								-- Allow drawing of the path:
								object_ready := true;

							when 1 =>
								live_path.start_point := get_nearest (proposed_airwires.first, point);
								selected_airwire := proposed_airwires.first;
								
								-- Allow drawing of the path:
								object_ready := true;

							when others =>
								set_request_clarification;
								selected_airwire := proposed_airwires.first;
								
						end case;

					else
						live_path.start_point := get_nearest (selected_airwire, point);
								
						-- Allow drawing of the path:
						object_ready := true;

						reset_request_clarification;
					end if;

					
				when NEAREST_OBJECT =>
					null; -- CS
			end case;

			set_status (status_start_point & to_string (live_path.start_point) & ". " &
				status_press_space & status_set_end_point & status_hint_for_abort);

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
		object_tool := tool;

		-- Initally the preliminary_track is NOT ready. Nothing will be drawn.
		-- Upon the first calling of this procedure the start point of the
		-- path will be set.
		
		if not object_ready then

			set_start_point;

		else -- preliminary_track IS ready

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
				object_ready := false;
			end if;
		end if;			
	end make_path;
		




	-- Outputs the selected line in the status bar:
	procedure show_selected_line (
		selected		: in et_board_ops.conductors.type_object_line_net;
		clarification	: in boolean := false)
	is 
		praeamble : constant string := "selected: net ";
		use pac_net_name;
		use et_nets;
		use pac_nets;
		use pac_conductor_lines;
	begin
		if clarification then
			set_status (praeamble & to_string (key (selected.net_cursor)) -- RESET_N
				& " / " & to_string (element (selected.line_cursor), true) -- start/end point/width/layer
				& ". " & status_next_object_clarification);
		else
			set_status (praeamble & to_string (key (selected.net_cursor)) -- RESET_N
				& " / " & to_string (element (selected.line_cursor), true)); -- start/end point/width/layer
		end if;		
	end show_selected_line;



	
	
-- 	procedure select_track is
-- 		use et_object_status;
-- 		selected_line : type_object_line_net;
-- 	begin
-- 		-- On every call of this procedure we advance from one
-- 		-- proposed segment to the next in a circular manner.
-- 
-- 		selected_line := get_first_line (
-- 			module_cursor	=> active_module, 
-- 			flag			=> SELECTED, 
-- 			freetracks		=> false,
-- 			log_threshold	=> log_threshold + 1);
-- 		
-- 		modify_status (
-- 			module_cursor	=> active_module, 
-- 			operation		=> (CLEAR, SELECTED),
-- 			line_cursor		=> selected_line.line_cursor, 
-- 			freetracks		=> false,
-- 			log_threshold	=> log_threshold + 1);
-- 		
-- 		next_proposed_line (
-- 			module_cursor	=> active_module, 
-- 			line			=> selected_line, 
-- 			freetracks		=> false,
-- 			log_threshold	=> log_threshold + 1);
-- 		
-- 		modify_status (
-- 			module_cursor	=> active_module, 
-- 			operation		=> (SET, SELECTED),
-- 			line_cursor		=> selected_line.line_cursor, 
-- 			freetracks		=> false,
-- 			log_threshold	=> log_threshold + 1);
-- 		
-- 		show_selected_line (selected_line, clarification => true);
-- 	end select_track;
	
	

	-- This procedure searches for the first selected object
	-- and sets its status to "moving":
-- 	procedure set_first_selected_object_moving is
-- 		use et_board_ops.conductors;
-- 		use et_object_status;
-- 
-- 		-- use et_board_shapes_and_text.pac_contours;
-- 		-- use pac_segments;
-- 		-- selected_segment : pac_segments.cursor; -- of a contour
-- 
-- 		selected_line : type_object_line_net;
-- 		-- selected_arc : type_arc_segment;
-- 	begin
-- 		log (text => "set_first_selected_object_moving ...", level => log_threshold);
-- 
-- 		selected_line := get_first_line_net (
-- 			module_cursor	=> active_module,
-- 			flag			=> SELECTED, 
-- 			freetracks		=> false,
-- 			log_threshold	=> log_threshold + 1);
-- 		
-- 		-- CS arcs, circles, zones
-- 		
-- 		modify_status (
-- 			module_cursor	=> active_module, 
-- 			line_cursor		=> selected_line.line_cursor, 
-- 			operation		=> (SET, MOVING),
-- 			freetracks		=> false,
-- 			log_threshold	=> log_threshold + 1);
-- 
-- 	end set_first_selected_object_moving;




	
-- 	procedure find_segments (
-- 	   point : in type_vector_model)
-- 	is 
-- 		count_total : natural := 0;
-- 		
-- 		use et_board_ops;
-- 		use et_board_ops.conductors;
-- 
-- 		
-- 		procedure propose_lines (layer : in type_signal_layer) is 
-- 			count : natural := 0;
-- 		begin
-- 			propose_lines (
-- 				module_cursor	=> active_module, 
-- 				point			=> point, 
-- 				layer			=> layer, 
-- 				zone			=> get_catch_zone (et_canvas_board_2.catch_zone),
-- 				count			=> count,
-- 				freetracks		=> false, 
-- 				log_threshold	=> log_threshold + 1);
-- 			
-- 			-- CS arcs, circles
-- 			count_total := count_total + count;
-- 		end propose_lines;
-- 
-- 
-- 		procedure select_first_proposed is 
-- 			proposed_line : type_object_line_net;
-- 			use et_object_status;
-- 		begin
-- 			proposed_line := get_first_line (
-- 				module_cursor	=> active_module,
-- 				flag			=> PROPOSED, 
-- 				freetracks		=> false,
-- 				log_threshold	=> log_threshold + 1);
-- 
-- 			modify_status (
-- 				module_cursor	=> active_module, 
-- 				line_cursor		=> proposed_line.line_cursor, 
-- 				operation		=> (SET, SELECTED),
-- 				freetracks		=> false,
-- 				log_threshold	=> log_threshold + 1);
-- 					
-- 			-- If only one line found, then show it in the status bar:
-- 			if count_total = 1 then
-- 				show_selected_line (proposed_line);		
-- 			end if;
-- 		end select_first_proposed;
-- 
-- 
-- 		use et_modes.board;
-- 		
-- 	begin
-- 		log (text => "locating segments ...", level => log_threshold);
-- 		log_indentation_up;
-- 
-- 		-- Propose  all segments in the vicinity of the given point:
-- 		-- CS should depend on enabled signal layer
-- 		for ly in 1 .. get_deepest_conductor_layer (active_module) loop
-- 			propose_lines (ly);
-- 		end loop;
-- 
-- 		-- CS arcs, zones
-- 		
-- 		-- evaluate the number of segments found here:
-- 		case count_total is
-- 			when 0 =>
-- 				reset_request_clarification;
-- 				reset_preliminary_object;
-- 
-- 				reset_proposed_lines (
-- 					module_cursor	=> active_module, 
-- 					freetracks		=> false,
-- 					log_threshold	=> log_threshold + 1);
-- 
-- 				
-- 			when 1 =>
-- 				object_ready := true;
-- 				select_first_proposed;
-- 
-- 				if verb = VERB_MOVE then
-- 					set_first_selected_object_moving;
-- 				end if;
-- 				
-- 				reset_request_clarification;
-- 
-- 				
-- 			when others =>
-- 				--log (text => "many objects", level => log_threshold + 2);
-- 				set_request_clarification;
-- 
-- 				-- preselect the first segment
-- 				select_first_proposed;
-- 		end case;
-- 		
-- 		log_indentation_down;
-- 	end find_segments;


	

-- MOVE:
	
-- 	procedure move_track (
-- 		tool	: in type_tool;
-- 		point	: in type_vector_model)
-- 	is
-- 
-- 		-- Assigns the final position after the move to the selected segment.
-- 		-- Resets variable preliminary_segment:
-- 		procedure finalize is 
-- 			use et_modes.board;
-- 			use et_undo_redo;
-- 			use et_commit;
-- 
-- 			use et_board_ops.conductors;
-- 			selected_line : type_object_line_net;
-- 
-- 			use pac_conductor_lines;
-- 			use et_object_status;
-- 		begin
-- 			log (text => "finalizing move ...", level => log_threshold);
-- 			log_indentation_up;
-- 
-- 			selected_line := get_first_line (
-- 				module_cursor	=> active_module,
-- 				flag			=> SELECTED, 
-- 				freetracks		=> false,
-- 				log_threshold	=> log_threshold + 1);
-- 
-- 			
-- 			if selected_line.line_cursor /= pac_conductor_lines.no_element then
-- 
-- 				-- Commit the current state of the design:
-- 				commit (PRE, verb, noun, log_threshold + 1);
-- 
-- 					-- case segment.shape is
-- 					-- 	when LINE =>
-- 							move_line (
-- 								module_cursor	=> active_module,
-- 								line			=> element (selected_line.line_cursor),
-- 								point_of_attack	=> object_point_of_attack,
-- 								destination		=> point,
-- 								log_threshold	=> log_threshold);
--        
-- 					-- 	when ARC =>
-- 					-- 		null; -- CS
--      -- 
-- 					-- 	when CIRCLE =>
-- 					-- 		null; -- CS
-- 					-- end case;
-- 
-- 				-- Commit the new state of the design:
-- 				commit (POST, verb, noun, log_threshold + 1);
-- 
-- 			else
-- 				log (text => "nothing to do", level => log_threshold);
-- 			end if;
-- 				
-- 			log_indentation_down;			
-- 			set_status (status_move_track);
-- 			
-- 			reset_preliminary_object;
-- 
-- 			reset_proposed_lines (
-- 				module_cursor	=> active_module, 
-- 				freetracks		=> false,
-- 				log_threshold	=> log_threshold + 1);
-- 			
-- 		end finalize;
-- 			
-- 		
-- 	begin
-- 		-- Initially the preliminary object is not ready.
-- 		if not object_ready then
-- 
-- 			-- Set the tool being used:
-- 			object_tool := tool;
-- 
-- 			object_point_of_attack := point;
-- 			
-- 			if not clarification_pending then
-- 				-- Locate all segments in the vicinity of the given point:
-- 				find_segments (point);
-- 				
-- 				-- NOTE: If many objects have been found, then
-- 				-- clarification is now pending.
-- 
-- 				-- If find_objects has found only one object
-- 				-- then the flag object_ready is set true.
-- 
-- 			else
-- 				-- Here the clarification procedure ends.
-- 				-- An object has been selected via procedure select_object.
-- 				-- By setting the status of the selected object
-- 				-- as "moving", the selected object
-- 				-- will be drawn according to object_point_of_attack and 
-- 				-- the tool position.
-- 				set_first_selected_object_moving;
-- 
-- 				-- Furtheron, on the next call of this procedure
-- 				-- the selected segment will be assigned its final position.
-- 				
-- 				object_ready := true;
-- 				reset_request_clarification;
-- 			end if;
-- 			
-- 		else
-- 			finalize;
-- 		end if;
-- 	end move_track;



	
-- RIPUP:


	
	
-- 	procedure ripup (
-- 		point	: in type_vector_model)
-- 	is
-- 		-- Rips up the selected single segment or the whole net.
-- 		-- Resets variable preliminary_segment:
-- 		procedure finalize is
-- 			use et_modes.board;
-- 			use et_undo_redo;
-- 			use et_commit;
-- 
-- 			use et_board_ops.conductors;
-- 			selected_line : type_object_line_net;
-- 
-- 			use pac_conductor_lines;
-- 			use et_object_status;
-- 
-- 			use et_nets;
-- 			use pac_nets;
-- 			use et_ripup;
-- 		begin
-- 			log (text => "finalizing ripup ...", level => log_threshold);
-- 			log_indentation_up;
-- 
-- 			selected_line := get_first_line (
-- 				module_cursor	=> active_module,
-- 				flag			=> SELECTED, 
-- 				freetracks		=> false,
-- 				log_threshold	=> log_threshold + 1);
-- 			
-- 			
-- 			if selected_line.line_cursor /= pac_conductor_lines.no_element then
-- 
-- 				-- Commit the current state of the design:
-- 				commit (PRE, verb, noun, log_threshold + 1);
-- 				
-- 				-- 	case segment.shape is
-- 				-- 		when LINE =>
-- 							case ripup_mode is
-- 								when SINGLE_SEGMENT =>
-- 									delete_line (
-- 										module_cursor	=> active_module,
-- 										net_name		=> key (selected_line.net_cursor),
-- 										line			=> element (selected_line.line_cursor),
-- 										log_threshold	=> log_threshold);
-- 
-- 								when WHOLE_NET =>
-- 									ripup_net (
-- 										module_cursor	=> active_module,
-- 										net_name		=> key (selected_line.net_cursor),
-- 										log_threshold	=> log_threshold);
-- 							end case;
--        
-- 				-- 		when ARC =>
-- 				-- 			null; -- CS
--     -- 
-- 				-- 		when CIRCLE =>
-- 				-- 			null; -- CS
-- 				-- 	end case;
-- 
-- 				-- Commit the new state of the design:
-- 				commit (POST, verb, noun, log_threshold + 1);
-- 				
-- 			else
-- 				log (text => "nothing to do", level => log_threshold);
-- 			end if;
-- 				
-- 			log_indentation_down;			
-- 			set_status (status_ripup);
-- 			
-- 			reset_preliminary_object;
-- 
-- 			reset_proposed_lines (
-- 				module_cursor	=> active_module, 
-- 				freetracks		=> false,
-- 				log_threshold	=> log_threshold + 1);
-- 			
-- 			reset_ripup_mode;
-- 		end finalize;
-- 
-- 		
-- 	begin
-- 		if not clarification_pending then
-- 			-- Locate all segments in the vicinity of the given point:
-- 			find_segments (point);
-- 			
-- 			-- NOTE: If many segments have been found, then
-- 			-- clarification is now pending.
-- 
-- 			-- If find_segments has found only one segment
-- 			-- then the flag object_ready is set true.
-- 
-- 			if object_ready then
-- 				finalize;
-- 			end if;
-- 		else
-- 			-- Here the clarification procedure ends.
-- 			-- A segment has been selected via procedure select_segment.
-- 
-- 			finalize;
-- 			reset_request_clarification;
-- 		end if;
-- 	end ripup;

	
	
end et_canvas_board_tracks;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
