------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        CANVAS SCHEMATIC UNITS                            --
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

with ada.text_io;					use ada.text_io;
with ada.exceptions;				use ada.exceptions;

with glib;
with glib.values;

with gdk.types;						use gdk.types;
with gdk.event;						use gdk.event;
with gdk.types.keysyms;				use gdk.types.keysyms;

with gtkada.file_selection;
with gtk.file_chooser_dialog;
with gtk.file_chooser;
with gtk.file_filter;

with gtk.main;
with gtk.widget;					use gtk.widget;
with gtk.window;
with gtk.box;
with gtk.combo_box_text;			use gtk.combo_box_text;
with gtk.label;
with gtk.gentry;
with gtk.menu;
with gtk.cell_renderer_text;		
-- with gtk.cell_layout;        		
with gtk.list_store;				use gtk.list_store;
with gtk.tree_model;				use gtk.tree_model;

with et_sheets;						use et_sheets;
with et_device_rw;
with et_package_names;
with et_package_variant;
with et_symbol_ports;
with et_device_appearance;
with et_device_purpose;				use et_device_purpose;
with et_device_partcode;			use et_device_partcode;
with et_device_model_names;			use et_device_model_names;
with et_device_value;				use et_device_value;
with et_board_ops.ratsnest;

with et_material;
with et_meta;
with et_module_names;				use et_module_names;
with et_modes.schematic;			use et_modes.schematic;

with et_canvas_schematic_2;			use et_canvas_schematic_2;

with et_net_strands;
with et_schematic_ops.nets;
with et_schematic_text;				use et_schematic_text;

with et_commit;
with et_undo_redo;
with et_directory_and_file_ops;
with et_object_status;				use et_object_status;

with et_canvas_schematic_preliminary_object; 	use et_canvas_schematic_preliminary_object;


package body et_canvas_schematic_units is

	use et_canvas_schematic_2.pac_canvas;


	
	procedure clear_proposed_units is begin
		clear (proposed_units);
		selected_unit := pac_proposed_units.no_element;
	end clear_proposed_units;

	
	function collect_units (
		module			: in pac_generic_modules.cursor;
		place			: in type_object_position; -- sheet/x/y
		zone			: in type_zone_radius; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_proposed_units.list
	is
		result : pac_proposed_units.list;

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use pac_devices_sch;
			device_cursor : pac_devices_sch.cursor := module.devices.first;

			
			procedure query_units (
				device_name	: in type_device_name;
				device		: in type_device_sch)
			is
				use pac_units;
				unit_cursor : pac_units.cursor := device.units.first;
			begin
				while unit_cursor /= pac_units.no_element loop
					
					-- We are interested in units on the given sheet only:
					if get_sheet (element (unit_cursor).position) = get_sheet (place) then

						log (text => "probing unit " & to_string (unit_cursor),
							level => log_threshold + 1);

						if in_catch_zone (
							zone	=> set_catch_zone (place.place, zone), 
							point	=> element (unit_cursor).position.place) 
						then
							log_indentation_up;

							log (text => "in zone", level => log_threshold + 1);
							result.append ((device_cursor, unit_cursor));
							
							log_indentation_down;
						end if;
					end if;

					next (unit_cursor);
				end loop;
				
			end query_units;


			
		begin -- query_devices
			while device_cursor /= pac_devices_sch.no_element loop

				log (text => "probing device " & to_string (key (device_cursor)),
					 level => log_threshold + 1);
				log_indentation_up;
					 
				query_element (
					position	=> device_cursor,
					process		=> query_units'access);

				next (device_cursor);

				log_indentation_down;
			end loop;
		end query_devices;

		
	begin -- collect_units
		log (text => "looking up units at " 
			 & to_string (set_catch_zone (place.place, zone)), 
			 level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module,
			process		=> query_devices'access);

		log_indentation_down;
		
		return result;
		
	end collect_units;


	
	procedure clarify_unit is
		use et_schematic_ops.units;
		u : pac_units.cursor;
	begin
		-- On every call of this procedure we must advance from one
		-- unit to the next in a circular manner. So if the end 
		-- of the list is reached, then the cursor selected_unit
		-- moves back to the start of the unit list.
		if next (selected_unit) /= pac_proposed_units.no_element then
			next (selected_unit);
		else
			selected_unit := proposed_units.first;
		end if;

		-- show the selected unit in the status bar
		u := element (selected_unit).unit;
	
		set_status ("selected unit " & to_string (u) 
			& ". " & status_next_object_clarification);
		
	end clarify_unit;





	procedure show_selected_object (
		object		: in type_object)
	is 
		praeamble : constant string := "selected object: ";
	begin
		case object.cat is
			when CAT_UNIT =>
				set_status (praeamble & get_object_name (object.unit)
					& ". " & status_next_object_clarification);

			-- CS placeholders
				
			when CAT_VOID => null; -- CS
		end case;
	end show_selected_object;

	



	
	procedure clarify_object is 

		procedure do_it is
			use pac_objects;
			
			-- Gather all proposed objects:
			proposed_objects : constant pac_objects.list := 
				get_objects (active_module, PROPOSED, log_threshold + 1);

			proposed_object : pac_objects.cursor;

			-- We start with the first object that is currently selected:
			selected_object : type_object := 
				get_first_object (active_module, SELECTED, log_threshold + 1);

		begin
			log (text => "proposed objects total " 
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
			selected_object : constant type_object := 
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
		log (text => "set_first_selected_object_moving", level => log_threshold);
		log_indentation_up;
		do_it;
		log_indentation_down;
	end set_first_selected_object_moving;



	
	


	procedure find_objects (
		point : in type_vector_model)
	is 
		use et_modes.schematic;
		
		-- The total number of objects that have
		-- been proposed:
		count_total : natural := 0;


		-- This procedure searches for the first proposed
		-- object and marks it as "selected":
		procedure select_first_proposed is
			object : type_object := get_first_object (
						active_module, PROPOSED, log_threshold + 1);
		begin
			modify_status (
				active_module, object, to_operation (SET, SELECTED), log_threshold + 1);

			-- If only one object found, then show it in the status bar:
			if count_total = 1 then
				show_selected_object (object);
			end if;
		end select_first_proposed;

		
	begin
		log (text => "locate objects", level => log_threshold);
		log_indentation_up;

		-- Propose objects according to current verb and noun:
		case verb is
			when VERB_DELETE | VERB_DRAG | VERB_FETCH | VERB_MOVE 
				| VERB_RENAME | VERB_ROTATE =>
				
				case noun is
					when NOUN_DEVICE | NOUN_UNIT =>
						
						-- Propose units in the vicinity of the given point:
						propose_units (
							module_cursor	=> active_module,
							catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
							count			=> count_total,
							log_threshold	=> log_threshold + 1);

					when others =>
						null; -- CS
				end case;
						
			when others =>
				null; -- CS 

		end case;
		
		-- CS placeholders


		log (text => "proposed objects total" & natural'image (count_total),
			 level => log_threshold + 1);

		
		-- Evaluate the number of objects found here:
		case count_total is
			when 0 =>
				null; -- nothing to do
				
			when 1 =>
				set_edit_process_running;
				select_first_proposed;

				case verb is
					when VERB_MOVE =>
						set_first_selected_object_moving;
						-- CS ? set_status (status_move);
						
					when VERB_DRAG =>
						set_first_selected_object_moving;

						-- CS ? set_status (status_drag);

						-- Set the net segments which are
						-- connected with the selected unit
						-- as "moving":
						set_segments_moving (active_module, log_threshold + 1);

					when others => null; -- CS
				end case;
				
				reset_request_clarification;
				
			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;
				select_first_proposed;
		end case;
		
		log_indentation_down;
	end find_objects;


	


	
	

	procedure move_object (
		tool	: in type_tool;
		point	: in type_vector_model)
	is 
		
		-- Assigns the final position after the move to the selected object.
		-- Resets variable preliminary_object:
		procedure finalize is
			use et_modes.schematic;
			use et_undo_redo;
			use et_commit;

			object : constant type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize move", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then

				reset_proposed_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				move_object (
					module_cursor	=> active_module, 
					object			=> object, 
					destination		=> point,
					log_threshold	=> log_threshold + 1);
				
				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

				-- If a unit has been moved, then the board
				-- must be redrawn:
				if object.cat = CAT_UNIT then
					redraw_board;
				end if;
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			
			set_status (status_move);
			-- CS clear status bar ?

			reset_editing_process; -- prepare for a new editing process
		end finalize;

		
	begin
		-- Initially the editing process is not running:
		if not edit_process_running then

			-- Set the tool being used:
			object_tool := tool;
			
			if not clarification_pending then
				-- Locate all objects in the vicinity of the given point:
				find_objects (point);
				-- NOTE: If many objects have been found, then
				-- clarification is now pending.

				-- If find_objects has found only one object,				
				-- then the flag edit_process_running is set true.
			else
				-- Here the clarification procedure ends.
				-- An object has been selected via procedure clarify_object.
				-- By setting the status of the selected object
				-- as "moving", the selected object
				-- will be drawn according to the given point and 
				-- the tool position.
				set_first_selected_object_moving;

				-- Furtheron, on the next call of this procedure
				-- the selected object will be assigned its final position.
				
				set_edit_process_running;
				reset_request_clarification;
			end if;
			
		else
			-- Finally move the selected device:
			finalize;
		end if;
	end move_object;





	
	
	
	procedure rotate_object (
		point	: in type_vector_model)
	is 
		
		-- Assigns the final rotation to the selected object:
		procedure finalize is
			use et_modes.schematic;
			use et_undo_redo;
			use et_commit;

			object : constant type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize rotate", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then

				reset_proposed_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				rotate_object (
					module_cursor	=> active_module, 
					object			=> object, 
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

				-- If a unit has been rotated, then the board
				-- must be redrawn:
				if object.cat = CAT_UNIT then
					redraw_board;
				end if;
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			

			-- CS clear status bar
			-- set_status (status_rotate);

			reset_editing_process; -- prepare for a new editing process
		end finalize;

		
	begin
		if not clarification_pending then
			-- Locate all objects in the vicinity of the given point:
			find_objects (point);
			-- NOTE: If many objects have been found, then
			-- clarification is now pending.

			-- If find_objects has found only one object,				
			-- then rotate the object immediatley.
			if edit_process_running then
				finalize;
			end if;
		else
			-- Here the clarification procedure ends.
			-- An object has been selected via procedure clarify_object.
			reset_request_clarification;
			finalize;
		end if;
	end rotate_object;





	


	procedure delete_object (
		point	: in type_vector_model)
	is 
		
		-- Deletes the selected object:
		procedure finalize is
			use et_modes.schematic;
			use et_undo_redo;
			use et_commit;

			object : type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize delete", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then

				reset_proposed_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);

				-- If a whole device is to be deleted, then
				-- we clear the cursor that points to the unit.
				-- This way, procedure delete_object is notified
				-- that the whole device is to be deleted:
				if noun = NOUN_DEVICE then
					object.unit.unit_cursor := pac_units.no_element;
				end if;

				-- Do the delete operation:
				delete_object (
					module_cursor	=> active_module, 
					object			=> object, 
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

				-- If a unit has been deleted, then the board
				-- must be redrawn:
				if object.cat = CAT_UNIT then
					redraw_board;
				end if;
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			

			-- CS clear status bar ?
			-- set_status (status_delete);

			reset_editing_process; -- prepare for a new editing process
		end finalize;

		
	begin
		if not clarification_pending then
			-- Locate all objects in the vicinity of the given point:
			find_objects (point);
			-- NOTE: If many objects have been found, then
			-- clarification is now pending.

			-- If find_objects has found only one object,				
			-- then delete the object immediateley.
			if edit_process_running then
				finalize;
			end if;
		else
			-- Here the clarification procedure ends.
			-- An object has been selected via procedure clarify_object.
			reset_request_clarification;
			finalize;
		end if;
	end delete_object;




	


	procedure cb_rename_new_name_entered (
		self : access gtk.gentry.gtk_entry_record'class) 
	is 
		device_name_new : type_device_name;

		
		-- Renames the selected object:
		procedure finalize is
			use et_modes.schematic;
			use et_undo_redo;
			use et_commit;

			object : constant type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize rename", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then

				reset_proposed_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				rename_object (
					module_cursor	=> active_module, 
					object			=> object, 
					new_name_device	=> device_name_new,
					log_threshold	=> log_threshold + 1);


				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

				redraw_board;
				-- CS redraw schematic ?
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			

			reset_editing_process; -- prepare for a new editing process
		end finalize;

		
		
	begin
		device_name_new := to_device_name (self.get_text); -- IC2

		-- CS: Precheck device name ?
		-- put_line ("new name entered: " & to_string (device_name_new));
		
		finalize;

		-- If everything was fine, close the window and clean up.
		-- If one of the operations above has raised an exception then
		-- nothing will be cleaned up and the window will remain until the
		-- operator enters a correct property.
		rename_window.destroy;

		-- CS
		-- Whatever goes wrong, output the message in the status bar
		-- of the properties window:
		-- exception when event: others =>
		-- 	set_status_properties (exception_message (event));
			
	end cb_rename_new_name_entered;



	

	

	procedure show_rename_window is

		object : constant type_object := get_first_object (
				active_module, SELECTED, log_threshold + 1);

		use pac_devices_sch;
		device_name : type_device_name; -- IC1
		
	begin
		build_rename_window;

		-- Connect the "destroy" signal.
		rename_window.on_destroy (cb_rename_window_destroy'access);


		case object.cat is
			when CAT_UNIT =>
				device_name := key (object.unit.device_cursor);

			when others =>
				raise constraint_error; -- CS
		end case;

		-- Set the text in the window:
		rename_old.set_text (to_string (device_name));

		-- Connect the "on_activate" signal (emitted when ENTER pressed)
		-- of the entry field for the new name:
		rename_new.on_activate (cb_rename_new_name_entered'access);
		-- gtk_entry (rename_window.box.get_child).on_activate (rename_new_name_entered'access);
		
		rename_new.grab_focus;
		
		rename_window.show_all;

		rename_window_open := true;
	end show_rename_window;


	


	
	procedure rename_object (
		point	: in type_vector_model)
	is begin
		if not rename_window_open then
		
			if not clarification_pending then
				-- Locate all objects in the vicinity of the given point:
				find_objects (point);
				-- NOTE: If many objects have been found, then
				-- clarification is now pending.

				-- If find_objects has found only one object,				
				-- then delete the object immediateley.
				if edit_process_running then
					show_rename_window;
				end if;
			else
				-- Here the clarification procedure ends.
				-- An object has been selected via procedure clarify_object.

				show_rename_window;
			end if;
		end if;
	end rename_object;
	

	

	
	


	

	procedure drag_object (
		tool	: in type_tool;
		point	: in type_vector_model)
	is 

		-- Drags the selected object:
		procedure finalize is
			use et_modes.schematic;
			use et_undo_redo;
			use et_commit;

			object : constant type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize drag", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then

				-- Since the drag operation affects both
				-- units and connected net segments, a reset
				-- is required for units and net segments:
				reset_proposed_objects (active_module, log_threshold + 1);
				et_schematic_ops.nets.reset_proposed_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				drag_object (
					module_cursor	=> active_module, 
					object			=> object, 
					destination		=> point,
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

				-- If a unit has been dragged, then the board
				-- must be redrawn:
				if object.cat = CAT_UNIT then
					redraw_board;
				end if; -- CS really required ? Redraw the schematic instead ?
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			

			-- CS clear status bar ?
			-- set_status (status_delete); -- CS correct ?

			reset_editing_process; -- prepare for a new editing process
		end finalize;

		
	begin
		-- Initially the editing process is not running:
		if not edit_process_running then
			
			-- Set the tool being used:
			object_tool := tool;
			
			if not clarification_pending then
				-- Locate all objects in the vicinity of the given point:
				find_objects (point);

				-- NOTE: If many objects have been found, then
				-- clarification is now pending.

				-- If find_objects has found only one object,				
				-- then the flag edit_process_running is set true.
			else
				-- Here the clarification procedure ends.
				-- An object has been selected via procedure clarify_object.
				-- By setting the status of the selected object
				-- as "moving", the selected object
				-- will be drawn according to the given point and 
				-- the tool position.
				set_first_selected_object_moving;
				
				-- Set the net segments which are
				-- connected with the selected unit as "moving":
				set_segments_moving (active_module, log_threshold + 1);

				-- Furtheron, on the next call of this procedure
				-- the selected object will be assigned its final position.

				set_edit_process_running;
				reset_request_clarification;
			end if;

		else
			finalize;
		end if;
	end drag_object;

	



	


-- ADD UNIT/DEVICE

	function get_top_most_important_library return string is
		use et_meta;
		use et_meta.pac_preferred_libraries_schematic;
		all_lib_dirs : pac_preferred_libraries_schematic.list;
		top_lib_dir : pac_preferred_library_schematic.bounded_string;

		use et_directory_and_file_ops;
	begin
		all_lib_dirs := get_preferred_libraries (active_module);
		top_lib_dir := element (all_lib_dirs.first);
		
		--return expand ("$HOME/git/BEL/ET_component_library/devices");
		return expand (to_string (top_lib_dir));
	end get_top_most_important_library;




	
	procedure reset_unit_add is begin
		unit_add := (others => <>);
	end reset_unit_add;


	
	
	

	procedure cb_package_variant_selected (
		combo : access gtk_combo_box_record'class) 
	is
		-- Get the model and active iter from the combo box:
		model : constant gtk_tree_model := combo.get_model;
		iter : constant gtk_tree_iter := combo.get_active_iter;

		use glib.values;
		name : gvalue;
	begin
		put_line ("cb_package_variant_selected");
		
		-- Get the variant name of the entry column 0:
		gtk.tree_model.get_value (model, iter, 0, name);
		
		unit_add.variant := to_variant_name (glib.values.get_string (name));

		unit_add.valid := true;
		reset_escape_counter;
		
	end cb_package_variant_selected;


	
	
	
	procedure cb_model_directory_selected (
		self : access gtk_file_chooser_button_record'class) 
	is begin
		put_line ("cb_model_directory_selected");
		
		put_line (self.get_current_folder);
	end cb_model_directory_selected;



	
-- 	procedure cb_model_directory_key_pressed (
-- 		self : access gtk_file_chooser_button_record'class) 
-- 	is begin
-- 		put_line ("cb_model_directory_key_pressed");
-- 		
-- 		-- put_line (self.get_current_folder);
-- 	end cb_model_directory_key_pressed;



	
	
	
	procedure cb_model_selected (
		self : access gtk_file_chooser_button_record'class) 
	is

		use gtk.menu;
		use gtk.menu_item;
		use et_device_rw;
		use et_device_appearance;
		
		device_model : pac_device_model_file.bounded_string;

		use pac_devices_lib;
		device_cursor_lib : pac_devices_lib.cursor; -- points to the device in the library

		use pac_unit_name;
		unit_name : pac_unit_name.bounded_string;
		
		use pac_variants;
		variants : pac_variants.map;


		procedure make_store_for_variants (
			store : in out gtk_list_store)
		is
			use gtk.list_store;
			
			column_0 : constant := 0; -- for the variant name
			column_1 : constant := 1; -- for the variant index

			entry_structure : glib.gtype_array := (
					column_0 => glib.gtype_string,
					column_1 => glib.gtype_string);

			use gtk.tree_model;
			iter : gtk_tree_iter;			
			index : natural := 0;

			-- Enters the name and index in the storage model:
			procedure query_variant (c : in pac_variants.cursor) is 
				use pac_package_variant_name;
			begin
				store.append (iter);
				set (store, iter, column_0, to_string (key (c)));
				set (store, iter, column_1, natural'image (index));
				index := index + 1;
			end query_variant;

		begin
			-- Create the storage model:
			gtk_new (list_store => store, types => (entry_structure));

			-- Insert the available net names in the storage model:
			variants.iterate (query_variant'access);	
		end make_store_for_variants;


		
		
		procedure make_combo_variant is
			use gtk.box;
			use gtk.label;
			
			use gtk.cell_renderer_text;

			use glib;
			spacing : constant natural := 10;
			

			box_variant : gtk_vbox;
			label_variant : gtk_label;
			store : gtk_list_store;			
			render	: gtk_cell_renderer_text;			

		begin
			put_line ("make_combo_variant");
			
			gtk_new_vbox (box_variant, homogeneous => false);
			pack_start (box_v4, box_variant, padding => guint (spacing));

			gtk_new (label_variant, "variant");
			pack_start (box_variant, label_variant, padding => guint (spacing));

			-- Create the storage model:
			make_store_for_variants (store);
			
			-- Create the combo box:
			gtk_new_with_model (
				combo_box	=> cbox_package_variant,
				model		=> +store); -- ?

			pack_start (box_variant, cbox_package_variant, padding => guint (spacing));
			cbox_package_variant.on_changed (cb_package_variant_selected'access);
			-- CS key pressed escape

			-- The purpose of this stuff is unclear, but it
			-- is required to make the entries visible:
			gtk_new (render);
			pack_start (cbox_package_variant, render, expand => true);
			add_attribute (cbox_package_variant, render, "markup", 0); -- column 0

			box_v4.show_all;
		end make_combo_variant;

		

	begin -- cb_model_selected
		log (text => "cb_model_selected", level => log_threshold);
		log_indentation_up;

		device_model := to_file_name (self.get_filename);
		
		log (text => "selected device model: " & to_string (device_model), level => log_threshold + 1);
		log_indentation_up;
		

		-- Read the device file and store it in the 
		-- rig wide device library.
		-- If the device is already in the library, then nothing happpens:
		read_device (
			file_name		=> device_model, -- ../lbr/logic_ttl/7400.dev
			log_threshold	=> log_threshold + 1);

		-- locate the device in the library
		device_cursor_lib := find (device_library, device_model);

		-- assign the cursor to the device model:
		unit_add.device := device_cursor_lib;

		-- Assign the name of the first unit.
		-- NOTE: When adding a device, the first unit within the device
		-- will be placed first. Further units are to be placed via
		-- fetch operations:
		unit_add.name := get_first_unit (device_cursor_lib);

		-- For a nice preview we also need the total of units provided
		-- the the device:
		unit_add.total := get_unit_count (unit_add.device);

		unit_add.valid := true;

		reset_escape_counter;
		
		-- assign the prospective device name:
		unit_add.device_pre := get_next_device_name (active_module, element (device_cursor_lib).prefix);
		
		-- get the available package variants:
		variants := get_available_variants (device_cursor_lib);
		

		case element (device_cursor_lib).appearance is
			when APPEARANCE_PCB =>
				if length (variants) > 1 then
					make_combo_variant;
				else
					unit_add.variant := key (variants.first);

					clear_out_properties_box;
					
					set_status (status_add);
					-- CS clear ?
				end if;
				
			when APPEARANCE_VIRTUAL => null;
		end case;

		log_indentation_down;
		log_indentation_down;
		
		-- CS exception handler in case read_device fails ?
	end cb_model_selected;



	


	
	
	
	procedure show_model_selection is
		use gtk.box;
		use gtk.label;
		use gtk.file_chooser_dialog;
		use gtk.file_chooser;
		use gtk.file_filter;

		box_directory, box_model : gtk_vbox;
		label_directory, label_model : gtk_label;
		
		use glib;
		spacing : constant natural := 10;

		file_filter : gtk_file_filter;

		use et_directory_and_file_ops;


		
		procedure make_button_directory is begin
			gtk_new_vbox (box_directory, homogeneous => false);
			pack_start (box_v4, box_directory, padding => guint (spacing));

			gtk_new (label_directory, "directory");
			pack_start (box_directory, label_directory, padding => guint (spacing));

			gtk_new (
				button		=> button_model_directory,
				title		=> "Select a device model",
				action		=> ACTION_SELECT_FOLDER);

			-- CS: Currently the button_directory shows only the most important
			-- library path. It would be convenient if the operator would be shown
			-- all preferred library paths sorted by their rank.
			if button_model_directory.set_current_folder_uri (get_top_most_important_library) then
				null; -- Testing the existence of the folder is not required.
			end if;
			
			pack_start (box_directory, button_model_directory, padding => guint (spacing));
			
			button_model_directory.on_file_set (cb_model_directory_selected'access);
			-- button_model_directory.on_key_pressed (cb_model_directory_key_pressed'access);
		end make_button_directory;

		

		procedure make_button_model is begin
			gtk_new_vbox (box_model, homogeneous => false);
			pack_start (box_v4, box_model, padding => guint (spacing));

			gtk_new (label_model, "model");
			pack_start (box_model, label_model, padding => guint (spacing));

			gtk_new (file_filter);
			add_pattern (file_filter, make_filter_pattern (device_model_file_extension));
			set_name (file_filter, "Device Models");
			
			gtk_new (
				button		=> button_model_file,
				title		=> "Select a device model",
				action		=> ACTION_OPEN);

			button_model_file.add_filter (file_filter);

			if button_model_file.set_current_folder (button_model_directory.get_current_folder_uri) then
				null; -- Testing the existence of the device model is not required.
			end if;
			
			pack_start (box_model, button_model_file, padding => guint (spacing));
			
			button_model_file.on_file_set (cb_model_selected'access);
			-- CS key pressed escape
		end make_button_model;
		
		
	begin
		log (text => "show_model_selection", level => log_threshold);
		log_indentation_up;
		
		-- Before inserting any widgets, the properties box must be cleared:
		clear_out_properties_box;

		-- Build the elements of the properties bar:
		make_button_directory;
		make_button_model;

		-- Show the properties box:
		box_v4.show_all;

		log_indentation_down;
	end show_model_selection;





	
	
	procedure drop_unit (
		position	: in type_vector_model)
	is 
		use pac_devices_lib;

		use et_commit;
		use et_undo_redo;
	begin
		-- Commit the current state of the design:
		commit (PRE, verb, noun, log_threshold);

		put_line ("drop_unit");
		
		add_device (
			module_cursor	=> active_module,
			device_model	=> pac_devices_lib.key (unit_add.device),
			variant			=> unit_add.variant,
			destination		=> to_position (position, active_sheet),
			log_threshold	=> log_threshold + 1);

		-- Commit the new state of the design:
		commit (POST, verb, noun, log_threshold);
		
		-- reset_unit_add;

		set_status (status_add);
		-- CS clear ?
		--status_enter_verb;

		-- CS update board, ratsnest
	end drop_unit;



	

	



	
	
	procedure unit_selection_cancelled (
	self : access gtk.menu_shell.gtk_menu_shell_record'class) 
	is begin
		set_status ("Unit selection cancelled");
		reset_unit_add;

		--put_line ("deselected");
	end unit_selection_cancelled;


	
	
	-- CS
	procedure set_position ( -- of a menu
		menu : not null access gtk.menu.gtk_menu_record'class;
		x : out glib.gint;
		y : out glib.gint;
		push_in : out boolean)
	is
		use glib;
		--use et_schematic_coordinates.pac_geometry_sch;

		--cp : type_vector_model := cursor_main.position;
		--mp : type_vector_model := canvas.drawing_to_model (cursor_main.position);
		--vp : type_view_point;

		use gtk.widget;
		
		a : gtk_allocation;
	begin
		canvas.get_allocation (a);

		-- gdk.window.get_position
		
		--vp := canvas.model_to_view (mp);

		--put_line (to_string (vp));
		
		--x := gint (type_view_coordinate (cp.x));
		--y := gint (vp.y);

		x := a.x;
		y := a.y;
		
		push_in := true;
	end set_position;








-- FETCH UNIT:


	
	procedure show_fetch_window is

		object : constant type_object := get_first_object (
				active_module, SELECTED, log_threshold + 1);
		
		use pac_devices_sch;
	
		
		device_model : pac_device_model_file.bounded_string;
		device_cursor_lib : pac_devices_lib.cursor;

		units_total : type_unit_count;

		use pac_unit_names;
		unit_names : pac_unit_names.list;

		use pac_unit_name;
		

		procedure show_menu is
			use gtk.menu;
			use gtk.menu_item;

			menu : gtk_menu; -- the menu
			item : gtk_menu_item; -- an item on the menu
									   
			-- If no units are available, then no menu is to be shown.
			-- So we must count units with this stuff:
			subtype type_units_available is natural range 0 .. type_unit_count'last;
			units_available : type_units_available := 0;
					

			
			procedure query_name (c : in pac_unit_names.cursor) is 
				in_use : boolean := false;

				
				-- Sets the in_use flag if given unit is already in use:
				procedure query_in_use (
					device_name	: in type_device_name;
					device		: in type_device_sch) 
				is
					use pac_units;
				begin
					if contains (device.units, element (c)) then
						in_use := true;
					end if;
				end query_in_use;

				
			begin -- query_name
				-- Test whether the unit is already in use.
				query_element (
					position	=> object.unit.device_cursor,
					process		=> query_in_use'access);

				-- If the unit is available then put its name on the menu:
				if not in_use then -- unit is available
					-- put_line ("unit " & to_string (element (c)) & " available");
					
					units_available := units_available + 1;
					
					-- Build the menu item. NOTE: The actual unit name must be
					-- the 2nd string of the entry. Procedure cb_fetch_menu_unit_select expects
					-- it at this place:
					item := gtk_menu_item_new_with_label (
						"unit " & to_string (element (c)));

					-- Connect the item with the "activate" signal which
					-- in turn calls procedure cb_fetch_menu_unit_select:
					item.on_activate (cb_fetch_menu_unit_select'access);

					menu.append (item);
					item.show;
				end if;
			end query_name;

			
		begin -- show_menu

			-- create the menu
			menu := gtk_menu_new;

			menu.on_cancel (cb_fetch_menu_destroy'access);

			-- Query the available units and add them
			-- to the menu:
			unit_names.iterate (query_name'access);

			-- If no units are available (because all are in use)
			-- then we do not show a menu.
			if units_available > 0 then
				-- put_line ("show units menu");

				unit_fetch.device := device_cursor_lib;
				unit_fetch.device_pre := key (object.unit.device_cursor);
				
				-- Show the menu:
				menu.show;

				-- CS place the menu either at cursor or pointer
				-- position.

				-- Open the menu:
				menu.popup (
					-- CS func => set_position'access,
							
					-- button 0 means: this is not triggered by a key press
					-- or a button click:
					button => 0,
							
					-- get_current_event_time causes the menu to remain
					-- until a 2nd click.
					activate_time => gtk.main.get_current_event_time);
				
				-- put_line ("units menu open");
			else
				set_status ("No more units of device " 
					& to_string (unit_fetch.device_pre)
					& " available !");

				reset_unit_add;
				--set_status (status_fetch);
			end if;
		end show_menu;


		
	begin -- show_fetch_window
		put_line ("show_fetch_window");
		

		case object.cat is
			when CAT_UNIT =>
				device_model := get_device_model_file (object.unit.device_cursor);
				-- put_line ("model " & to_string (device_model));
				
				device_cursor_lib := get_device_model_cursor (device_model);

				units_total := get_unit_count (device_cursor_lib);

				-- collect the names of all units of the selected device:
				-- put_line ("get all units");
				unit_names := get_all_units (device_cursor_lib);				
				
				show_menu;
				
				
			when others =>
				null;
		end case;
		
	end show_fetch_window;
	




	procedure reset_unit_fetch is begin
		unit_fetch := (others => <>);
	end;
	

	
	
	procedure cb_fetch_menu_destroy (
		menu : access gtk.menu_shell.gtk_menu_shell_record'class) 
	is begin
		set_status ("cb_fetch_menu_destroy");

		-- Clean up for next unit to be fetched:
		reset_proposed_units (active_module, log_threshold + 1);
		reset_unit_fetch;
  	end cb_fetch_menu_destroy;




	
	-- Extracts from the selected menu item the unit name.
	procedure cb_fetch_menu_unit_select (
		menu : access gtk.menu_item.gtk_menu_item_record'class)
	is
		
		-- Extract the unit name from field 2 of the menu item:
		unit_name : constant string := get_field_from_line (
			text_in		=> menu.get_label,
			position	=> 2);
	begin
		put_line ("cb_fetch_menu_unit_select");
		
		-- assign the unit to be drawn:
		unit_fetch.name := to_unit_name (unit_name);

		-- Signal procedure draw_units to draw this unit as a preview:
		unit_fetch.valid := true;

		reset_proposed_units (active_module, log_threshold + 1);
		
		set_status ("Device " & to_string (unit_fetch.device_pre) 
			& " unit " & unit_name & " selected.");

		-- exception
		-- 	when event: others =>
		-- 		-- log (text => ada.exceptions.exception_information (event), console => true);
		-- 		log (text => ada.exceptions.exception_information (event));
		
	end cb_fetch_menu_unit_select;
	



	
	
	
	procedure fetch_unit (
		tool	: in type_tool;
		point	: in type_vector_model) 
	is 
		use pac_unit_name;

		
		procedure finalize is 
			use et_commit;
			use et_undo_redo;
		begin
			log (text => "finalize fetch", level => log_threshold);
			log_indentation_up;
						
			reset_proposed_objects (active_module, log_threshold + 1);
			
			-- Commit the current state of the design:
			commit (PRE, verb, noun, log_threshold);
			
			fetch_unit (
				module_cursor	=> active_module,
				device_name		=> unit_fetch.device_pre,
				unit_name		=> unit_fetch.name,
				destination		=> to_position (point, active_sheet, unit_fetch.rotation),
				log_threshold	=> log_threshold + 1);


			-- Commit the current state of the design:
			commit (POST, verb, noun, log_threshold);

			-- Clean up for the next unit to be fetched:
			reset_unit_fetch;			
			reset_request_clarification;
			set_status (status_fetch);

			log_indentation_down;		
		end finalize;


	begin
		-- Set the tool being used:
		primary_tool := tool;

		-- CS test whether the fetch menu is not open ?
		
		if not clarification_pending then
			-- Locate all objects in the vicinity of the given point:
			find_objects (point);
			
			-- NOTE: If many objects have been found, then
			-- clarification is now pending.

			-- If find_objects has found only one object
			-- then the flag edit_process_running is set true.

			if edit_process_running then
				show_fetch_window;
			end if;
		else
			-- Here the clarification procedure ends.
			-- An object has been selected
			-- via procedure clarify_object.

			show_fetch_window;
		end if;


		-- If a unit has been selected from the 
		-- fetch menu, then the data in unit_fetch is regarded
		-- as valid and the unit inserted in the database:
		if unit_fetch.valid then
			finalize;
		end if;

	end fetch_unit;



	
	
	
	
-- PLACEHOLDERS

	procedure clarify_placeholder is
		use pac_unit_name;
		use pac_devices_sch;
		use pac_units;
		u : pac_units.cursor;
		d : pac_devices_sch.cursor;
	begin
		-- On every call of this procedure we must advance from one
		-- placeholder to the next in a circular manner. So if the end 
		-- of the list is reached, then the cursor selected_name_placeholder
		-- moves back to the start of the placeholder list.
		if next (selected_placeholder) /= pac_proposed_placeholders.no_element then
			next (selected_placeholder);
		else
			selected_placeholder := proposed_placeholders.first;
		end if;

		-- show the selected placeholder in the status bar
		u := element (selected_placeholder).unit;
		d := element (selected_placeholder).device;
		
		set_status ("selected placeholder of " 
			& to_string (key (d)) 
			& "."
			& to_string (key (u)) 
			& ". " & status_next_object_clarification);
		
	end clarify_placeholder;

	
	procedure clear_proposed_placeholders is begin
		clear (proposed_placeholders);
		selected_placeholder := pac_proposed_placeholders.no_element;
	end clear_proposed_placeholders;

	
	procedure reset_placeholder is begin
		placeholder_move := (others => <>);
		clear_proposed_placeholders;
	end reset_placeholder;

	
	procedure finalize_move_placeholder (
		destination		: in type_vector_model;
		category		: in type_placeholder_meaning;
		log_threshold	: in type_log_level)
	is
		su : type_selected_unit;

		use pac_devices_sch;
		use pac_units;
	begin
		log (text => "finalizing move placeholder ...", level => log_threshold);
		log_indentation_up;

		if selected_placeholder /= pac_proposed_placeholders.no_element then
			su := element (selected_placeholder);

			move_unit_placeholder (
				module_cursor	=> active_module,
				device_name		=> key (su.device),
				unit_name		=> key (su.unit),
				coordinates		=> ABSOLUTE,
				point			=> destination,
				meaning			=> category,
				log_threshold	=> log_threshold);

			-- CS write a reduced procedure of move_unit_placeholder that takes a 
			-- module cursor, device cursor and unit cursor instead.

		else
			log (text => "nothing to do", level => log_threshold);
		end if;
			
		log_indentation_down;

		set_status (status_move_placeholder);
		
		reset_placeholder;
	end finalize_move_placeholder;


	procedure move_placeholder (
		tool		: in type_tool;
		position	: in type_vector_model;
		category	: in type_placeholder_meaning)
	is begin
		if not placeholder_move.being_moved then

			-- Set the tool being used:
			placeholder_move.tool := tool;
			placeholder_move.category := category;

			if not clarification_pending then
				find_placeholders (
					point		=> position,
					category	=> category);
			else
				placeholder_move.being_moved := true;
				reset_request_clarification;
			end if;
			
		else
			-- Finally assign the position to the
			-- currently selected placeholder:
			finalize_move_placeholder (
				destination		=> position,
				category		=> category,
				log_threshold	=> log_threshold + 1);

		end if;
	end move_placeholder;


	
	
	function collect_placeholders (
		module			: in pac_generic_modules.cursor;
		place			: in type_object_position; -- sheet/x/y
		zone			: in type_zone_radius; -- the circular area around the place
		category		: in type_placeholder_meaning; -- name, value, purpose
		log_threshold	: in type_log_level)
		return pac_proposed_placeholders.list
	is
		result : pac_proposed_placeholders.list;

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use et_device_appearance;
			use pac_devices_sch;
			
			device_cursor : pac_devices_sch.cursor := module.devices.first;

			
			procedure query_units (
				device_name	: in type_device_name;
				device		: in type_device_sch)
			is
				use pac_units;
				unit_cursor : pac_units.cursor := device.units.first;

				placeholder_position : type_vector_model;

				
				procedure test_placeholder_position is 
					pos_abs : type_object_position;
				begin
					-- The current placeholder_position is relative to the unit position.
					-- It must be moved by the unit position in order to get the absolute position:
					move_by (placeholder_position, element (unit_cursor).position.place);

					-- Add the sheet information to the position:
					pos_abs := to_position (placeholder_position, get_sheet (place));
					
					--log (text => to_string (pos_abs), level => log_threshold + 1);

					-- Test whether the placeholder is inside the catch zone around the given place:
					if in_catch_zone (
						zone	=> set_catch_zone (place.place, zone),
						point	=> pos_abs.place) 
					then
						log_indentation_up;

						log (text => "in catch zone", level => log_threshold + 1);
						result.append ((device_cursor, unit_cursor));
						
						log_indentation_down;
					end if;
				end test_placeholder_position;

				
			begin -- query_units
				while unit_cursor /= pac_units.no_element loop
					
					-- We are interested in units on the given sheet only:
					if get_sheet (element (unit_cursor).position) = get_sheet (place) then

						log (text => "probing unit " & to_string (unit_cursor),
							level => log_threshold + 1);

						case category is
							when NAME =>
								-- Get the position of the name placeholder relative to the unit origin:
								placeholder_position := element (unit_cursor).placeholders.name.position;
								test_placeholder_position;

							when VALUE =>
								-- Get the position of the value placeholder relative to the unit origin:
								placeholder_position := element (unit_cursor).placeholders.value.position;
								test_placeholder_position;

							when PURPOSE =>
								-- Get the position of the purpose placeholder relative to the unit origin:
								placeholder_position := element (unit_cursor).placeholders.purpose.position;
								test_placeholder_position;
								
						end case;
					end if;

					next (unit_cursor);
				end loop;
				
			end query_units;

			
		begin -- query_devices
			while device_cursor /= pac_devices_sch.no_element loop

				-- Only real devices have placeholders. Virtual devices are skipped here:
				if element (device_cursor).appearance = APPEARANCE_PCB then
				
					log (text => "probing device " & to_string (key (device_cursor)),
						level => log_threshold + 1);
					log_indentation_up;
						
					query_element (
						position	=> device_cursor,
						process		=> query_units'access);

				end if;
				
				next (device_cursor);

				log_indentation_down;
			end loop;
		end query_devices;

		
	begin -- collect_placeholders
		log (text => "looking up placeholders of category " 
			& enclose_in_quotes (to_string (category))
			& to_string (set_catch_zone (place.place, zone)),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module,
			process		=> query_devices'access);

		log_indentation_down;
		
		return result;
		
	end collect_placeholders;
	
	
	procedure find_placeholders (
		point		: in type_vector_model;
		category	: in type_placeholder_meaning)
	is begin
		log (text => "locating placeholders of category " 
			 & enclose_in_quotes (to_string (category)) & " ...",
			 level => log_threshold);
		log_indentation_up;
		
		-- Collect all placeholders in the vicinity of the given point:
		proposed_placeholders := collect_placeholders (
			module			=> active_module,
			place			=> to_position (point, active_sheet),
			zone			=> get_catch_zone (catch_zone_radius_default),
			category		=> category,
			log_threshold	=> log_threshold + 1);

		
		-- evaluate the number of placeholders found here:
		case length (proposed_placeholders) is
			when 0 =>
				reset_request_clarification;
				reset_placeholder;
				
			when 1 =>
				placeholder_move.being_moved := true;
				selected_placeholder := proposed_placeholders.first;

				set_status (status_move_placeholder);

				reset_request_clarification;
				
			when others =>
				set_request_clarification;

				-- preselect the first placeholder
				selected_placeholder := proposed_placeholders.first;
		end case;

		log_indentation_down;
	end find_placeholders;



	
	procedure rotate_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		unit			: in type_selected_unit;
		category		: in type_placeholder_meaning;
		log_threshold	: in type_log_level)
	is
		rotation : constant et_schematic_coordinates.type_rotation_model := 90.0;

		use pac_devices_sch;
		use pac_units;
		use pac_unit_name;
		

		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_cursor : pac_devices_sch.cursor;

			procedure query_units (
				device_name	: in type_device_name;
				device		: in out type_device_sch) 
			is 
				unit_cursor : pac_units.cursor;

				-- Rotates the placeholder indicated by category
				-- by 90 degree. Since the rotation of placeholders
				-- is documentational, the rotation is always converted
				-- to HORIZONTAL or VERTICAL via function "snap":
				procedure rotate_placeholder (
					name	: in pac_unit_name.bounded_string; -- A
					unit	: in out type_unit) 
				is
					r : type_rotation_model;
					use pac_text_schematic;
				begin
					case category is
						when et_device_placeholders.NAME =>
							r := unit.placeholders.name.rotation + rotation;
							unit.placeholders.name.rotation := snap (r);
							
						when VALUE =>
							r := unit.placeholders.value.rotation + rotation;
							unit.placeholders.value.rotation := snap (r);
							
						when PURPOSE =>
							r := unit.placeholders.purpose.rotation + rotation;
							unit.placeholders.purpose.rotation := snap (r);

					end case;
				end rotate_placeholder;

				
			begin -- query_units
				-- Locate the given unit inside the device:
				unit_cursor := find (device.units, key (unit.unit));

				pac_units.update_element (
					container	=> device.units,
					position	=> unit_cursor,
					process		=> rotate_placeholder'access);
				
			end query_units;

			
		begin -- query_devices
			-- Locate the given device inside the module:
			device_cursor := find (module.devices, key (unit.device));

			update_element (
				container	=> module.devices,
				position	=> device_cursor,
				process		=> query_units'access);
			
		end query_devices;
		
		
	begin -- rotate_placeholder
		log (text => "module " & enclose_in_quotes (to_string (key (module_cursor))) 
			 & " rotating " & to_string (key (unit.device)) & " unit " 
			 & to_string (key (unit.unit)) 
			 & " placeholder " & enclose_in_quotes (to_string (category))
			 & " by" & to_string (rotation) & " degree ...",
			 level => log_threshold);

		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);
		
		log_indentation_down;				
	end rotate_placeholder;



	
	procedure rotate_selected_placeholder (
		category	: in type_placeholder_meaning)
	is begin
		log (text => "rotating placeholder after clarification ...", level => log_threshold);
		log_indentation_up;

		rotate_placeholder (active_module, element (selected_placeholder), category, log_threshold + 1);
		
		log_indentation_down;
	end rotate_selected_placeholder;


	
	
	procedure rotate_placeholder (
		point 		: in type_vector_model;
		category	: in type_placeholder_meaning)
	is begin
		log (text => "rotating placeholder ...",
			 level => log_threshold);

		log_indentation_up;
		
		-- Collect all placeholders in the vicinity of the given point:
		proposed_placeholders := collect_placeholders (
			module			=> active_module,
			place			=> to_position (point, active_sheet),
			zone			=> get_catch_zone (catch_zone_radius_default),
			category		=> category,
			log_threshold	=> log_threshold + 1);

		
		-- evaluate the number of placeholders found here:
		case length (proposed_placeholders) is
			when 0 =>
				reset_request_clarification;
				reset_placeholder;
				
			when 1 =>
				selected_placeholder := proposed_placeholders.first;

				rotate_placeholder (active_module, element (selected_placeholder), category, log_threshold + 1);
				
				set_status (status_rotate_placeholder);

				reset_request_clarification;
				
			when others =>
				set_request_clarification;

				-- preselect the first placeholder
				selected_placeholder := proposed_placeholders.first;
		end case;
		
		log_indentation_down;
	end rotate_placeholder;



	
-- SET PROPERTIES SUCH AS NAME, VALUE, PURPOSE, PARCODE

	-- Called when the operator presses ENTER after typing a property in
	-- the properties window.
	-- The properties window will remain open until the operator enters 
	-- a correct property. The status bar of the window shows the error message:	
	procedure property_entered (self : access gtk.gentry.gtk_entry_record'class) is 
		su : type_selected_unit := element (selected_unit);

		use pac_devices_sch;

		value	: pac_device_value.bounded_string;
		purpose	: pac_device_purpose.bounded_string;
		variant	: pac_package_variant_name.bounded_string;

		partcode : pac_device_partcode.bounded_string;

		
		procedure clean_up is begin
			properties_confirmed := true;
			window_properties.window.destroy;
			reset_request_clarification;
			--status_clear;
			clear_proposed_units;
		-- CS redraw;
		end clean_up;

		
	begin -- property_entered
		case noun is
			when NOUN_DEVICE =>
				rename_device (
					module_cursor 		=> active_module,
					device_name_before	=> key (su.device), -- IC2
					device_name_after	=> to_device_name (self.get_text), -- IC3
					log_threshold		=> log_threshold + 1);

				-- CS use rename_device that takes a module cursor and a device cursor
				-- for device_name_before
				
			when NOUN_PARTCODE =>
				partcode := to_partcode (self.get_text);

				set_partcode (
					module_name		=> key (active_module),
					device_name		=> key (su.device),
					partcode		=> partcode,
					log_threshold	=> log_threshold + 1);

				-- CS use set_partcode that takes a module cursor and a device cursor
				
			when NOUN_PURPOSE =>
				purpose := to_purpose (self.get_text);
				
				set_purpose (
					module_name		=> key (active_module),
					device_name		=> key (su.device),
					purpose			=> purpose,
					log_threshold	=> log_threshold + 1);

				-- CS use set_purpose that takes a module cursor and a device cursor
				
			when NOUN_VALUE =>
				value := to_value_with_check (self.get_text);

				set_value (
					module_name		=> key (active_module),
					device_name		=> key (su.device),
					value			=> value,
					log_threshold	=> log_threshold + 1);

				-- CS use set_value that takes a module cursor and a device cursor

			when NOUN_VARIANT =>
				check_variant_name_length (self.get_text);
				variant := to_variant_name (self.get_text);
				check_variant_name_characters (variant);

				set_variant (
					module			=> active_module,
					device			=> su.device,
					variant			=> variant);
				
			when others => raise constraint_error;
		end case;

		-- If everything was fine, close the window and clean up.
		-- If one of the operations above has raised an exception then
		-- nothing will be cleaned up and the window will remain until the
		-- operator enters a correct property.
		clean_up;
		
		-- Whatever goes wrong, output the message in the status bar
		-- of the properties window:
		exception when event: others =>
			set_status_properties (exception_message (event));
			
	end property_entered;


	
	procedure window_set_property is
		use gtk.window;
		use gtk.box;
		use gtk.label;
		use gtk.gentry;
		
		box : gtk_vbox;
		label : gtk_label;
		gentry : gtk_gentry;
		
		su : type_selected_unit := element (selected_unit);

		use pac_devices_sch;
		device_name : constant string := to_string (key (su.device)); -- IC2

		use pac_package_variant_name;
	begin		
		-- Properties of real devices can be changed.
		-- Virtual devices (like GND symbols) can not be changed.
		if is_real (su.device) then
			build_window_properties;

			window_properties.window.set_default_size (200, 100);
			window_properties.window.set_resizable (false);
			
			gtk_new_vbox (box);
			add (window_properties.window, box);

			-- Prepare displaying the old state of the property:
			gtk_new (entry_property_old);
			
			case noun is
				when NOUN_DEVICE =>
					gtk_new (label, "Name of " & device_name);
					set_property_before (device_name); -- IC2

				when NOUN_PARTCODE =>
					gtk_new (label, "Partcode of " & device_name);
					set_property_before (to_string (get_partcode (su.device)));

				when NOUN_PURPOSE =>
					gtk_new (label, "Purpose of " & device_name);
					set_property_before (to_string (get_purpose (su.device)));
					
				when NOUN_VALUE =>
					gtk_new (label, "Value of " & device_name);
					set_property_before (to_string (get_value (su.device)));

				when NOUN_VARIANT =>
					gtk_new (label, "Package variant of " & device_name);
					set_property_before (to_string (get_package_variant (su.device)));
					
				when others => raise constraint_error;
			end case;				

			pack_start (box, label);

			-- show the old property:
			gtk_new (label_property_old, "old:");
			pack_start (box, label_property_old);
			pack_start (box, entry_property_old);

			-- show the new property (will be entered by the operator later):
			gtk_new (label_property_new, "new:");
			pack_start (box, label_property_new);
			
			gtk_new (gentry); -- CS if NOUN_VARIANT propose available variants here
			pack_start (box, gentry);
			gentry.on_activate (property_entered'access);
			gentry.grab_focus;

			gtk_new (label_properties_status);
			pack_start (box, label_properties_status);
			
			window_properties.window.show_all;
		else
			set_status ("ERROR: Device " & device_name & " is virtual !");
		end if;
	end window_set_property;



	
	procedure set_property (point : in type_vector_model) is begin
		-- If the properties window is already open, then it
		-- is moved to the foreground.
		if not window_properties_is_open then
			log (text => "setting property ...", level => log_threshold);
			log_indentation_up;
			
			-- Collect all units in the vicinity of the given point:
			proposed_units := collect_units (
				module			=> active_module,
				place			=> to_position (point, active_sheet),
				zone			=> get_catch_zone (catch_zone_radius_default),
				log_threshold	=> log_threshold + 1);

			
			-- evaluate the number of units found here:
			case length (proposed_units) is
				when 0 =>
					reset_request_clarification;
					
				when 1 =>
					selected_unit := proposed_units.first;

					window_set_property;

				when others =>
					set_request_clarification;

					-- preselect the first unit
					selected_unit := proposed_units.first;
			end case;

			log_indentation_down;
		else
			log (text => "Window to set properties already open !", level => log_threshold);

			-- Move the properties window to the foreground so that the operator
			-- is notified about the already open properties window:
			window_properties.window.present;
		end if;
	end set_property;

	
	
	procedure set_property_selected_unit is
		use et_schematic_ops.units;
	begin
		log (text => "setting property of unit/device after clarification ...", level => log_threshold);
		log_indentation_up;

		window_set_property;
		
		log_indentation_down;
	end set_property_selected_unit;

	

	procedure show_properties_of_selected_device is
		use et_device_appearance;
		use pac_devices_sch;
		use pac_units;
		
		su		: constant type_selected_unit := element (selected_unit);

		--device	: constant et_devices.type_device_name := key (su.device);
		
		-- NOTE: In case the unit name is required for some reason,
		-- mind the cursor su.unit can be no_element if all units are selected.
		--unit	: constant pac_unit_name.bounded_string := key (su.unit);

		model	: constant pac_device_model_file.bounded_string := element (su.device).model;

		
		function further_properties return string is 
			use et_material;
			var	: constant string := ", variant ";
			pc	: constant string := ", partcode ";

			use pac_package_variant_name;
		begin
			case element (su.device).appearance is
				when APPEARANCE_PCB =>
					return var & to_string (element (su.device).variant)
						& pc & to_string (element (su.device).partcode);

				when others => return "";
			end case;
		end further_properties;

		
	begin
		reset_request_clarification;
		
		set_status ("Properties:"
			& " Model " & to_string (model)
			& further_properties); -- variant, partcode, ...	
		
	end show_properties_of_selected_device;



	
	procedure find_units_for_show (point : in type_vector_model) is 
		use et_modes.schematic;
	begin
		log (text => "locating units for show ...", level => log_threshold);
		log_indentation_up;
		
		-- Collect all units in the vicinity of the given point:
		proposed_units := collect_units (
			module			=> active_module,
			place			=> to_position (point, active_sheet),
			zone			=> get_catch_zone (catch_zone_radius_default),
			log_threshold	=> log_threshold + 1);

		
		-- evaluate the number of units found here:
		case length (proposed_units) is
			when 0 =>
				reset_request_clarification;
				
			when 1 =>
				selected_unit := proposed_units.first;
				show_properties_of_selected_device;
				
			when others =>
				set_request_clarification;

				-- preselect the first unit
				selected_unit := proposed_units.first;
		end case;
		
		log_indentation_down;
	end find_units_for_show;

	
end et_canvas_schematic_units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
