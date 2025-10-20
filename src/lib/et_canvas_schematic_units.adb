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
with et_device_read;
with et_package_names;
with et_package_variant;
with et_symbol_ports;
with et_device_appearance;
with et_device_purpose;				use et_device_purpose;
with et_device_partcode;			use et_device_partcode;
with et_device_model_names;			use et_device_model_names;
with et_device_value;				use et_device_value;
with et_board_ops.ratsnest;
with et_schematic_ops.groups;

with et_material;
with et_meta;
with et_module_names;				use et_module_names;
with et_modes.schematic;			use et_modes.schematic;

with et_canvas_schematic;			use et_canvas_schematic;

with et_net_strands;
with et_schematic_text;				use et_schematic_text;

with et_commit;
with et_undo_redo;
with et_directory_and_file_ops;
with et_object_status;				use et_object_status;

with et_canvas_schematic_preliminary_object; 	use et_canvas_schematic_preliminary_object;


package body et_canvas_schematic_units is

	use et_canvas_schematic.pac_canvas;
	use et_canvas_schematic.pac_device_ops;




	procedure show_selected_object (
		object		: in type_object)
	is 
		praeamble : constant string := "selected object: ";
	begin
		case object.cat is
			when CAT_UNIT =>
				set_status (praeamble & get_object_name (object.unit)
					& ". " & status_next_object_clarification);

			when CAT_PLACEHOLDER =>
				set_status (praeamble
					& get_object_name (object.placeholder) & " "
					& get_meaning (object.placeholder)
					& ". " & status_next_object_clarification);

				
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
		log (text => "find_objects", level => log_threshold);
		log_indentation_up;

		-- CS: Before locating any objects, previous
		-- proposed or selected objects should be reset:		
		et_schematic_ops.groups.reset_objects (active_module, log_threshold + 1);
		-- CS: Is this a good idea ?
		

		-- Propose objects according to current verb and noun:
		case verb is
			when VERB_COPY | VERB_DELETE | VERB_DRAG | VERB_FETCH | VERB_MOVE 
				| VERB_RENAME | VERB_ROTATE | VERB_SHOW =>
				
				case noun is
					when NOUN_DEVICE | NOUN_UNIT =>
						
						-- Propose units in the vicinity of the given point:
						propose_units (
							module_cursor	=> active_module,
							catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
							count			=> count_total,
							log_threshold	=> log_threshold + 1);

						
					when NOUN_PLACEHOLDER =>
						
						-- Propose placeholders in the vicinity of the given point:
						propose_placeholders (
							module_cursor	=> active_module,
							catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
							count			=> count_total,
							log_threshold	=> log_threshold + 1);

						
					when others =>
						null; -- CS
				end case;


				
			when VERB_SET =>
				case noun is
					when NOUN_VALUE | NOUN_PARTCODE | NOUN_PURPOSE | NOUN_VARIANT =>

						-- Propose units in the vicinity of the given point.
						-- This applies for real devices only:
						propose_units (
							module_cursor	=> active_module,
							catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
							count			=> count_total,
							real_only		=> true,
							log_threshold	=> log_threshold + 1);

					when others =>
						null; -- CS
				end case;
				
						
			when others =>
				null; -- CS 

		end case;
		



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




	


	procedure cb_new_value_entered (
		self : access gtk.gentry.gtk_entry_record'class) 
	is 
		device_value_new : pac_device_value.bounded_string;

		
		-- Sets the value of the selected object:
		procedure finalize is
			use et_modes.schematic;
			use et_undo_redo;
			use et_commit;

			object : type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize set value", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then

				reset_proposed_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);

				-- Do the set value operation:
				set_value (
					module_cursor	=> active_module, 
					object			=> object, 
					new_value		=> device_value_new,
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
		device_value_new := to_value (self.get_text); -- 100R

		-- CS: Precheck device value ?
		-- put_line ("new value entered: " & to_string (device_value_new));
		
		finalize;

		-- If everything was fine, close the window and clean up.
		-- If one of the operations above has raised an exception then
		-- nothing will be cleaned up and the window will remain until the
		-- operator enters a correct property.
		value_window.destroy;

		-- CS
		-- Whatever goes wrong, output the message in the status bar
		-- of the properties window:
		-- exception when event: others =>
		-- 	set_status_properties (exception_message (event));
			
	end cb_new_value_entered;




	
	procedure cb_value_window_destroy (
		window : access gtk_widget_record'class)
	is
	begin
		put_line ("cb_value_window_destroy");
		reset;
	end cb_value_window_destroy;



	


	procedure show_value_window is

		-- Get the first selected object (which is a unit):
		object : constant type_object := get_first_object (
				active_module, SELECTED, log_threshold + 1);

		
		procedure do_it is
			device	: type_device_name; -- IC1
			value	: pac_device_value.bounded_string;
		begin
			-- Get the name of the selected device:
			device := get_device_name (object.unit.device_cursor);

			-- Get the old value of the selected device:
			value := get_value (object.unit.device_cursor);
				
			build_value_window (device);

			-- Connect the "destroy" signal.
			value_window.on_destroy (cb_value_window_destroy'access);

			-- Set the old value in the window:
			value_old.set_text (to_string (value));
	
			-- Connect the "on_activate" signal (emitted when ENTER pressed)
			-- of the entry field for the new value:
			value_new.on_activate (cb_new_value_entered'access);
			
			value_new.grab_focus;
			
			value_window.show_all;

			value_window_open := true;
		end do_it;
		
				
	begin
		case object.cat is
			when CAT_UNIT =>
				do_it;
  
			when others =>
				raise constraint_error; -- CS
		end case;
	end show_value_window;


	


	

	procedure set_value (
		point	: in type_vector_model)
	is begin
		if not value_window_open then
			
			if not clarification_pending then
				-- Locate all objects in the vicinity of the given point:
				find_objects (point);
				-- NOTE: If many objects have been found, then
				-- clarification is now pending.

				-- If find_objects has found only one object,				
				-- then delete the object immediateley.
				if edit_process_running then
					show_value_window;
				end if;
			else
				-- Here the clarification procedure ends.
				-- An object has been selected via procedure clarify_object.
				show_value_window;
			end if;

		end if;
	end set_value;







	

	


	procedure cb_new_purpose_entered (
		self : access gtk.gentry.gtk_entry_record'class) 
	is 
		device_purpose_new : pac_device_purpose.bounded_string;

		
		-- Sets the purpose of the selected object:
		procedure finalize is
			use et_modes.schematic;
			use et_undo_redo;
			use et_commit;

			object : type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize set purpose", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then

				reset_proposed_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);

				-- Do the set purpose operation:
				set_purpose (
					module_cursor	=> active_module, 
					object			=> object, 
					new_purpose		=> device_purpose_new,
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
		device_purpose_new := to_purpose (self.get_text); -- "Brightness Control"

		-- CS: Precheck device purpose ?
		-- put_line ("new purpose entered: " & to_string (device_purpose_new));
		
		finalize;

		-- If everything was fine, close the window and clean up.
		-- If one of the operations above has raised an exception then
		-- nothing will be cleaned up and the window will remain until the
		-- operator enters a correct property.
		purpose_window.destroy;

		-- CS
		-- Whatever goes wrong, output the message in the status bar
		-- of the properties window:
		-- exception when event: others =>
		-- 	set_status_properties (exception_message (event));
			
	end cb_new_purpose_entered;

	



	procedure cb_purpose_window_destroy (
		window : access gtk_widget_record'class)
	is
	begin
		put_line ("cb_purpose_window_destroy");
		reset;
	end cb_purpose_window_destroy;

	

	

	procedure show_purpose_window is

		-- Get the first selected object (which is a unit):
		object : constant type_object := get_first_object (
				active_module, SELECTED, log_threshold + 1);

		
		procedure do_it is
			device	: type_device_name; -- IC1
			purpose	: pac_device_purpose.bounded_string;
		begin
			-- Get the name of the selected device:
			device := get_device_name (object.unit.device_cursor);

			-- Get the old purpose of the selected device:
			purpose := get_purpose (object.unit.device_cursor);
				
			build_purpose_window (device);

			-- Connect the "destroy" signal.
			purpose_window.on_destroy (cb_purpose_window_destroy'access);

			-- Set the old purpose in the window:
			purpose_old.set_text (to_string (purpose));
	
			-- Connect the "on_activate" signal (emitted when ENTER pressed)
			-- of the entry field for the new purpose:
			purpose_new.on_activate (cb_new_purpose_entered'access);
			
			purpose_new.grab_focus;
			
			purpose_window.show_all;

			purpose_window_open := true;
		end do_it;
		
				
	begin
		case object.cat is
			when CAT_UNIT =>
				do_it;
  
			when others =>
				raise constraint_error; -- CS
		end case;
	end show_purpose_window;


	


	procedure set_purpose (
		point	: in type_vector_model)
	is begin
		if not purpose_window_open then
			
			if not clarification_pending then
				-- Locate all objects in the vicinity of the given point:
				find_objects (point);
				-- NOTE: If many objects have been found, then
				-- clarification is now pending.

				-- If find_objects has found only one object,				
				-- then delete the object immediateley.
				if edit_process_running then
					show_purpose_window;
				end if;
			else
				-- Here the clarification procedure ends.
				-- An object has been selected via procedure clarify_object.
				show_purpose_window;
			end if;

		end if;
	end set_purpose;



	
	




	

	procedure cb_new_partcode_entered (
		self : access gtk.gentry.gtk_entry_record'class) 
	is 
		device_partcode_new : pac_device_partcode.bounded_string;

		
		-- Sets the partcode of the selected object:
		procedure finalize is
			use et_modes.schematic;
			use et_undo_redo;
			use et_commit;

			object : type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize set partcode", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then

				reset_proposed_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);

				-- Do the set partcode operation:
				set_partcode (
					module_cursor	=> active_module, 
					object			=> object, 
					new_partcode	=> device_partcode_new,
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
		device_partcode_new := to_partcode (self.get_text); -- R_PAC_S_0805_VAL_100R

		-- CS: Precheck device partcode ?
		-- put_line ("new partcode entered: " & to_string (device_partcode_new));
		
		finalize;

		-- If everything was fine, close the window and clean up.
		-- If one of the operations above has raised an exception then
		-- nothing will be cleaned up and the window will remain until the
		-- operator enters a correct property.
		partcode_window.destroy;

		-- CS
		-- Whatever goes wrong, output the message in the status bar
		-- of the properties window:
		-- exception when event: others =>
		-- 	set_status_properties (exception_message (event));
			
	end cb_new_partcode_entered;

	

	


	procedure cb_partcode_window_destroy (
		window : access gtk_widget_record'class)
	is
	begin
		put_line ("cb_partcode_window_destroy");
		reset;
	end cb_partcode_window_destroy;

	

	
	

	procedure show_partcode_window is

		-- Get the first selected object (which is a unit):
		object : constant type_object := get_first_object (
				active_module, SELECTED, log_threshold + 1);

		
		procedure do_it is
			device	: type_device_name; -- IC1
			partcode	: pac_device_partcode.bounded_string;
		begin
			-- Get the name of the selected device:
			device := get_device_name (object.unit.device_cursor);

			-- Get the old partcode of the selected device:
			partcode := get_partcode (object.unit.device_cursor);
				
			build_partcode_window (device);

			-- Connect the "destroy" signal.
			partcode_window.on_destroy (cb_partcode_window_destroy'access);

			-- Set the old partcode in the window:
			partcode_old.set_text (to_string (partcode));
	
			-- Connect the "on_activate" signal (emitted when ENTER pressed)
			-- of the entry field for the new partcode:
			partcode_new.on_activate (cb_new_partcode_entered'access);
			
			partcode_new.grab_focus;
			
			partcode_window.show_all;

			partcode_window_open := true;
		end do_it;
		
				
	begin
		case object.cat is
			when CAT_UNIT =>
				do_it;
  
			when others =>
				raise constraint_error; -- CS
		end case;
	end show_partcode_window;





	
	


	procedure set_partcode (
		point	: in type_vector_model)
	is begin
		if not partcode_window_open then
			
			if not clarification_pending then
				-- Locate all objects in the vicinity of the given point:
				find_objects (point);
				-- NOTE: If many objects have been found, then
				-- clarification is now pending.

				-- If find_objects has found only one object,				
				-- then delete the object immediateley.
				if edit_process_running then
					show_partcode_window;
				end if;
			else
				-- Here the clarification procedure ends.
				-- An object has been selected via procedure clarify_object.
				show_partcode_window;
			end if;

		end if;
	end set_partcode;








	
-- PACKAGE VARIANT:


	procedure cb_new_package_variant_selected (
		combo : access gtk_combo_box_record'class)
	is 
		-- Get the model and active iter from the combo box:
		model : constant gtk_tree_model := combo.get_model;
		iter : constant gtk_tree_iter := combo.get_active_iter;

		use glib.values;
		name : gvalue;
		
	begin
		log (text => "cb_new_package_variant_selected", level => log_threshold);
		log_indentation_up;
		
		-- Get the variant name of the entry column 0:
		gtk.tree_model.get_value (model, iter, 0, name);
		
		variant_new := to_variant_name (glib.values.get_string (name)); -- S_0805

		log (text => "selected variant: " 
			 & pac_package_variant_name.to_string (variant_new),
			 level => log_threshold + 1);
		
		log_indentation_down;
	end cb_new_package_variant_selected;

	

	


	procedure cb_package_variant_window_destroy (
		window : access gtk_widget_record'class)
	is begin
		log (text => "cb_package_variant_window_destroy", level => log_threshold);
		reset;
	end cb_package_variant_window_destroy;

	


	

	procedure cb_package_variant_apply (
		button : access gtk_button_record'class)
	is 

		-- Sets the package_variant of the selected object:
		procedure finalize is
			use et_modes.schematic;
			use et_undo_redo;
			use et_commit;

			object : type_object := get_first_object (
				active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize set package variant", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then

				reset_proposed_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);

				-- Do the set package variant operation:
				set_package_variant (
					module_cursor	=> active_module, 
					object			=> object, 
					new_variant		=> variant_new,
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
		log (text => "cb_package_variant_apply", level => log_threshold);
		log_indentation_up;
		
		finalize;

		-- put_line ("destroy window");

		-- If everything was fine, close the window and clean up.
		package_variant_window.destroy;

		-- put_line ("done");		
		log_indentation_down;
	end cb_package_variant_apply;



	
	
	

	procedure show_package_variant_window is

		-- Get the first selected object (which is a unit):
		object : constant type_object := get_first_object (
				active_module, SELECTED, log_threshold + 1);

		
		procedure do_it is
			device	: type_device_name; -- IC1

			use pac_package_variant_name;
			variant	: pac_package_variant_name.bounded_string;
		begin
			-- Get the name of the selected device:
			device := get_device_name (object.unit.device_cursor);

			-- Get the old variant of the selected device:
			variant := get_package_variant (object.unit.device_cursor);
				
			build_package_variant_window (object.unit.device_cursor);

			-- Connect the "destroy" signal.
			package_variant_window.on_destroy (cb_package_variant_window_destroy'access);

			-- Set the old variant in the window:
			package_variant_old.set_text (to_string (variant));
	
			-- Connect the "on_changed" signal that
			-- is emitted when an entry for the new variant 
			-- has been selected:
			package_variant_new.on_changed (cb_new_package_variant_selected'access);
			package_variant_new.grab_focus;

			-- Connect the "on_clicked" signal that is emitted
			-- when the operator clicks the "apply"-button:
			package_variant_button_apply.on_clicked (cb_package_variant_apply'access);
			
			package_variant_window.show_all;

			package_variant_window_open := true;
		end do_it;
		
				
	begin
		case object.cat is
			when CAT_UNIT =>
				do_it;
  
			when others =>
				raise constraint_error; -- CS
		end case;
	end show_package_variant_window;


	

	


	procedure set_package_variant (
		point	: in type_vector_model)
	is begin
		if not package_variant_window_open then
			
			if not clarification_pending then
				-- Locate all objects in the vicinity of the given point:
				find_objects (point);
				-- NOTE: If many objects have been found, then
				-- clarification is now pending.

				-- If find_objects has found only one object,				
				-- then delete the object immediateley.
				if edit_process_running then
					show_package_variant_window;
				end if;
			else
				-- Here the clarification procedure ends.
				-- An object has been selected via procedure clarify_object.
				show_package_variant_window;
			end if;

		end if;
	end set_package_variant;



	


	


-- RENAME:

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



	


	procedure cb_rename_window_destroy (
		window : access gtk_widget_record'class)
	is
	begin
		put_line ("cb_rename_window_destroy");
		reset;
	end cb_rename_window_destroy;


	
	
	

	procedure show_rename_window is

		-- Get the first selected object (which is a unit):
		object : constant type_object := get_first_object (
				active_module, SELECTED, log_threshold + 1);


		procedure do_it is
			device_name : type_device_name; -- IC1
		begin
			-- Get the name of the selected device:
			device_name := get_device_name (object.unit.device_cursor);
			
			build_rename_window;

			-- Connect the "destroy" signal.
			rename_window.on_destroy (cb_rename_window_destroy'access);

			-- Set the old name of the device in the window:
			rename_old.set_text (to_string (device_name));

			-- Connect the "on_activate" signal (emitted when ENTER pressed)
			-- of the entry field for the new name:
			rename_new.on_activate (cb_rename_new_name_entered'access);
			
			rename_new.grab_focus;
			
			rename_window.show_all;

			rename_window_open := true;			
		end do_it;
		
		
	begin
		case object.cat is
			when CAT_UNIT =>
				do_it;

			when others =>
				raise constraint_error; -- CS
		end case;
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
	

	

	

	

-- DRAG UNIT:
	

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
				et_schematic_ops.groups.reset_objects (active_module, log_threshold + 1);
				
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

	



	


-- ADD UNIT/DEVICE:

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




	procedure rotate_unit_add is begin
		if unit_add.valid then
			add (unit_add.rotation, 90.0);
		end if;
	end;

	

	
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
		log (text => "cb_package_variant_selected", level => log_threshold);
		log_indentation_up;

		log (text => "selected variant: " & pac_package_variant_name.to_string (unit_add.variant),
			 level => log_threshold + 1);
		-- CS move downward after unit_add.variant assignment
		
		-- Get the variant name of the entry column 0:
		gtk.tree_model.get_value (model, iter, 0, name);
		
		unit_add.variant := to_variant_name (glib.values.get_string (name));

		
		-- Once the operator has started selecting a package variant, the
		-- counter that counts the number of ESC hits until a reset 
		-- is perfomed, must be reset:
		reset_escape_counter;
		

		-- Now the information in unit_add is complete.
		-- By setting the flag "valid" the draw operation of the unit
		-- starts drawing the unit as it is sticking at the current tool
		-- (cursor or mouse):
		unit_add.valid := true;

		focus_canvas;
		
		log_indentation_down;
	end cb_package_variant_selected;




	
	
	
	procedure cb_model_directory_selected (
		button : access gtk_file_chooser_button_record'class) 
	is begin
		log (text => "cb_model_directory_selected", level => log_threshold);
		
		log_indentation_up;
		log (text => "directory: " & button.get_current_folder,
			 level => log_threshold + 1);
		
		log_indentation_down;
	end cb_model_directory_selected;



	



	
	procedure remove_box_package_variant is begin
		if box_package_variant_active then
			-- Remove the box_variant and everything
			-- that is inside:
			box_v4.remove (box_package_variant);
			box_package_variant_active := false;
		end if;
	end;
	


	

	
	procedure cb_device_model_selected (
		button : access gtk_file_chooser_button_record'class) 
	is
		-- The delected device model file (*.dev) is stored here:
		device_model_file : pac_device_model_file.bounded_string;

		use pac_devices_lib;
		-- This cursor points to the device model in the library:
		device_cursor_lib : pac_devices_lib.cursor;

		use pac_unit_name;
		unit_name : pac_unit_name.bounded_string;

		-- If package variants are available, then
		-- they are stored here temporaily:
		use pac_package_variants;
		variants : pac_package_variants.map;

		
		-- This procedure builds the box_variant with a
		-- combo box inside that allows
		-- the operator to select a package variant for the
		-- currently selected device model:
		procedure make_combo_box_package_variant is
			use gtk.label;			
			use gtk.cell_renderer_text;

			label_variant : gtk_label;
			store : gtk_list_store;			
			render	: gtk_cell_renderer_text;

			-- This is the combo box that allows the operator
			-- to select among a list of package variant names.
			-- It is filled with variant names each time the operator selects
			-- a new device model file:
			cbox_package_variant : gtk_combo_box;
			
		begin
			-- put_line ("make_combo_box_package_variant");

			-- If the box already exists, due to a previous
			-- model selection, then it will be removed first
			-- (with all its content) and a new one created:
			remove_box_package_variant;

			-- Create the box_variant and insert it in the properties box:
			gtk_new_vbox (box_package_variant, homogeneous => false);
			pack_start (box_v4, box_package_variant, padding => box_properties_spacing);

			-- Create a label for the box and insert it in box_variant:
			gtk_new (label_variant, "variant");
			pack_start (box_package_variant, label_variant, padding => box_properties_spacing);

			-- Create the storage model for the content of the combo box:
			make_store_for_variants (variants, store);
			
			-- Create the combo box:
			gtk_new_with_model (
				combo_box	=> cbox_package_variant,
				model		=> +store); -- ?

			-- Insert the combo box in box_variant:
			pack_start (box_package_variant, cbox_package_variant, padding => box_properties_spacing);

			-- Connect the "on_changed" signal with procedure
			-- cb_package_variant_selected:
			cbox_package_variant.on_changed (cb_package_variant_selected'access);

			-- The purpose of this stuff is unclear, but it
			-- is required to make the entries in the combo box visible:
			gtk_new (render);
			pack_start (cbox_package_variant, render, expand => true);
			add_attribute (cbox_package_variant, render, "markup", 0); -- column 0

			-- Show the box_variant with all its content:
			box_package_variant.show_all;			

			box_package_variant_active := true;
		end make_combo_box_package_variant;

		

	begin -- cb_device_model_selected
		log (text => "cb_device_model_selected", level => log_threshold);

		-- Once the operator has started selecing a device model, the
		-- counter that counts the number of ESC hits until a reset 
		-- is perfomed, must be reset:
		reset_escape_counter;
		
		log_indentation_up;

		-- Get the name of the device model file from the button:
		device_model_file := to_file_name (button.get_filename);
		
		log (text => "selected device model file: " & to_string (device_model_file), 
			 level => log_threshold + 1);
		
		log_indentation_up;
		
		-- Read the device mode file and store it in the 
		-- rig wide device library.
		-- If the device is already in the library, then nothing happpens:
		et_device_read.read_device (
			file_name		=> device_model_file, -- ../lbr/logic_ttl/7400.dev
			log_threshold	=> log_threshold + 2);
		-- CS add error flag output by read_device and evaluate accordingly.
		-- Wrap follwing actions in a procedure.
		-- CS use device cursor output by read_device instead 
		-- the following statement.
		
		-- Locate the device in the library:
		device_cursor_lib := get_device_model_cursor (device_model_file);

		-- Assign the cursor to the unit_add:
		unit_add.device := device_cursor_lib;

		-- Assign the name of the first unit.
		-- NOTE: When adding a device, the first unit within the device
		-- will be placed first. Further units of the instantiated device
		-- must to be placed via a fetch operation:
		unit_add.name := get_first_unit (device_cursor_lib);

		-- This is about a new device being added to the module.
		-- No value has been assigned yet. For this reason we
		-- assign the default value as defined in the device model.
		-- If the device is virtual, then an empty value will be assigned:
		unit_add.value := get_default_value (device_cursor_lib);
				
		-- In the preview, the total number of units determines whether
		-- to show the unit name as a suffix or not:
		unit_add.total := get_unit_count (unit_add.device);
		
		-- Assign the prospective device name:
		unit_add.device_pre := get_next_available_device_name (
			active_module, get_prefix (device_cursor_lib));
		
		-- Depending on the nature of the device we
		-- offer a selection of package variants. Virtual devices
		-- like supply symbols have no package variants:
		if is_real (device_cursor_lib) then

			-- Get the available package variants.
			-- If the device model does not provide package variants
			-- then the list "variants" is empty:
			variants := get_available_variants (device_cursor_lib);

			-- If more than one package variant is available,
			-- then show the combo box that allows the operator
			-- to select a variant.
			-- Otherwise, if only one variant is available, then
			-- assign its name directly to unit_add:
			if get_variant_count (variants) > 1 then
				-- Create the combo box for the package variant.
				make_combo_box_package_variant;
			else
				remove_box_package_variant;
				unit_add.variant := get_first_package_variant (variants);
			end if;

		else
			-- Virtual devices do not have package variants:
			remove_box_package_variant;
		end if;

		log_indentation_down;
		log_indentation_down;

		status_clear;
		
		-- Now the information in unit_add is complete.
		-- By setting the flag "valid" the draw operation of the unit
		-- starts drawing the unit as it is sticking at the current tool
		-- (cursor or mouse):
		unit_add.valid := true;

		focus_canvas;
	end cb_device_model_selected;



	


	
	
	
	procedure show_device_model_selection is
		use gtk.box;
		use gtk.label;
		use gtk.file_chooser;
		

		-- The button for the directory:		
		button_model_directory : gtk_file_chooser_button;

		-- The button for the model file:
		button_model_file : gtk_file_chooser_button;
		
		
		-- This procedure creates a button by which the operator
		-- selects the directory where a device model can be taken from:
		procedure make_button_directory is 
			box_directory : gtk_vbox;
			label_directory : gtk_label;
		begin
			-- Make a box and insert it in the properties box:
			gtk_new_vbox (box_directory, homogeneous => false);
			pack_start (box_v4, box_directory, padding => box_properties_spacing);

			-- Makre a label for the box and insert it in the box_directory:
			gtk_new (label_directory, "Model Directory");
			pack_start (box_directory, label_directory, padding => box_properties_spacing);

			-- Create a button by which the operator can select a directory:
			gtk_new (
				button		=> button_model_directory,
				title		=> "Select a Directory",
				action		=> ACTION_SELECT_FOLDER);

			-- CS: Currently the button_directory shows only the most important
			-- library path. It would be convenient if the operator would be shown
			-- all preferred library paths sorted by their rank.
			if button_model_directory.set_current_folder_uri (get_top_most_important_library) then
				null; -- Testing the existence of the folder is not required.
			end if;

			-- Insert the button_model_directory in the box_directory:
			pack_start (box_directory, button_model_directory, padding => box_properties_spacing);

			-- Connect the "on_file_set" signal with procedure
			-- cb_model_directory_selected:
			button_model_directory.on_file_set (cb_model_directory_selected'access);
			
			-- NOTE: Key pressed events are handled by the main window.
		end make_button_directory;

		

		-- This procedure creates a button by which the operator
		-- selects the model file (*.dev):
		procedure make_button_model is 
			box_model : gtk_vbox;
			label_model : gtk_label;
			
			use et_directory_and_file_ops;
			use gtk.file_filter;
			file_filter : gtk_file_filter;
		begin
			-- Make a box and insert it in the properties box:
			gtk_new_vbox (box_model, homogeneous => false);
			pack_start (box_v4, box_model, padding => box_properties_spacing);

			-- Make a label for the box and insert it in the box_model:
			gtk_new (label_model, "Model File");
			pack_start (box_model, label_model, padding => box_properties_spacing);

			-- Create a file filter so that only *.dev files are shown
			-- to the operator:
			gtk_new (file_filter);
			add_pattern (file_filter, make_filter_pattern (device_model_file_extension));
			set_name (file_filter, "Device Models");

			-- Create a button by which the operator can select a model file:
			gtk_new (
				button		=> button_model_file,
				title		=> "Select a Device Model",
				action		=> ACTION_OPEN);

			-- Add the file filter to the button_model_file:
			button_model_file.add_filter (file_filter);

			if button_model_file.set_current_folder (button_model_directory.get_current_folder_uri) then
				null; -- Testing the existence of the device model is not required.
			end if;

			-- Insert the button_model_file in the box_model:
			pack_start (box_model, button_model_file, padding => box_properties_spacing);

			-- Connect the "on_file_set" signal with procedure cb_device_model_selected:
			button_model_file.on_file_set (cb_device_model_selected'access);

			-- NOTE: Key pressed events are handled by the main window.
		end make_button_model;
		
		
	begin
		log (text => "show_device_model_selection", level => log_threshold);
		log_indentation_up;
		
		-- Before inserting any widgets, the properties box must be cleared:
		clear_out_properties_box;
		box_package_variant_active := false;

		-- Since the device model selection (re)starts here,
		-- a possible preview must be turned off:
		unit_add.valid := false;

		-- Build the elements of the properties bar:
		make_button_directory;
		make_button_model;

		-- Show the properties box:
		box_v4.show_all;

		log_indentation_down;
	end show_device_model_selection;





	
	
	procedure add_device (
		position	: in type_vector_model)
	is 
		use et_commit;
		use et_undo_redo;
	begin
		log (text => "add_device", level => log_threshold);
		log_indentation_up;
		
		-- Commit the current state of the design:
		commit (PRE, verb, noun, log_threshold);
		
		add_device (
			module_cursor	=> active_module,
			device_model	=> get_device_model_file (unit_add.device),
			variant			=> unit_add.variant,
			destination		=> to_position (position, active_sheet, unit_add.rotation),
			log_threshold	=> log_threshold + 1);

		-- Commit the new state of the design:
		commit (POST, verb, noun, log_threshold);

		status_clear;

		redraw_board;

		-- In case further devices are to be added,
		-- assign the prospective next device name:
		unit_add.device_pre := get_next_available_device_name (
			active_module, get_prefix (unit_add.device));
		
		log_indentation_down;
	end add_device;



	

	

	procedure copy_object (
		tool	: in type_tool;
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
			log (text => "finalize copy", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then

				reset_proposed_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);

				-- Do the copy operation:
				copy_object (
					module_cursor	=> active_module, 
					object			=> object, 
					destination		=> type_position (to_position (point, unit_add.rotation)),
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

				-- If a device has been copied, then the board
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

			-- The preview-object is no longer required:
			reset_unit_add;
			
			reset_editing_process; -- prepare for a new editing process
		end finalize;


		
		-- After the orignial object has been selected, and the the operator is moving 
		-- the pointer or the cursor, a preview of the copied object is attached to
		-- the tool. The "preview object" is floating:
		procedure build_preview is

			-- Get the selected original object:
			object : type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);

		begin
			case object.cat is
				when CAT_UNIT =>
					-- Build the preview of the unit that is going to
					-- be added as part of a new device:
					unit_add.device := get_device_model (object.unit.device_cursor);

					-- If the device is real, then get the package variant:
					if is_real (unit_add.device) then
						unit_add.variant := get_package_variant (object.unit.device_cursor);
					end if;
					
					unit_add.name := get_first_unit (unit_add.device);
					unit_add.value := get_value (object.unit.device_cursor);
					unit_add.total := get_unit_count (object.unit.device_cursor);
					unit_add.device_pre := get_next_available_device_name (active_module, get_prefix (unit_add.device));
					unit_add.rotation := get_rotation (object.unit.unit_cursor);
					unit_add.valid := true;
					
				when others =>
					-- CS
					null;
			end case;
		end build_preview;

		
		
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

				-- Build the floating "preview-object" that is attached
				-- to the tool being used:
				if edit_process_running then
					build_preview;
				end if;
				
			else
				-- Here the clarification procedure ends.
				-- An object has been selected via procedure clarify_object.

				-- Build the floating "preview-object" that is attached
				-- to the tool being used:
				build_preview;

				set_edit_process_running;
				reset_request_clarification;
			end if;

		else
			finalize;
		end if;			
	end copy_object;


	


	









-- FETCH UNIT:


	
	procedure show_fetch_menu is

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

				-- The value of the device is already known. So we
				-- assign to the preliminary unit the value of the parent device.
				-- If the device is virtual, then an empty value will be assigned:
				unit_fetch.value := get_value (object.unit.device_cursor);
				
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
				set_status ("No more units of this device available !");
				reset_unit_add;
			end if;
		end show_menu;


		
	begin -- show_fetch_menu
		put_line ("show_fetch_menu");
		

		case object.cat is
			when CAT_UNIT =>
				device_model := get_device_model_file (object.unit.device_cursor);
				-- put_line ("model " & to_string (device_model));
				
				device_cursor_lib := get_device_model_cursor (device_model);

				-- CS use device_cursor_lib := get_device_model (object.unit.device_cursor);
				-- instead of the statements above.

				units_total := get_unit_count (device_cursor_lib);

				-- collect the names of all units of the selected device:
				-- put_line ("get all units");
				unit_names := get_all_units (device_cursor_lib);				
				
				show_menu;
				
				
			when others =>
				null;
		end case;
		
	end show_fetch_menu;
	




	procedure rotate_unit_fetch is begin
		if unit_fetch.valid then
			add (unit_fetch.rotation, 90.0);
		end if;
	end;
		
	

	

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

		-- In the preview, the total number of units determines whether
		-- to show the unit name as a suffix or not:
		unit_fetch.total := get_unit_count (unit_fetch.device);
		
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
				show_fetch_menu;
			end if;
		else
			-- Here the clarification procedure ends.
			-- An object has been selected
			-- via procedure clarify_object.

			show_fetch_menu;
		end if;


		-- If a unit has been selected from the 
		-- fetch menu, then the data in unit_fetch is regarded
		-- as valid and the unit inserted in the database:
		if unit_fetch.valid then
			finalize;
		end if;

	end fetch_unit;



	


	procedure show_object (
		position : in type_vector_model)
	is 

		-- Shows some information in the status bar:
		procedure finalize is
			object : type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize show", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then

				reset_proposed_objects (active_module, log_threshold + 1);

				-- Show the device:
				show_object (
					module_cursor	=> active_module, 
					object			=> object, 
					log_threshold	=> log_threshold + 1);

				-- Highlight the package in the board editor:
				redraw_board;
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			

			-- CS show some basic information about the device
			-- set_status (status_delete);

			reset_editing_process; -- prepare for a new editing process
		end finalize;
		

	begin		
		if not clarification_pending then

			-- CS: Before locating any objects, previous
			-- proposed or selected objects should be reset:
			-- reset_proposed_objects (active_module, log_threshold + 1);
			-- This action is currently perfomed in procedure find_objects.
			
			-- Locate all objects in the vicinity of the given point:
			find_objects (position);
			-- NOTE: If many objects have been found, then
			-- clarification is now pending.

			-- If find_objects has found only one object
			-- then the flag edit_process_running is set true.
			if edit_process_running then
			 	finalize;
			end if;
			
		else
			-- Here the clarification procedure ends.
			-- An object has been selected via procedure clarify_object.
			reset_request_clarification;
			finalize;
		end if;
	end show_object;


	
	

	
-- SET PROPERTIES SUCH AS NAME, VALUE, PURPOSE, PARCODE

	-- Called when the operator presses ENTER after typing a property in
	-- the properties window.
	-- The properties window will remain open until the operator enters 
	-- a correct property. The status bar of the window shows the error message:	
-- 	procedure property_entered (self : access gtk.gentry.gtk_entry_record'class) is 
-- 		su : type_selected_unit := element (selected_unit);
-- 
-- 		use pac_devices_sch;
-- 
-- 		value	: pac_device_value.bounded_string;
-- 		purpose	: pac_device_purpose.bounded_string;
-- 		variant	: pac_package_variant_name.bounded_string;
-- 
-- 		partcode : pac_device_partcode.bounded_string;
-- 
-- 		
-- 		procedure clean_up is begin
-- 			properties_confirmed := true;
-- 			window_properties.window.destroy;
-- 			reset_request_clarification;
-- 			--status_clear;
-- 			clear_proposed_units;
-- 		-- CS redraw;
-- 		end clean_up;
-- 
-- 		
-- 	begin -- property_entered
-- 		case noun is
-- 			when NOUN_DEVICE =>
-- 				rename_device (
-- 					module_cursor 		=> active_module,
-- 					device_name_before	=> key (su.device), -- IC2
-- 					device_name_after	=> to_device_name (self.get_text), -- IC3
-- 					log_threshold		=> log_threshold + 1);
-- 
-- 				-- CS use rename_device that takes a module cursor and a device cursor
-- 				-- for device_name_before
-- 				
-- 			when NOUN_PARTCODE =>
-- 				partcode := to_partcode (self.get_text);
-- 
-- 				set_partcode (
-- 					module_name		=> key (active_module),
-- 					device_name		=> key (su.device),
-- 					partcode		=> partcode,
-- 					log_threshold	=> log_threshold + 1);
-- 
-- 				-- CS use set_partcode that takes a module cursor and a device cursor
-- 				
-- 			when NOUN_PURPOSE =>
-- 				purpose := to_purpose (self.get_text);
-- 				
-- 				set_purpose (
-- 					module_name		=> key (active_module),
-- 					device_name		=> key (su.device),
-- 					purpose			=> purpose,
-- 					log_threshold	=> log_threshold + 1);
-- 
-- 				-- CS use set_purpose that takes a module cursor and a device cursor
-- 				
-- 			when NOUN_VALUE =>
-- 				value := to_value_with_check (self.get_text);
-- 
-- 				set_value (
-- 					module_name		=> key (active_module),
-- 					device_name		=> key (su.device),
-- 					value			=> value,
-- 					log_threshold	=> log_threshold + 1);
-- 
-- 				-- CS use set_value that takes a module cursor and a device cursor
-- 
-- 			when NOUN_VARIANT =>
-- 				check_variant_name_length (self.get_text);
-- 				variant := to_variant_name (self.get_text);
-- 				check_variant_name_characters (variant);
-- 
-- 				set_variant (
-- 					module			=> active_module,
-- 					device			=> su.device,
-- 					variant			=> variant);
-- 				
-- 			when others => raise constraint_error;
-- 		end case;
-- 
-- 		-- If everything was fine, close the window and clean up.
-- 		-- If one of the operations above has raised an exception then
-- 		-- nothing will be cleaned up and the window will remain until the
-- 		-- operator enters a correct property.
-- 		clean_up;
-- 		
-- 		-- Whatever goes wrong, output the message in the status bar
-- 		-- of the properties window:
-- 		exception when event: others =>
-- 			set_status_properties (exception_message (event));
-- 			
-- 	end property_entered;


	
-- 	procedure window_set_property is
-- 		use gtk.window;
-- 		use gtk.box;
-- 		use gtk.label;
-- 		use gtk.gentry;
-- 		
-- 		box : gtk_vbox;
-- 		label : gtk_label;
-- 		gentry : gtk_gentry;
-- 		
-- 		su : type_selected_unit := element (selected_unit);
-- 
-- 		use pac_devices_sch;
-- 		device_name : constant string := to_string (key (su.device)); -- IC2
-- 
-- 		use pac_package_variant_name;
-- 	begin		
-- 		-- Properties of real devices can be changed.
-- 		-- Virtual devices (like GND symbols) can not be changed.
-- 		if is_real (su.device) then
-- 			build_window_properties;
-- 
-- 			window_properties.window.set_default_size (200, 100);
-- 			window_properties.window.set_resizable (false);
-- 			
-- 			gtk_new_vbox (box);
-- 			add (window_properties.window, box);
-- 
-- 			-- Prepare displaying the old state of the property:
-- 			gtk_new (entry_property_old);
-- 			
-- 			case noun is
-- 				when NOUN_DEVICE =>
-- 					gtk_new (label, "Name of " & device_name);
-- 					set_property_before (device_name); -- IC2
-- 
-- 				when NOUN_PARTCODE =>
-- 					gtk_new (label, "Partcode of " & device_name);
-- 					set_property_before (to_string (get_partcode (su.device)));
-- 
-- 				when NOUN_PURPOSE =>
-- 					gtk_new (label, "Purpose of " & device_name);
-- 					set_property_before (to_string (get_purpose (su.device)));
-- 					
-- 				when NOUN_VALUE =>
-- 					gtk_new (label, "Value of " & device_name);
-- 					set_property_before (to_string (get_value (su.device)));
-- 
-- 				when NOUN_VARIANT =>
-- 					gtk_new (label, "Package variant of " & device_name);
-- 					set_property_before (to_string (get_package_variant (su.device)));
-- 					
-- 				when others => raise constraint_error;
-- 			end case;				
-- 
-- 			pack_start (box, label);
-- 
-- 			-- show the old property:
-- 			gtk_new (label_property_old, "old:");
-- 			pack_start (box, label_property_old);
-- 			pack_start (box, entry_property_old);
-- 
-- 			-- show the new property (will be entered by the operator later):
-- 			gtk_new (label_property_new, "new:");
-- 			pack_start (box, label_property_new);
-- 			
-- 			gtk_new (gentry); -- CS if NOUN_VARIANT propose available variants here
-- 			pack_start (box, gentry);
-- 			gentry.on_activate (property_entered'access);
-- 			gentry.grab_focus;
-- 
-- 			gtk_new (label_properties_status);
-- 			pack_start (box, label_properties_status);
-- 			
-- 			window_properties.window.show_all;
-- 		else
-- 			set_status ("ERROR: Device " & device_name & " is virtual !");
-- 		end if;
-- 	end window_set_property;




	
	
-- 	procedure set_property_selected_unit is
-- 		use et_schematic_ops.units;
-- 	begin
-- 		log (text => "setting property of unit/device after clarification ...", level => log_threshold);
-- 		log_indentation_up;
-- 
-- 		window_set_property;
-- 		
-- 		log_indentation_down;
-- 	end set_property_selected_unit;

	

-- 	procedure show_properties_of_selected_device is
-- 		use et_device_appearance;
-- 		use pac_devices_sch;
-- 		use pac_units;
-- 		
-- 		su		: constant type_selected_unit := element (selected_unit);
-- 
-- 		--device	: constant et_devices.type_device_name := key (su.device);
-- 		
-- 		-- NOTE: In case the unit name is required for some reason,
-- 		-- mind the cursor su.unit can be no_element if all units are selected.
-- 		--unit	: constant pac_unit_name.bounded_string := key (su.unit);
-- 
-- 		model	: constant pac_device_model_file.bounded_string := element (su.device).model;
-- 
-- 		
-- 		function further_properties return string is 
-- 			use et_material;
-- 			var	: constant string := ", variant ";
-- 			pc	: constant string := ", partcode ";
-- 
-- 			use pac_package_variant_name;
-- 		begin
-- 			case element (su.device).appearance is
-- 				when APPEARANCE_PCB =>
-- 					return var & to_string (element (su.device).variant)
-- 						& pc & to_string (element (su.device).partcode);
-- 
-- 				when others => return "";
-- 			end case;
-- 		end further_properties;
-- 
-- 		
-- 	begin
-- 		reset_request_clarification;
-- 		
-- 		set_status ("Properties:"
-- 			& " Model " & to_string (model)
-- 			& further_properties); -- variant, partcode, ...	
-- 		
-- 	end show_properties_of_selected_device;

	
	
end et_canvas_schematic_units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
