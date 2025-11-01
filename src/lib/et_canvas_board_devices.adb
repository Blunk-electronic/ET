------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD DEVICES                             --
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

with et_generic_module;				use et_generic_module;
with et_canvas_board;				use et_canvas_board;
with et_schematic_ops.units;
with et_schematic_ops.groups;
with et_board_ops.devices;			use et_board_ops.devices;
with et_board_ops.groups;

with et_device_property_level;
with et_devices_electrical;				use et_devices_electrical;
with et_devices_electrical.units;
with et_devices_electrical.packages;	use et_devices_electrical.packages;
with et_devices_non_electrical;			use et_devices_non_electrical;

with et_modes.board;
with et_undo_redo;
with et_commit;
with et_object_status;				use et_object_status;

with et_canvas_board_preliminary_object;	use et_canvas_board_preliminary_object;


package body et_canvas_board_devices is

	use et_canvas_board.pac_canvas;
	use et_canvas_board.pac_device_ops;
	use pac_generic_modules;
	use pac_devices_electrical;
	use pac_devices_non_electrical;
	

	
	procedure reset_preliminary_electrical_device is begin
		reset_edit_process_running;
		object_tool := MOUSE;
		object_device_name := no_name;

		et_schematic_ops.units.reset_status_units (
			active_module, log_threshold + 1);
	end reset_preliminary_electrical_device;



	
	procedure reset_preliminary_non_electrical_device is begin
		reset_edit_process_running;
		object_tool := MOUSE;
		object_device_name := no_name;

		reset_proposed_non_electrical_devices (active_module, log_threshold + 1);
	end reset_preliminary_non_electrical_device;
	

	

	-- Outputs the name of a device in the status bar:
	procedure show_selected_device ( -- CS no need anymore ?
		name			: in type_device_name;
		electrical		: in boolean;
		clarification	: in boolean := false)
	is 
		praeamble_electric     : constant string := "selected device: ";
		praeamble_non_electric : constant string := "selected non-electrical device: ";
	begin
		if electrical then
			if clarification then
				set_status (praeamble_electric & to_string (name)
					& ". " & status_next_object_clarification);
			else
				set_status (praeamble_electric & to_string (name));
			end if;
		
		else
			if clarification then
				set_status (praeamble_non_electric & to_string (name)
					& ". " & status_next_object_clarification);
			else
				set_status (praeamble_non_electric & to_string (name));
			end if;
		end if;
	end show_selected_device;



	procedure show_selected_object (
		object		: in type_object)
	is 
		praeamble_electric     : constant string := "selected device: ";
		praeamble_non_electric : constant string := "selected non-electrical device: ";
	begin
		case object.cat is
			when CAT_ELECTRICAL_DEVICE =>
				set_status (praeamble_electric & get_device_name (object.electrical_device.cursor)
					& ". " & status_next_object_clarification);

			when CAT_NON_ELECTRICAL_DEVICE =>
				set_status (praeamble_non_electric & get_device_name (object.non_electrical_device.cursor)
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
		log (text => "set_first_selected_object_moving ...", level => log_threshold);
		log_indentation_up;
		do_it;
		log_indentation_down;
	end set_first_selected_object_moving;


	

	

	procedure find_objects (
		point : in type_vector_model)
	is 
		use et_modes.board;
		
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


		-- This procedure proposes objects according to
		-- the current verb and noun:		 
		procedure propose is begin
			-- Before locating any objects, previous
			-- proposed or selected objects should be reset
			-- in both schematic and board editor:
			et_schematic_ops.groups.reset_objects (
				active_module, log_threshold + 1);

			et_board_ops.groups.reset_objects (
				active_module, log_threshold + 1);

			
			-- Propose objects according to current verb and noun:
			case verb is
				when VERB_MOVE | VERB_FLIP | VERB_ROTATE | VERB_SHOW =>
					
					case noun is
						when NOUN_DEVICE =>
			
							-- Propose electrical devices in the vicinity of the given point:
							propose_electrical_devices (
								module_cursor	=> active_module,
								catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
								count			=> count_total,
								log_threshold	=> log_threshold + 1);

							-- Propose non-electrical devices in the vicinity of the given point:
							propose_non_electrical_devices (
								module_cursor	=> active_module,
								catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
								count			=> count_total,
								log_threshold	=> log_threshold + 1);

						when others =>
							null; -- CS
					end case;


				when VERB_COPY | VERB_DELETE | VERB_RENAME =>
					
					case noun is
						when NOUN_DEVICE =>

							-- Propose non-electrical devices in the vicinity of the given point:
							propose_non_electrical_devices (
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
		end propose;

		
	begin
		log (text => "find_objects", level => log_threshold);
		log_indentation_up;

		propose;

		log (text => "proposed objects total" & natural'image (count_total),
			 level => log_threshold + 1);

		
		-- Evaluate the number of objects found here:
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
		
		-- Assigns the final position after the move to the selected object.
		-- Resets variable preliminary_object:
		procedure finalize is
			use et_modes.board;
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

				reset_status_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				move_object (
					module_cursor	=> active_module, 
					object			=> object, 
					destination		=> point,
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			
			status_clear;

			reset_editing_process; -- prepare for a new editing process
		end finalize;


		
	begin
		-- Initially the editing process is not running:
		if not edit_process_running then

			-- Set the tool being used:
			object_tool := tool;
			
			if not clarification_pending then
				-- Locate all devices in the vicinity of the given point:
				find_objects (point);
				-- NOTE: If many devices have been found, then
				-- clarification is now pending.

				-- If find_non_electrical_devices has found only one device
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

	
	

	
	

	
-- ROTATE:


	procedure rotate_object (
		position : in type_vector_model)
	is 

		procedure finalize is
			use et_modes.board;
			use et_undo_redo;
			use et_commit;

			object : type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize rotate", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then

				reset_status_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				rotate_object (
					module_cursor	=> active_module, 
					object			=> object, 
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			
			status_clear;

			reset_editing_process; -- prepare for a new editing process
		end finalize;

		

	begin		
		if not clarification_pending then

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
	end rotate_object;






	
	
-- RENAME:


	procedure cb_rename_new_name_entered (
		self : access gtk_entry_record'class) 
	is 
		device_name_new : type_device_name;

		
		-- Renames the selected object:
		procedure finalize is
			use et_modes.board;
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

				reset_status_objects (active_module, log_threshold + 1);
				
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
			device_name := get_device_name (object.non_electrical_device.cursor);
			
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
			when CAT_NON_ELECTRICAL_DEVICE =>
				do_it;

			when others =>
				raise constraint_error; -- CS
		end case;
	end show_rename_window;


	

	

	procedure rename_object (
		point : in type_vector_model)
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




	
	
	

	
-- FLIP / MIRROR:


	procedure flip_object (
		position : in type_vector_model)
	is 

		procedure finalize is
			use et_modes.board;
			use et_undo_redo;
			use et_commit;

			object : type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize flip", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then

				reset_status_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				flip_object (
					module_cursor	=> active_module, 
					object			=> object, 
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			
			status_clear;

			reset_editing_process; -- prepare for a new editing process
		end finalize;

		

	begin		
		if not clarification_pending then

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
	end flip_object;

	








	

-- DELETE:	
	

	procedure delete_object (
		position : in type_vector_model)
	is 

		-- Shows some information in the status bar:
		procedure finalize is
			use et_modes.board;
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

				reset_status_objects (active_module, log_threshold + 1);
				
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
			
			status_clear;

			reset_editing_process; -- prepare for a new editing process
		end finalize;
		

	begin		
		if not clarification_pending then

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
	end delete_object;

	


	

-- SHOW:


	procedure show_object (
		position : in type_vector_model)
	is 

		-- Shows some information in the status bar:
		procedure finalize is
			use et_device_property_level;
			use et_devices_electrical.units;
			
			object : type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize show", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then

				reset_status_objects (active_module, log_threshold + 1);

				-- Show the device:
				show_object (
					module_cursor	=> active_module, 
					object			=> object, 
					log_threshold	=> log_threshold + 1);

				-- Highlight the device in the schematic editor:
				redraw_schematic;


				-- Write some basic information in the status bar:
				case object.cat is
					when CAT_ELECTRICAL_DEVICE =>
						status_clear;
						
						set_status (get_properties (
							device_cursor	=> object.electrical_device.cursor,
							level			=> DEVICE_PROPERTIES_LEVEL_1,						   
							all_units		=> true));

						
					when CAT_NON_ELECTRICAL_DEVICE =>
						status_clear;

						set_status (get_properties (
							device_cursor	=> object.non_electrical_device.cursor,
							level			=> DEVICE_PROPERTIES_LEVEL_1));

						
					when others =>
						status_clear;
						-- CS CAT_PLACEHOLDER ?
				end case;					

				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			

			reset_editing_process; -- prepare for a new editing process
		end finalize;
		

	begin		
		if not clarification_pending then

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

	
	
end et_canvas_board_devices;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
