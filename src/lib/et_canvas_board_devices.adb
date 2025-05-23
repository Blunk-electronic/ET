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


with et_generic_module;				use et_generic_module;
with et_canvas_board_2;
with et_board_ops.devices;			use et_board_ops.devices;

with et_devices_electrical;			use et_devices_electrical;
with et_devices_non_electrical;		use et_devices_non_electrical;
with et_modes.board;
with et_undo_redo;
with et_commit;
with et_object_status;				use et_object_status;

with et_canvas_board_preliminary_object;	use et_canvas_board_preliminary_object;


package body et_canvas_board_devices is

	use et_canvas_board_2.pac_canvas;
	use pac_generic_modules;
	use pac_devices_sch;
	use pac_devices_non_electric;
	

	
	procedure reset_preliminary_electrical_device is begin
		reset_edit_process_running;
		object_tool := MOUSE;
		object_device_name := no_name;

		reset_proposed_devices (active_module, log_threshold + 1);
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

	
	
	
	procedure clarify_electrical_device is -- CS no need anymore ?
		use et_object_status;
		selected_device : pac_devices_sch.cursor;
	begin
		-- On every call of this procedure we advance from one proposed
		-- device to the next in a circular manner.
		selected_device := get_first_device (active_module, SELECTED, log_threshold + 1);
		
		modify_status (active_module, selected_device, to_operation (CLEAR, SELECTED), log_threshold + 1);
		next_proposed_device (active_module, selected_device, log_threshold + 1);
		modify_status (active_module, selected_device, to_operation (SET, SELECTED), log_threshold + 1);
		
		-- Show the selected device in the status bar
		show_selected_device (name => key (selected_device), electrical => true, clarification => true);		
	end clarify_electrical_device;




	
	
	procedure clarify_non_electrical_device is  -- CS no need anymore ?
		selected_device : pac_devices_non_electric.cursor;
		use et_object_status;
	begin
		-- On every call of this procedure we advance from one proposed
		-- device to the next in a circular manner.
		selected_device := get_first_non_electrical_device (active_module, SELECTED, log_threshold + 1);
		
		modify_status (active_module, selected_device, to_operation (CLEAR, SELECTED), log_threshold + 1);
		next_proposed_non_electrical_device (active_module, selected_device, log_threshold + 1);
		modify_status (active_module, selected_device, to_operation (SET, SELECTED), log_threshold + 1);
		
		-- Show the selected device in the status bar
		show_selected_device (name => key (selected_device), electrical => false, clarification => true);		
	end clarify_non_electrical_device;



	

	
	procedure find_electrical_devices (
		point : in type_vector_model)
	is 
		count : natural := 0;

		
		procedure select_first_proposed is 
			proposed_device : pac_devices_sch.cursor;
			use et_object_status;
		begin
			proposed_device := get_first_device (active_module, PROPOSED, log_threshold + 1);

			modify_status (active_module, proposed_device, to_operation (SET, SELECTED), log_threshold + 1);
			
			-- If only one device found, then show its name in the status bar:
			if count = 1 then
				show_selected_device (name => key (proposed_device), electrical => true);		
			end if;
		end select_first_proposed;
		
		
	begin
		log (text => "locating devices ...", level => log_threshold);
		log_indentation_up;
		
		-- Propose all devices in the vicinity of the given point:
		propose_electrical_devices (
			module_cursor	=> active_module,
			catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
			count			=> count,
			log_threshold	=> log_threshold + 1);
		
		-- Evaluate the number of devices found here:
		case count is
			when 0 =>
				reset_request_clarification;
				reset_preliminary_electrical_device;
				
			when 1 =>
				set_edit_process_running;
				select_first_proposed;
				reset_request_clarification;
				
			when others =>
				-- Preselect the first device among the proposed device:
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;
				select_first_proposed;			
		end case;

		log_indentation_down;
	end find_electrical_devices;
	

	
	
	
	procedure find_non_electrical_devices (
		point : in type_vector_model)
	is 
		count : natural := 0;

		
		procedure select_first_proposed is 
			proposed_device : pac_devices_non_electric.cursor;
			use et_object_status;
		begin
			proposed_device := get_first_non_electrical_device (active_module, PROPOSED, log_threshold + 1);

			modify_status (active_module, proposed_device, to_operation (SET, SELECTED), log_threshold + 1);
			
			-- If only one device found, then show its name in the status bar:
			if count = 1 then
				show_selected_device (name => key (proposed_device), electrical => false);		
			end if;
		end select_first_proposed;

		
	begin
		log (text => "locating non-electrical devices ...", level => log_threshold);
		log_indentation_up;
		
		-- Propose all devices in the vicinity of the given point:
		propose_non_electrical_devices (
			module_cursor	=> active_module,
			catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
			count			=> count,
			log_threshold	=> log_threshold + 1);
		
		-- Evaluate the number of devices found here:
		case count is
			when 0 =>
				reset_request_clarification;
				reset_preliminary_non_electrical_device;
				
			when 1 =>
				set_edit_process_running;
				select_first_proposed;
				reset_request_clarification;
				
			when others =>
				-- Preselect the first device among the proposed device:
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;
				select_first_proposed;
		end case;

		log_indentation_down;
	end find_non_electrical_devices;





	
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

		
	begin
		log (text => "locating non-electrical devices ...", level => log_threshold);
		log_indentation_up;

		
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

	procedure move_electrical_device ( -- CS no need anymore ?
		tool	: in type_tool;
		point	: in type_vector_model)
	is 

		-- Assigns the final position after the move to the selected 
		-- electrical device.
		-- Resets global variable preliminary_electrical_device:
		procedure finalize is
			use et_modes.board;
			use et_undo_redo;
			use et_commit;
			use et_object_status;

			selected_device : pac_devices_sch.cursor;
		begin
			log (text => "finalizing move ...", level => log_threshold);
			log_indentation_up;

			selected_device := get_first_device (active_module, SELECTED, log_threshold + 1);
			
			if selected_device /= pac_devices_sch.no_element then

				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				move_device (
					module_cursor	=> active_module,
					device_name		=> key (selected_device),
					coordinates		=> ABSOLUTE,
					point			=> point,
					log_threshold	=> log_threshold);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			set_status (status_move_device);			
			reset_preliminary_electrical_device;
		end finalize;

		
	begin
		-- Initially the preliminary_electrical_device is not ready.
		if not edit_process_running then

			-- Set the tool being used:
			object_tool := tool;
			
			if not clarification_pending then
				-- Locate all devices in the vicinity of the given point:
				find_electrical_devices (point);
				-- NOTE: If many devices have been found, then
				-- clarification is now pending.

				-- If find_electrical_devices has found only one device
				-- then the flag preliminary_electrical_device.read is set true.
				
			else
				-- Here the clarification procedure ends.
				-- A device has been selected
				-- via procedure clarify_electrical_device.
				-- By setting edit_process_running, the selected
				-- device will be drawn at the tool position
				-- when packages are drawn on the canvas.
				-- Furtheron, on the next call of this procedure
				-- the selected device will be assigned its final position.
				set_edit_process_running;
				reset_request_clarification;
			end if;
			
		else
			-- Finally move the selected device:
			finalize;
		end if;
	end move_electrical_device;



	
	
	procedure move_non_electrical_device (  -- CS no need anymore ?
		tool	: in type_tool;
		point	: in type_vector_model)
	is 
		
		-- Assigns the final position after the move to the selected 
		-- non-electrical device.
		-- Resets global variable preliminary_non_electrical_device:
		procedure finalize is 
			use et_modes.board;
			use et_undo_redo;
			use et_commit;
			use et_object_status;

			selected_device : pac_devices_non_electric.cursor;
		begin
			log (text => "finalizing move ...", level => log_threshold);
			log_indentation_up;

			selected_device := get_first_non_electrical_device (active_module, SELECTED, log_threshold + 1);
			
			if selected_device /= pac_devices_non_electric.no_element then

				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				move_device (
					module_cursor	=> active_module,
					device_name		=> key (selected_device),
					coordinates		=> ABSOLUTE,
					point			=> point,
					log_threshold	=> log_threshold);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);			
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			set_status (status_move_device);			
			reset_preliminary_non_electrical_device;
		end finalize;

		
	begin
		-- Initially the editing process is not running:
		if not edit_process_running then

			-- Set the tool being used:
			object_tool := tool;
			
			if not clarification_pending then
				-- Locate all devices in the vicinity of the given point:
				find_non_electrical_devices (point);
				-- NOTE: If many devices have been found, then
				-- clarification is now pending.

				-- If find_non_electrical_devices has found only one device
				-- then the flag edit_process_running is set true.
			else
				-- Here the clarification procedure ends.
				-- A device has been selected
				-- via procedure clarify_non_electrical_device.
				-- By setting edit_process_running, the selected
				-- device will be drawn at the tool position
				-- when packages are drawn on the canvas.
				-- Furtheron, on the next call of this procedure
				-- the selected device will be assigned its final position.
				set_edit_process_running;
				reset_request_clarification;
			end if;
			
		else
			-- Finally move the selected device:
			finalize;
		end if;
	end move_non_electrical_device;



	

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
			log (text => "finalizing move ...", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then
				
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
			
			set_status (status_move_device);
			
			reset_proposed_objects (active_module, log_threshold + 1);

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

	procedure rotate_electrical_device (
		tool	: in type_tool;
		point	: in type_vector_model)
	is 

		-- Rotates the selected electrical device by default_rotation.
		-- Resets global variable preliminary_electrical_device:
		procedure finalize is
			use et_modes.board;
			use et_undo_redo;
			use et_commit;
			use et_object_status;

			selected_device : pac_devices_sch.cursor;
		begin
			log (text => "finalizing rotation ...", level => log_threshold);
			log_indentation_up;

			selected_device := get_first_device (active_module, SELECTED, log_threshold + 1);
			
			if selected_device /= pac_devices_sch.no_element then

				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				rotate_device (
					module_cursor	=> active_module,
					device_name		=> key (selected_device),
					coordinates		=> RELATIVE,
					rotation		=> default_rotation,
					log_threshold	=> log_threshold);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			set_status (status_rotate_device);			
			reset_preliminary_electrical_device;
		end finalize;


	begin
		-- Set the tool being used:
		object_tool := tool;
		
		if not clarification_pending then
			-- Locate all devices in the vicinity of the given point:
			find_electrical_devices (point);
			-- NOTE: If many devices have been found, then
			-- clarification is now pending.

			-- If find_electrical_devices has found only one device
			-- then rotate that device immediately.
			if edit_process_running then
				finalize;
			end if;

		else
			-- Here the clarification procedure ends.
			-- A device has been selected
			-- via procedure clarify_electrical_device.
			reset_request_clarification;
			finalize;
		end if;
	end rotate_electrical_device;



	
	
	procedure rotate_non_electrical_device (
		tool	: in type_tool;
		point	: in type_vector_model)
	is 

		-- Rotates the selected non-electrical device by default_rotation.
		-- Resets global variable preliminary_non_electrical_device:
		procedure finalize is 
			use et_modes.board;
			use et_undo_redo;
			use et_commit;
			use et_object_status;

			selected_device : pac_devices_non_electric.cursor;
		begin
			log (text => "finalizing rotation ...", level => log_threshold);
			log_indentation_up;

			selected_device := get_first_non_electrical_device (active_module, SELECTED, log_threshold + 1);
			
			if selected_device /= pac_devices_non_electric.no_element then

				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);

				rotate_device (
					module_cursor	=> active_module,
					device_name		=> key (selected_device),
					coordinates		=> RELATIVE,
					rotation		=> default_rotation,
					log_threshold	=> log_threshold);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			set_status (status_rotate_device);			
			reset_preliminary_non_electrical_device;
		end finalize;

		
	begin
		-- Set the tool being used:
		object_tool := tool;
		
		if not clarification_pending then
			-- Locate all devices in the vicinity of the given point:
			find_non_electrical_devices (point);
			-- NOTE: If many devices have been found, then
			-- clarification is now pending.

			-- If find_non_electrical_devices has found only one device
			-- then rotate that device immediately.
			if edit_process_running then
				finalize;
			end if;

		else
			-- Here the clarification procedure ends.
			-- A device has been selected (indicated by cursor selected_non_electrical_device)
			-- via procedure clarify_non_electrical_device.
			reset_request_clarification;
			finalize;
		end if;
	end rotate_non_electrical_device;


	
	

-- FLIP / MIRROR:


	procedure flip_electrical_device (
		tool	: in type_tool;
		point	: in type_vector_model)
	is 
		use et_pcb_sides;

		
		-- Flips the selected electrical device.
		-- Resets global variable preliminary_electrical_device:
		procedure finalize is
			face : type_face;
			use et_modes.board;
			use et_undo_redo;
			use et_commit;
			use et_object_status;

			selected_device : pac_devices_sch.cursor;
		begin
			log (text => "finalizing flipping ...", level => log_threshold);
			log_indentation_up;

			selected_device := get_first_device (active_module, SELECTED, log_threshold + 1);
			
			if selected_device /= pac_devices_sch.no_element then

				face := get_face (selected_device);
				toggle (face);

				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				flip_device (
					module_cursor	=> active_module,
					device_name		=> key (selected_device),
					face			=> face,
					log_threshold	=> log_threshold);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			set_status (status_flip_device);			
			reset_preliminary_electrical_device;
		end finalize;

		
	begin
		-- Set the tool being used:
		object_tool := tool;
		
		if not clarification_pending then
			-- Locate all devices in the vicinity of the given point:
			find_electrical_devices (point);
			-- NOTE: If many devices have been found, then
			-- clarification is now pending.

			-- If find_electrical_devices has found only one device
			-- then flip that device immediately.
			if edit_process_running then
				finalize;
			end if;
			
		else
			-- Here the clarification procedure ends.
			-- A device has been selected
			-- via procedure clarify_electrical_device.
			reset_request_clarification;
			finalize;
		end if;
	end flip_electrical_device;



	
	
	procedure flip_non_electrical_device (
		tool	: in type_tool;
		point	: in type_vector_model)
	is 
		use et_pcb_sides;

		
		procedure finalize is
			face : type_face;
			use et_modes.board;
			use et_undo_redo;
			use et_commit;
			use et_object_status;

			selected_device : pac_devices_non_electric.cursor;
		begin
			log (text => "finalizing flipping ...", level => log_threshold);
			log_indentation_up;

			selected_device := get_first_non_electrical_device (active_module, SELECTED, log_threshold + 1);

			if selected_device /= pac_devices_non_electric.no_element then

				face := get_face (selected_device);
				toggle (face);

				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				flip_device (
					module_cursor	=> active_module,
					device_name		=> key (selected_device),
					face			=> face,
					log_threshold	=> log_threshold);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			set_status (status_flip_device);			
			reset_preliminary_non_electrical_device;
		end finalize;

		
	begin
		-- Set the tool being used:
		object_tool := tool;
		
		if not clarification_pending then
			-- Locate all devices in the vicinity of the given point:
			find_non_electrical_devices (point);
			-- NOTE: If many devices have been found, then
			-- clarification is now pending.

			-- If find_non_electrical_devices has found only one device
			-- then flip that device immediately.
			if edit_process_running then
				finalize;
			end if;
			
		else
			-- Here the clarification procedure ends.
			-- A device has been selected
			-- via procedure clarify_non_electrical_device.
			reset_request_clarification;
			finalize;
		end if;
	end flip_non_electrical_device;




	

-- DELETE:	
	
	procedure delete_non_electrical_device (
		tool	: in type_tool;
		point	: in type_vector_model)
	is 

		procedure finalize is 
			use et_modes.board;
			use et_undo_redo;
			use et_commit;
			use et_object_status;

			selected_device : pac_devices_non_electric.cursor;
		begin
			log (text => "finalizing deletion ...", level => log_threshold);
			log_indentation_up;

			selected_device := get_first_non_electrical_device (active_module, SELECTED, log_threshold + 1);
			
			if selected_device /= pac_devices_non_electric.no_element then

				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				delete_device (
					module_cursor	=> active_module,
					device_name		=> key (selected_device),
					log_threshold	=> log_threshold);

				-- Commit the current state of the design:
				commit (POST, verb, noun, log_threshold + 1);
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			set_status (status_delete_device);			
			reset_preliminary_non_electrical_device;
		end finalize;


	begin
		-- Set the tool being used:
		object_tool := tool;
		
		if not clarification_pending then
			-- Locate all devices in the vicinity of the given point:
			find_non_electrical_devices (point);
			-- NOTE: If many devices have been found, then
			-- clarification is now pending.

			-- If find_non_electrical_devices has found only one device
			-- then delete that device immediately.
			if edit_process_running then
				finalize;
			end if;

		else
			-- Here the clarification procedure ends.
			-- A device has been selected
			-- via procedure clarify_non_electrical_device.
			reset_request_clarification;
			finalize;
		end if;
	end delete_non_electrical_device;

	
end et_canvas_board_devices;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
