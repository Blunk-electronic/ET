------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      CANVAS SCHEMATIC / NETCHANGERS                      --
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

with ada.text_io;					use ada.text_io;
with ada.exceptions;				use ada.exceptions;

with glib;
with glib.values;

with gdk.types;						use gdk.types;
with gdk.event;						use gdk.event;
with gdk.types.keysyms;				use gdk.types.keysyms;

with et_sheets;							use et_sheets;
with et_device_property_level;
with et_board_ops_groups;
with et_schematic_ops_groups;
with et_schematic_ops_netchangers;		use et_schematic_ops_netchangers;

with et_module_names;				use et_module_names;
with et_modes.schematic;			use et_modes.schematic;

with et_canvas_schematic;			use et_canvas_schematic;

with et_net_strands;

with et_commit;
with et_undo_redo;
with et_directory_and_file_ops;
with et_object_status;				use et_object_status;

with et_canvas_schematic_preliminary_object; 	use et_canvas_schematic_preliminary_object;


package body et_canvas_schematic_netchangers is

	use et_canvas_schematic.pac_canvas;
	use et_canvas_schematic.pac_device_ops;




	procedure show_selected_object (
		object		: in type_object)
	is 
		praeamble : constant string := "selected object: ";
	begin
		case object.cat is
			when CAT_NETCHANGER =>
				set_status (praeamble & get_object_name (object.netchanger)
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


		-- This procedure proposes objects according to
		-- the current verb and noun:
		procedure propose is begin
			-- Before locating any objects, previous
			-- proposed or selected objects should be reset
			-- in both schematic and board editor:
			et_schematic_ops_groups.reset_objects (
				active_module, log_threshold + 1);

			et_board_ops_groups.reset_objects (
				active_module, log_threshold + 1);


			-- Propose objects according to current verb and noun:
			case verb is
				when VERB_COPY | VERB_DELETE | VERB_DRAG | VERB_FETCH | VERB_MOVE 
					| VERB_RENAME | VERB_ROTATE | VERB_SHOW =>
					
					case noun is
						when NOUN_NETCHANGER =>
							
							-- Propose netchangers in the vicinity of the given point:
							propose_netchangers (
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

				case verb is
					when VERB_MOVE =>
						set_first_selected_object_moving;
						-- CS ? set_status (status_move);
						
					when VERB_DRAG =>
						set_first_selected_object_moving;

						-- CS ? set_status (status_drag);

						-- Set the net segments which are
						-- connected with the selected netchanger
						-- as "moving":
						-- CS set_segments_moving (active_module, log_threshold + 1);

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

				-- If a unit has been moved, then the board
				-- must be redrawn:
				if object.cat = CAT_NETCHANGER then
					redraw_board;
				end if;
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			
			set_status (status_move_netchanger);
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

				reset_status_objects (active_module, log_threshold + 1);
				
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
				if object.cat = CAT_NETCHANGER then
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

				reset_status_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);

				-- Do the delete operation:
				delete_object (
					module_cursor	=> active_module, 
					object			=> object, 
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

				-- If a unit has been deleted, then the board
				-- must be redrawn:
				if object.cat = CAT_NETCHANGER then
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





	



	


-- RENAME:

	procedure cb_rename_new_name_entered (
		self : access gtk_entry_record'class) 
	is 
		name_new : type_netchanger_id;

		
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

				reset_status_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				rename_object (
					module_cursor	=> active_module, 
					object			=> object, 
					new_name		=> name_new,
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
		name_new := to_netchanger_id (self.get_text); -- 1, 2, 3, ..

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
			netchanger_id : type_netchanger_id; -- 1, 2, 3, ...
		begin
			-- Get the name of the selected netchanger:
			netchanger_id := get_object_name (object.netchanger);
			
			build_rename_window;

			-- Connect the "destroy" signal.
			rename_window.on_destroy (cb_rename_window_destroy'access);

			-- Set the old name of the netchanger in the window:
			rename_old.set_text (to_string (netchanger_id));

			-- Connect the "on_activate" signal (emitted when ENTER pressed)
			-- of the entry field for the new name:
			rename_new.on_activate (cb_rename_new_name_entered'access);
			
			rename_new.grab_focus;
			
			rename_window.show_all;

			rename_window_open := true;			
		end do_it;
		
		
	begin
		case object.cat is
			when CAT_NETCHANGER =>
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
				-- netchangers and connected net segments, a reset
				-- is required for netchangers and net segments:
				et_schematic_ops_groups.reset_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				drag_object (
					module_cursor	=> active_module, 
					object			=> object, 
					destination		=> point,
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

				-- If a netchanger has been dragged, then the board
				-- must be redrawn:
				if object.cat = CAT_NETCHANGER then
					redraw_board;
				end if; -- CS really required ? Redraw the schematic instead ?
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			

			-- clear status bar
			status_clear;

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
				-- CS
				-- set_segments_moving (active_module, log_threshold + 1);

				-- Furtheron, on the next call of this procedure
				-- the selected object will be assigned its final position.

				set_edit_process_running;
				reset_request_clarification;
			end if;

		else
			finalize;
		end if;
	end drag_object;

	



	





	procedure rotate_netchanger_add is begin
		if netchanger_add.valid then
			-- CS
			null;
			-- add (netchanger_add.rotation, 90.0);
		end if;
	end;

	

	
	procedure reset_netchanger_add is begin
		netchanger_add := (others => <>);
	end;


	
	
	




	
	
	procedure add_netchanger (
		place : in type_vector_model)
	is 
		use et_commit;
		use et_undo_redo;
	begin
		log (text => "add_netchanger", level => log_threshold);
		log_indentation_up;
		
		-- Commit the current state of the design:
		commit (PRE, verb, noun, log_threshold);
		

		-- CS
		-- add_netchanger (
		-- 	module_cursor	=> active_module,
		-- 	place			=> to_position (place, active_sheet, netchanger_add.rotation),
		-- 	log_threshold	=> log_threshold + 1);

		-- Commit the new state of the design:
		commit (POST, verb, noun, log_threshold);

		status_clear;

		redraw_board;

		-- In case further netchangers are to be added,
		-- assign the prospective next name:
		netchanger_add.name_pre := next_netchanger_index (active_module);
		
		log_indentation_down;
	end add_netchanger;



	

	

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

				reset_status_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);

				-- Do the copy operation:
				copy_object (
					module_cursor	=> active_module, 
					object			=> object, 
					destination		=> type_position (to_position (point, netchanger_add.rotation)),
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

				-- If a netchanger has been copied, then the board
				-- must be redrawn:
				if object.cat = CAT_NETCHANGER then
					redraw_board;
				end if;
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			

			-- clear status bar
			status_clear;

			-- The preview-object is no longer required:
			reset_netchanger_add;
			
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
				when CAT_NETCHANGER =>
					-- Build the preview of the netchanger that is going to
					-- be added:
					netchanger_add.name := get_object_name (object.netchanger);

					-- CS
					-- unit_add.name := get_first_unit (unit_add.device);
					-- unit_add.value := get_value (object.unit.device_cursor);
					-- unit_add.total := get_unit_count (object.unit.device_cursor);
     -- 
					-- unit_add.device_pre := get_next_available_device_name (
					-- 	active_module, get_prefix (unit_add.device), log_threshold + 1);
     -- 
					-- unit_add.rotation := get_rotation (object.unit.unit_cursor);
					-- unit_add.valid := true;
					
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


	






	


	procedure show_object (
		position : in type_vector_model)
	is 

		-- Shows some information in the status bar:
		procedure finalize is
			use et_device_property_level;
			
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

				-- Highlight the netchanger in the board editor:
				redraw_board;


				-- Write some basic information in the status bar:
				case object.cat is
					when CAT_NETCHANGER =>

						-- CS
						null;
						-- set_status (get_properties (
						-- 	device_cursor	=> object.unit.device_cursor,
						-- 	level			=> DEVICE_PROPERTIES_LEVEL_1,						   
						-- 	all_units		=> false,
						-- 	unit_cursor		=> object.unit.unit_cursor));

					when others =>
						status_clear;
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
	

	
	
end et_canvas_schematic_netchangers;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
