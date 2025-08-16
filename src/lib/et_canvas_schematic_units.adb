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

with gdk.types;						use gdk.types;
with gdk.event;						use gdk.event;
with gdk.types.keysyms;				use gdk.types.keysyms;

with gtkada.file_selection;
with gtk.file_chooser_dialog;
with gtk.file_chooser;
with gtk.file_chooser_button;
with gtk.file_filter;

with gtk.main;
with gtk.widget;					use gtk.widget;
with gtk.window;
with gtk.box;
with gtk.label;
with gtk.gentry;
with gtk.menu;
with gtk.menu_item;
with gtk.menu_shell;

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
		log (text => "set_first_selected_object_moving ...", level => log_threshold);
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
		log (text => "locating objects ...", level => log_threshold);
		log_indentation_up;

		-- CS propose objects according to
		-- current verb.
		
		-- Propose units in the vicinity of the given point:
		propose_units (
			module_cursor	=> active_module,
			catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
			count			=> count_total,
			log_threshold	=> log_threshold + 1);


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

				-- If a unit has been deleted, then the board
				-- must be redrawn:
				if object.cat = CAT_UNIT then
					redraw_board;
				end if;
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			
			set_status (status_move);
			
			reset_proposed_objects (active_module, log_threshold + 1);

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
			log (text => "finalizing rotate ...", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then
				
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
			
			set_status (status_rotate);
			
			reset_proposed_objects (active_module, log_threshold + 1);

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

			object : constant type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalizing delete ...", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
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
			
			set_status (status_delete);
			
			reset_proposed_objects (active_module, log_threshold + 1);

			reset_editing_process; -- prepare for a new editing process
		end finalize;

		
	begin
		if not clarification_pending then
			-- Locate all objects in the vicinity of the given point:
			find_objects (point);
			-- NOTE: If many objects have been found, then
			-- clarification is now pending.

			-- If find_objects has found only one object,				
			-- then delete the object immediatley.
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




	
	
	procedure reset_segments_being_dragged is begin
		segments_being_dragged.clear;
	end reset_segments_being_dragged;

-- 	
-- 	
-- 	procedure finalize_drag (
-- 		destination		: in type_vector_model;
-- 		log_threshold	: in type_log_level)
-- 	is
-- 		su : type_selected_unit;
-- 
-- 		use pac_devices_sch;
-- 		use pac_units;
-- 
-- 		use et_commit;
-- 		use et_undo_redo;
-- 	begin
-- 		log (text => "finalizing drag ...", level => log_threshold);
-- 		log_indentation_up;
-- 
-- 		if selected_unit /= pac_proposed_units.no_element then
-- 
-- 			su := element (selected_unit);
-- 
-- 			-- Commit the current state of the design:
-- 			commit (PRE, verb, noun, log_threshold);
-- 			
-- 			drag_unit (
-- 				module_cursor	=> active_module,
-- 				device_name		=> key (su.device),
-- 				unit_name		=> key (su.unit),
-- 				coordinates		=> ABSOLUTE,
-- 				destination		=> destination,
-- 				log_threshold	=> log_threshold);
-- 
-- 			-- Commit the new state of the design:
-- 			commit (POST, verb, noun, log_threshold);			
-- 		else
-- 			log (text => "nothing to do", level => log_threshold);
-- 		end if;
-- 			
-- 		log_indentation_down;
-- 
-- 		set_status (status_drag);
-- 
-- 		reset_segments_being_dragged;
-- 		
-- 		-- CS reset_unit_move; -- reset_proposed_objects ?
-- 	end finalize_drag;




	

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
			log (text => "finalizing drag ...", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then
				
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
			
			set_status (status_delete); -- CS correct ?
			
			reset_proposed_objects (active_module, log_threshold + 1);

			et_schematic_ops.nets.reset_segments (active_module, log_threshold + 1);

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
				-- connected with the selected unit
				-- as "moving":
				set_segments_moving (active_module, log_threshold + 1);

				-- Furtheron, on the next call of this procedure
				-- the selected object will be assigned its final position.

				set_edit_process_running;
				reset_request_clarification;
			end if;

		else
			finalize;
			
			-- -- Finally assign the pointer position to the
			-- -- currently selected unit:
			-- finalize_drag (
			-- 	destination		=> position,
			-- 	log_threshold	=> log_threshold + 1);

		end if;
	end drag_object;

	


	
-- 	procedure find_units_for_move (point : in type_vector_model) is 
-- 		use et_modes.schematic;
-- 	begin
-- 		log (text => "locating units for move/drag ...", level => log_threshold);
-- 		log_indentation_up;
-- 		
-- 		-- Collect all units in the vicinity of the given point:
-- 		proposed_units := collect_units (
-- 			module			=> active_module,
-- 			place			=> to_position (point, active_sheet),
-- 			zone			=> get_catch_zone (catch_zone_radius_default),
-- 			log_threshold	=> log_threshold + 1);
-- 
-- 		
-- 		-- evaluate the number of units found here:
-- 		case length (proposed_units) is
-- 			when 0 =>
-- 				reset_request_clarification;
-- 				--reset_unit_move;
-- 				-- CS reset_proposed_objects ?
-- 				
-- 			when 1 =>
-- 				set_edit_process_running;
-- 				selected_unit := proposed_units.first;
-- 
-- 				case verb is
-- 					when VERB_DRAG => 
-- 						find_attached_segments;
-- 						set_status (status_drag);
-- 						
-- 					when VERB_MOVE => 
-- 						set_status (status_move);
-- 						
-- 					when others => null;
-- 				end case;
-- 
-- 				reset_request_clarification;
-- 				
-- 			when others =>
-- 				--log (text => "many objects", level => log_threshold + 2);
-- 				set_request_clarification;
-- 
-- 				-- preselect the first unit
-- 				selected_unit := proposed_units.first;
-- 		end case;
-- 		
-- 		log_indentation_down;
-- 	end find_units_for_move;




	
	procedure find_attached_segments is
		-- Device and unit name of the selected unit:
		use pac_devices_sch;
		use pac_units;
		use et_symbol_ports;
		use et_net_strands;
		
		su : type_selected_unit := element (selected_unit);
		device_name : constant type_device_name := key (su.device);
		unit_name : constant pac_unit_name.bounded_string := key (su.unit);

		-- The ports with their positions of the selected unit:
		ports : pac_ports.map;

		-- The initial position of the selected unit before 
		-- the drag:
		unit_position : type_object_position;
		
		procedure get_ports (su : in type_selected_unit) is 
			use pac_units;
		begin
			-- Get the unit position before the drag:
			unit_position := element (su.unit).position;

			-- Get the default port positions as defined in the library:
			ports := get_ports_of_unit (su.device, key (su.unit));

			-- Calculate the port positions in the schematic before the drag:
			rotate_ports (ports, get_rotation (unit_position));
			move_ports (ports, unit_position);
		end get_ports;

		
		procedure query_port (p : in pac_ports.cursor) is
			use pac_nets;
			use pac_ports;

			procedure query_net (n : in pac_nets.cursor) is
				use pac_strands;

				procedure query_strand (s : in pac_strands.cursor) is
					use pac_net_segments;

					procedure query_segment (g : in pac_net_segments.cursor) is begin
						log (text => to_string (element (g)));
						
						if get_A (g) = element (p).position then
							segments_being_dragged.append ((
-- 								net		=> n,
-- 								strand	=> s,
								segment	=> element (g),
								zone	=> START_POINT));

							--log (text => "dg S" & to_string (element (g)), console => true);
						end if;

						if get_B (g) = element (p).position then
							segments_being_dragged.append ((
-- 								net		=> n,
-- 								strand	=> s,
								segment	=> element (g),
								zone	=> END_POINT));

							--log (text => "dg1 E" & to_string (element (g)), console => true);
						end if;
					end query_segment;
					
				begin -- query_strand
					if get_sheet (element (s).position) = active_sheet then
						iterate (element (s).segments, query_segment'access);
					end if;
				end query_strand;
				
			begin
				--log (text => "iterate strands", console => true);
				iterate (element (n).strands, query_strand'access);
			end query_net;
			
		begin
			--log (text => "iterate nets", console => true);
			iterate (element (active_module).nets, query_net'access);
		end query_port;
		
	begin -- find_attached_segments
		--log (text => "find attached segments", console => true);
		
		query_element (selected_unit, get_ports'access);
		-- now the ports of the selected unit are in "ports"

		--log (text => count_type'image (ports.length), console => true);
		
		pac_ports.iterate (ports, query_port'access);
		
	end find_attached_segments;



	


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

	
	function device_selection_is_open return boolean is begin
		return device_selection.open;
	end device_selection_is_open;

	
	procedure close_device_selection is begin
		put_line ("close_device_selection");
		device_selection.window.destroy;
		device_selection.open := false;
	end close_device_selection;	

	
	function extract_variant_name (menu_item : in string) 
		return pac_package_variant_name.bounded_string 
	is
		-- Extract the variant name from field 3 of the menu item.
		-- Field separator is space:
		var_name : constant string := get_field_from_line (
			text_in		=> menu_item,
			position	=> 3);

	begin
		return to_variant_name (var_name);
	end extract_variant_name;

	
	
	-- In order to place a package variant and the associated model
	-- on a menu, use this function:
	function to_package_variant_item (variant : in pac_variants.cursor) 
		return string 
	is
		use pac_variants;
		use pac_package_variant_name;
		variant_name : constant string := to_string (key (variant));

		use et_package_names;
		package_model : constant string := to_string (element (variant).package_model);
	begin
		-- Build the menu item. NOTE: The actual variant name must be
		-- the 3rd string of the entry. Field separator is space.
		-- Procedures that evaluate the item expect it at this place:
		return "package variant: " & variant_name & " model: " & package_model;
	end to_package_variant_item;

	
	procedure reset_unit_add is begin
		unit_add := (others => <>);
	end reset_unit_add;


	
	procedure variant_selected (self : access gtk.menu_item.gtk_menu_item_record'class) is
		use pac_package_variant_name;
	begin
		unit_add.variant := extract_variant_name (self.get_label);
		
		set_status ("Variant " 
			& enclose_in_quotes (to_string (unit_add.variant))
			& " selected. "
			& status_add);

		-- The device selection window is no longer needed:
		close_device_selection;
	end variant_selected;


	
	--procedure device_directory_selected (self : access gtk.file_chooser_button.gtk_file_chooser_button_record'class) is
	--begin
		--put_line (self.get_current_folder);
	--end device_directory_selected;

	
	
	procedure device_model_selected (self : access gtk.file_chooser_button.gtk_file_chooser_button_record'class) is

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


		
		procedure show_variants_menu is
			m : gtk_menu;
			i : gtk_menu_item;

			
			procedure query_variant (c : in pac_variants.cursor) is begin
				-- Build the menu item:
				i := gtk_menu_item_new_with_label (to_package_variant_item (c));
				i.on_activate (variant_selected'access);
				m.append (i);
				i.show;
			end query_variant;
			
		begin
			m := gtk_menu_new;
			variants.iterate (query_variant'access);

			m.show;
			m.popup;

		end show_variants_menu;
		

	begin -- device_model_selected
		put_line (self.get_filename);
		
		device_model := to_file_name (self.get_filename);

		--set_status ("selected device model: " & to_string (device_model));

		-- Read the device file and store it in the rig wide device 
		-- library et_devices.devices.
		-- If the device is already in the library, nothing happpens.
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
		
		-- assign the prospective device name:
		unit_add.device_pre := get_next_device_name (active_module, element (device_cursor_lib).prefix);
		
		-- get the available package variants:
		variants := get_available_variants (device_cursor_lib);

		case element (device_cursor_lib).appearance is
			when APPEARANCE_PCB =>
				if length (variants) > 1 then
					show_variants_menu;
				else
					unit_add.variant := key (variants.first);

					-- The device selection window is no longer needed:
					close_device_selection;
					
					set_status (status_add);
				end if;
				
			when APPEARANCE_VIRTUAL => null;
		end case;
			
		-- CS exception handler in case read_device fails ?
	end device_model_selected;


	
	
	function device_model_selection_key_event (
		self	: access gtk_widget_record'class;
		event	: in gdk_event_key) 
		return boolean 
	is
		key : gdk_key_type := event.keyval;

		-- This is required in order to propagate the key-pressed event further.
		result : boolean; -- to be returned. Indicates that the event has been handled.
	begin
		case key is
			when GDK_ESCAPE =>
				--put_line ("key A");

				-- Close the device selection if operator hits ESC:
				close_device_selection;
				result := true;

			when others =>
				--put_line ("key B");
				result := false;
		end case;
		
		return result;
	end device_model_selection_key_event;


	
	
	procedure device_model_selection_close (
		self	: access gtk_widget_record'class) 
	is begin
		device_selection.open := false;
	end device_model_selection_close;


	
	
	procedure show_model_selection is
		use gtk.window;
		use gtk.box;
		use gtk.label;
		use gtk.file_chooser_dialog;
		use gtk.file_chooser;
		use gtk.file_chooser_button;
		use gtk.file_filter;

		box_main : gtk_hbox;
		box_directory, box_model : gtk_vbox;
		label_directory, label_model : gtk_label;
		
		use glib;
		spacing : constant natural := 10;
		
		button_directory, button_model : gtk_file_chooser_button;
		filter : gtk_file_filter;

		use et_directory_and_file_ops;
	begin
		-- BUILD THE DEVICE SELECTION WINDOW:

		-- CS: Add the variant menu to this window.
		
		-- If it is already up, move it to the foreground.
		-- Otherwise build it:
		if device_selection_is_open then
			device_selection.window.present;
		else
			-- Mark the window as "open" so that it can not be opened anew:
			device_selection.open := true;
			
			gtk_new (device_selection.window);
			device_selection.window.set_title ("Select a device model");
			device_selection.window.on_destroy (device_model_selection_close'access);
			device_selection.window.on_key_press_event (device_model_selection_key_event'access);

			
			gtk_new_hbox (box_main, homogeneous => false, spacing => gint (spacing));
			add (device_selection.window, box_main);

			-- directory selection on the LEFT:
			gtk_new_vbox (box_directory, homogeneous => false);
			pack_start (box_main, box_directory, padding => guint (spacing));

			gtk_new (label_directory, "directory");
			
			gtk_new (
				button		=> button_directory,
				title		=> "Select a device model",
				action		=> ACTION_SELECT_FOLDER);

			-- CS: Currently the button_directory shows only the most important
			-- library path. It would be convenient if the operator would be shown
			-- all preferred library paths sorted by their rank.
			if button_directory.set_current_folder_uri (get_top_most_important_library) then
				null; -- Testing the existence of the folder is not required.
			end if;
			
			--button_directory.on_file_set (device_directory_selected'access);
			pack_start (box_directory, label_directory, padding => guint (spacing));
			pack_start (box_directory, button_directory, padding => guint (spacing));

			
			-- device model selection on the RIGHT:		
			gtk_new_vbox (box_model, homogeneous => false);
			pack_start (box_main, box_model, padding => guint (spacing));

			gtk_new (label_model, "model");
			
			gtk_new (filter);
			add_pattern (filter, make_filter_pattern (device_model_file_extension));
			set_name (filter, "Device Models");
			
			gtk_new (
				button		=> button_model,
				title		=> "Select a device model",
				action		=> ACTION_OPEN);

			button_model.add_filter (filter);

			if button_model.set_current_folder (button_directory.get_current_folder_uri) then
				null; -- Testing the existence of the device model is not required.
			end if;
			
			button_model.on_file_set (device_model_selected'access);
			pack_start (box_model, label_model, padding => guint (spacing));
			pack_start (box_model, button_model, padding => guint (spacing));
			
			device_selection.window.show_all;
		end if;
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
			module_name		=> key (active_module),
			device_model	=> pac_devices_lib.key (unit_add.device),
			variant			=> unit_add.variant,
			destination		=> to_position (position, active_sheet),
			log_threshold	=> log_threshold + 1);

		-- Commit the new state of the design:
		commit (POST, verb, noun, log_threshold);
		
		reset_unit_add;

		set_status (status_add);
		--status_enter_verb;
	end drop_unit;

	
	procedure finalize_fetch (
		position		: in type_vector_model;
		log_threshold	: in type_log_level)
	is 
		use pac_unit_name;
		use et_commit;
		use et_undo_redo;
	begin
		log (text => "finalizing fetch ...", level => log_threshold);
		log_indentation_up;
		
		if length (unit_add.name) > 0 then

			-- Commit the current state of the design:
			commit (PRE, verb, noun, log_threshold);
			
			fetch_unit (
				module_cursor	=> active_module,
				device_name		=> unit_add.device_pre,
				unit_name		=> unit_add.name,
				destination		=> to_position (position, active_sheet),
				log_threshold	=> log_threshold + 1);


			-- Commit the current state of the design:
			commit (POST, verb, noun, log_threshold);

		else
			log (text => "nothing to do", level => log_threshold + 1);
		end if;

		reset_unit_add;
		reset_request_clarification;
		set_status (status_fetch);

		log_indentation_down;		
	end finalize_fetch;

	
	

	-- Extracts from the selected menu item the unit name.
	procedure unit_selected (self : access gtk.menu_item.gtk_menu_item_record'class) is
		
		-- Extract the unit name from field 2 of the menu item:
		unit_name : constant string := get_field_from_line (
			text_in		=> self.get_label,
			position	=> 2);
	begin
		--put_line ("selected");
		
		-- assign the unit to be drawn:
		unit_add.name := to_unit_name (unit_name);

		-- Signal procedure draw_units to draw this unit as a preview:
		unit_add.via_fetch := true;

		-- The list of proposed units and the cursor "selected_unit" are
		-- no longer required. This operation also prevents the formerly
		-- selected unit to be drawn highlighted:
		clear_proposed_units;
		
		set_status ("Device " & to_string (unit_add.device_pre) 
			& " unit " & unit_name & " selected.");

	end unit_selected;
	

	
	procedure unit_selection_cancelled (self : access gtk.menu_shell.gtk_menu_shell_record'class) is
	begin
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


	
	procedure show_units is
		use pac_devices_sch;
		
		--su : constant type_selected_unit := element (selected_unit);
		su : type_selected_unit;
		
		device_model : pac_device_model_file.bounded_string;
		device_cursor_lib : pac_devices_lib.cursor;
		
		unit_names : pac_unit_names.list;

		use pac_unit_name;

		
		procedure show_menu is
			--use glib;
			use gtk.menu;
			use gtk.menu_item;

			m : gtk_menu; -- the menu
			i : gtk_menu_item; -- an item on the menu
									   
			-- If no units are available, then no menu is to be shown.
			-- So we must count units with this stuff:
			subtype type_units_available is natural range 0 .. type_unit_count'last;
			units_available : type_units_available := 0;
			
			use pac_unit_names;

			
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
					position	=> su.device,
					process		=> query_in_use'access);

				-- If the unit is available then put its name on the menu:
				if not in_use then -- unit is available
					-- put_line ("unit " & to_string (element (c)) & " available");
					
					units_available := units_available + 1;
					
					-- Build the menu item. NOTE: The actual unit name must be
					-- the 2nd string of the entry. Procedure unit_selected expects
					-- it at this place:
					i := gtk_menu_item_new_with_label (
						"unit " & to_string (element (c)));

					-- Connect the item with the "activate" signal which
					-- in turn calls procedure unit_selected:
					i.on_activate (unit_selected'access);

					m.append (i);
					i.show;
				end if;

			end query_name;

			
		begin -- show_menu

			-- create a menu
			m := gtk_menu_new;

			-- In case the operator closes the menu (via ESC for example)
			-- then reset unit_add.
			m.on_cancel (unit_selection_cancelled'access);

			-- Query the available units and add them
			-- to the menu:
			unit_names.iterate (query_name'access);

			-- If no units are available (because all are in use)
			-- then we do not show a menu.
			if units_available > 0 then
				-- put_line ("show units menu");

				-- Show the menu:
				m.show;

				-- CS place the menu either a cursor or pointer
				-- position.

				-- Open the menu:
				m.popup (
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
					& to_string (unit_add.device_pre)
					& " available !");

				reset_unit_add;
				--set_status (status_fetch);
			end if;
		end show_menu;

		
	begin -- show_units
		-- put_line ("show_units");
		su := element (selected_unit);		
		-- put_line ("selected " & to_string (key (su.device)));

		device_model := element (su.device).model;
		-- put_line ("model " & to_string (device_model));
		
		device_cursor_lib := get_device_model_cursor (device_model);

		-- assign the cursor to the device model:
		-- put_line ("assign model");
		unit_add.device := device_cursor_lib;

		-- For a nice preview we also need the total of units provided
		-- the the device:
		-- put_line ("assign total");
		unit_add.total := get_unit_count (unit_add.device);
			
		-- assign the prospective device name:
		-- put_line ("assign prospective device");
		unit_add.device_pre := key (su.device);

		-- collect the names of all units of the selected device:
		-- put_line ("get all units");
		unit_names := get_all_units (device_cursor_lib);

		-- Show the units of the device in a menu. After the operator
		-- has selected a unit, procedure unit_selected finally
		-- assigns the unit name to unit_add.
		-- put_line ("show units");
		show_menu;
		
	end show_units;



	
	
	procedure fetch_unit (point : in type_vector_model) is 
		use et_schematic_ops.units;
	begin
		log (text => "fetching unit ...", level => log_threshold);
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
				show_units;

			when others =>
				set_request_clarification;

				-- preselect the first unit
				selected_unit := proposed_units.first;
		end case;
		
		log_indentation_down;
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
								placeholder_position := element (unit_cursor).name.position;
								test_placeholder_position;

							when VALUE =>
								-- Get the position of the value placeholder relative to the unit origin:
								placeholder_position := element (unit_cursor).value.position;
								test_placeholder_position;

							when PURPOSE =>
								-- Get the position of the purpose placeholder relative to the unit origin:
								placeholder_position := element (unit_cursor).purpose.position;
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
							r := unit.name.rotation + rotation;
							unit.name.rotation := snap (r);
							
						when VALUE =>
							r := unit.value.rotation + rotation;
							unit.value.rotation := snap (r);
							
						when PURPOSE =>
							r := unit.purpose.rotation + rotation;
							unit.purpose.rotation := snap (r);

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
					module_name 		=> key (active_module),
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
