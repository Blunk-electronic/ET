------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        CANVAS SCHEMATIC NETS                             --
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
-- <http://www.gnu.org/licenses/>.   
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
with ada.strings;					use ada.strings;
with ada.characters.handling;
with ada.exceptions;				use ada.exceptions;

with gtk.window;
with gtk.box;
with gtk.label;

with et_module_names;				use et_module_names;
with et_modes.schematic;			use et_modes.schematic;
with et_netlists;
with et_net_class;
with et_schematic_ops_groups;
with et_schematic_ops_nets;			use et_schematic_ops_nets;

with et_undo_redo;
with et_commit;
with et_object_status;				use et_object_status;
with et_canvas_schematic_preliminary_object;	use et_canvas_schematic_preliminary_object;


package body et_canvas_schematic_nets is

	use et_schematic_ops;
	use et_canvas_schematic.pac_canvas;
	use et_canvas_schematic.pac_net_ops;
	



	
	procedure make_path (
		tool	: in type_tool;
		point	: in type_vector_model)
	is begin
		-- Set the tool being used for this path so that procedure
		-- draw_path (in et_canvas_schematic-draw_nets)
		-- knows where to get the end point from.
		object_tool := tool;

		-- Initally the preliminary_segment is NOT ready. Nothing will be drawn.
		-- Upon the first calling of this procedure the start point of the
		-- path will be set.
		
		if not edit_process_running then
			-- Set start point:
			live_path.A := point;
					
			-- Allow drawing of the path:
			set_edit_process_running;
			
			set_status (status_A & to_string (live_path.A) & ". " &
				status_press_space & status_set_B & status_hint_for_abort);
			
		else -- preliminary_segment IS ready

			-- Start a new path only if the given point differs from 
			-- the start point of the current path:
			if point /= live_path.A then
				
				-- Complete the path by setting its end point.
				-- The the current bend point (if there is one) into account:

				if live_path.bended = NO then				
					live_path.B := point;

					-- Insert a single net segment:
					add_net_segment (
						module			=> active_module,
						sheet			=> active_sheet,
						net_name_given	=> object_net_name, -- RESET_N, or empty
						segment			=> to_net_segment (live_path.A, live_path.B),
						log_threshold	=> log_threshold + 1);

				else
					-- The path is bended. The bend point has been computed
					-- interactively while moving the mouse or the cursor.
					-- See procedure draw_path in et_canvas_schematic-draw_nets.
					

					-- Insert first segment of the path:
					add_net_segment (
						module			=> active_module,
						sheet			=> active_sheet,
						net_name_given	=> object_net_name, -- RESET_N, or empty
						segment			=> to_net_segment (live_path.A, live_path.bend_point),
						log_threshold	=> log_threshold + 1);

					
					-- END POINT:
					live_path.B := point;
					

					-- Insert second segment of the path:
					add_net_segment (
						module			=> active_module,
						sheet			=> active_sheet,
						net_name_given	=> object_net_name, -- RESET_N, or empty
						segment			=> to_net_segment (live_path.bend_point, live_path.B),
						log_threshold	=> log_threshold + 1);
						

				end if;

				-- Set start point of path so that a new
				-- path can be drawn:			
				live_path.A := point;

			else
				reset_preliminary_segment;
			end if;
		end if;
	end make_path;



	
	


	procedure add_net_segment (
		module			: in pac_generic_modules.cursor;
		net_name_given	: in pac_net_name.bounded_string; -- RESET_N
		sheet			: in type_sheet;
		segment			: in type_net_segment;
		log_threshold	: in type_log_level)
	is 
		A : constant type_object_position := to_position (get_A (segment), sheet);
		B : constant type_object_position := to_position (get_B (segment), sheet);

		-- When a net segment is added to the drawing, then
		-- it may become connected with already existing segments of other nets.
		-- These lists will contain the net names of affected nets:
		nets_at_A, nets_at_B : pac_net_names.list;

		-- If no net name was specified by the caller (net_name_given), then
		-- an anonymous net with an auto-generated name will be created:		
		net_name_auto_generated	: pac_net_name.bounded_string; -- like N$1

		
		-- This procedure extends a net that exists at the A or B end of
		-- the given segment by the given segment:
		procedure extend_net (net_name : in pac_net_name.bounded_string) is begin
			-- log (text => "attaching start point of new segment to net "
			-- 	& enclose_in_quotes (to_string (net_name)),
			-- 	level => log_threshold + 1);
			
			log_indentation_up;

			insert_net_segment (
				module_cursor	=> module,
				net_name		=> net_name, 
				A				=> to_position (get_A (segment), sheet),
				B				=> get_B (segment),
				log_threshold	=> log_threshold + 1);
			
			status_clear;
			
			log_indentation_down;

			-- CS
			-- Outputs a message if an explicit net_name_given was provided stating
			-- that this net_name_given will be ignored.
			
			-- If an explicit net name was given AND if it does
			-- NOT match the name of the net being extended,
			-- then output a message:

-- 			if not is_empty (net_name_given) then -- explicit name given
-- 				if net_name_given /= net_name then -- names do NOT match
-- 
-- 					set_status ("WARNING ! Given net name " 
-- 						& enclose_in_quotes (to_string (net_name_given))
-- 						& " ignored while extending net " 
-- 						& enclose_in_quotes (to_string (net_name)) & " !");
-- 					
-- 				end if;
-- 			end if;
		end extend_net;


		use et_undo_redo;
		use et_commit;

		
	begin
		log (text => "adding net segment on sheet " & to_string (sheet) 
			 & " " & to_string (segment), 
			 level => log_threshold);

		log_indentation_up;

		-- Commit the current state of the design:
		commit (PRE, verb, noun, log_threshold + 1);
		

		-- Look for already existing nets on the A and B end
		-- of the given segment:
		nets_at_A := get_nets_at_place (module, A, log_threshold + 2);
		nets_at_B := get_nets_at_place (module, B, log_threshold + 2);
		
		
		-- If no nets at BOTH the A AND the B end, then
		-- a new strand will be generated:
		if nets_at_A.is_empty and nets_at_B.is_empty then

			-- If no explict net name was specified by the caller,
			-- then a name will be auto-generated:
			if is_empty (net_name_given) then

				-- Create a new anonymous net with a name like N$234:
				net_name_auto_generated := get_lowest_available_anonymous_net (module); -- N$234
				
				log (text => "auto-generated net name is: " & to_string (net_name_auto_generated),
					 level => log_threshold + 1);
				
				log_indentation_up;

				insert_net_segment (
					module_cursor	=> module,
					net_name		=> net_name_auto_generated, 
					A				=> to_position (get_A (segment), sheet),
					B				=> get_B (segment),
					log_threshold	=> log_threshold + 2);
				
				status_clear;
				log_indentation_down;
				
				
			else -- explicit net name provided
				log (text => "explicitly given net name is: " & to_string (net_name_given),
					level => log_threshold + 1);
				
				log_indentation_up;

				insert_net_segment (
					module_cursor	=> module,
					net_name		=> net_name_given, 
					A				=> to_position (get_A (segment), sheet),
					B				=> get_B (segment),
					log_threshold	=> log_threshold + 2);
				
				status_clear;
				log_indentation_down;
			end if;
		end if;

		
		-- If nets exist at the A end AND if no nets exist at the B end,
		-- then the first net at the A end will be extended by the new segment:
		if not nets_at_A.is_empty and nets_at_B.is_empty then
			extend_net (nets_at_A.first_element);
		end if;

		-- If no nets exist at the A end AND if nets exist at the B end,
		-- then the first net at the B end will be extended by the new segment:
		if nets_at_A.is_empty and not nets_at_B.is_empty then
			extend_net (nets_at_B.first_element);
		end if;

-- CS		
		-- If net at start point AND at end point then extend the
		-- net at the start point by the segment. We could extend the net
		-- at the end point as well. It does not matter.
		-- This results in connecting two strands with each other. Their
		-- net names must be equal.
		-- The verification that the net names match is done by
		-- et_schematic_ops_nets.insert_segment.
		-- if not is_empty (net_name_end) and not is_empty (net_name_start) then
		-- 	extend_net (net_name_start);
		-- end if;

		-- Commit the new state of the design:
		commit (POST, verb, noun, log_threshold + 1);
		
		log_indentation_down;
	end add_net_segment;





	
	


	

	procedure reset_preliminary_segment is 
	begin
		reset_edit_process_running;
		object_tool := MOUSE;
		-- live_path := (bend_style => live_path.bend_style, -- no change
					-- others => <>);

		-- object_net_name := no_name;
		object_point_of_attack := origin;
		reset_finalizing_granted;
		
		-- clear_proposed_segments;
	end reset_preliminary_segment;


	

	


	
	
	procedure cb_rename_new_name_entered (
		self : access gtk.gentry.gtk_entry_record'class) 
	is 
		net_name_new : pac_net_name.bounded_string;

		
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
					new_name_net	=> net_name_new,
					log_threshold	=> log_threshold + 1);


				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

				redraw_board; -- board is not always affected
				-- CS redraw schematic ?
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			

			reset_editing_process; -- prepare for a new editing process
		end finalize;

		
		
	begin
		net_name_new := to_net_name (self.get_text); -- RST_N

		-- CS: Precheck net name ?
		-- put_line ("new name entered: " & to_string (net_name_new));
		
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

		object : constant type_object := get_first_object (
				active_module, SELECTED, log_threshold + 1);

		net_name : pac_net_name.bounded_string; -- RESET_N
		
	begin
		build_rename_window;

		-- Connect the "destroy" signal.
		rename_window.on_destroy (cb_rename_window_destroy'access);


		case object.cat is
			when CAT_NET =>
				net_name := get_net_name (object.net.net_cursor);

			when CAT_STRAND =>
				net_name := get_net_name (object.strand.net_cursor);

			when others =>
				raise constraint_error; -- CS
		end case;

		-- Set the text in the window:
		rename_old.set_text (to_string (net_name));

		-- Connect the "on_activate" signal (emitted when ENTER pressed)
		-- of the entry field for the new name:
		rename_new.on_activate (cb_rename_new_name_entered'access);
		-- gtk_entry (rename_window.box.get_child).on_activate (rename_new_name_entered'access);
		
		rename_new.grab_focus;
		
		rename_window.show_all;

		rename_window_open := true;
	end show_rename_window;

	




	procedure show_selected_object (
		object		: in type_object)
	is 
		praeamble : constant string := "selected object: ";
	begin
		case object.cat is
			when CAT_SEGMENT =>
				set_status (praeamble & to_string (object.segment)
					& ". " & status_next_object_clarification);

			when CAT_STRAND =>
				set_status (praeamble & to_string (object.strand)
					& ". " & status_next_object_clarification);
				
			when CAT_NET =>
				set_status (praeamble & to_string (object.net)
					& ". " & status_next_object_clarification);

			when CAT_LABEL =>
				set_status (praeamble & to_string (object.label)
					& ". " & status_next_object_clarification);

			when CAT_CONNECTOR =>
				set_status (praeamble & to_string (object.connector)
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

			drag_granted : boolean := false;
			
		begin
			-- Get a cursor to the candidate object
			-- among all selected objects:
			c := objects.find (selected_object);


			-- CS test noun ?
			
			case verb is

				-- If we are dragging a net segment, then we regard it as primary segment.
				-- Its start or/and ends (A/B) must be marked as "moving"
				-- The current point of attack determines whether start or
				-- end or both of the segment are affected.
				-- Start/end points (A/B) of secondary segments which are 
				-- connected with the primary segment must be marked as "moving" also.
				-- If dragging is not granted (due to connected ports) then nothing happens:
				when VERB_DRAG =>
					set_primary_segment_AB_moving (
						module_cursor	=> active_module, 
						object_cursor	=> c,
						point_of_attack	=> object_point_of_attack,
						movable_test	=> true, -- The segment must be testet whether it is movable.
						granted			=> drag_granted,
						log_threshold	=> log_threshold + 1);

					if drag_granted then
						-- Set the whole segment as moving:
						modify_status (active_module, c, to_operation (SET, MOVING), log_threshold + 1);

						-- Set attached secondary segments as moving:
						set_secondary_segments_AB_moving (
							active_module, c, log_threshold + 1);

					end if;

					
				when others =>
					modify_status (active_module, c, to_operation (SET, MOVING), log_threshold + 1);
			end case;
			
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
		log (text => "find_objects", level => log_threshold);
		log_indentation_up;

		-- Before locating any objects, previous
		-- proposed or selected objects should be reset:		
		et_schematic_ops_groups.reset_objects (active_module, log_threshold + 1);

		
		-- Propose objects according to current verb and noun:
		case verb is
			when VERB_DELETE | VERB_DRAG =>
				case noun is
					when NOUN_SEGMENT =>
				
						-- Propose net segments in the vicinity of the given point:
						propose_segments (
							module_cursor	=> active_module,
							catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
							count			=> count_total,
							log_threshold	=> log_threshold + 1);

						
					when NOUN_STRAND =>

						-- Propose strands in the vicinity of the given point:
						propose_strands (
							module_cursor	=> active_module,
							catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
							count			=> count_total,
							log_threshold	=> log_threshold + 1);


					when NOUN_NET =>

						-- Propose nets in the vicinity of the given point:
						propose_nets (
							module_cursor	=> active_module,
							catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
							count			=> count_total,
							log_threshold	=> log_threshold + 1);


					when NOUN_NET_LABEL =>

						-- Propose net labels in the vicinity of the given point:
						propose_labels (
							module_cursor	=> active_module,
							catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
							count			=> count_total,
							log_threshold	=> log_threshold + 1);

						
					when NOUN_NET_CONNECTOR =>
						
						-- Propose net connectors in the vicinity of the given point:
						propose_connectors (
							module_cursor	=> active_module,
							catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
							count			=> count_total,
							log_threshold	=> log_threshold + 1);
						
						
					when others => null; -- CS
				end case;

				
			when VERB_PLACE =>
				case noun is
					when NOUN_NET_CONNECTOR | NOUN_NET_LABEL =>

						-- Propose net segments in the vicinity of the given point:
						propose_segments (
							module_cursor	=> active_module,
							catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
							count			=> count_total,
							log_threshold	=> log_threshold + 1);

						
					when others => null;
				end case;
				

			when VERB_RENAME =>
				case noun is
					when NOUN_STRAND =>

						-- Propose strands in the vicinity of the given point:
						propose_strands (
							module_cursor	=> active_module,
							catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
							count			=> count_total,
							log_threshold	=> log_threshold + 1);


					when NOUN_NET =>

						-- Propose nets in the vicinity of the given point:
						propose_nets (
							module_cursor	=> active_module,
							catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
							count			=> count_total,
							log_threshold	=> log_threshold + 1);

						
					when others => null; -- CS
				end case;

				
			when VERB_SHOW =>
				case noun is
					when NOUN_NET =>
						-- Propose nets in the vicinity of the given point:
						propose_nets (
							module_cursor	=> active_module,
							catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
							count			=> count_total,
							log_threshold	=> log_threshold + 1);

						
					when NOUN_NET_LABEL =>
						-- Propose simple label in the vicinity of the given point:
						propose_labels (
							module_cursor	=> active_module,
							catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
							count			=> count_total,
							log_threshold	=> log_threshold + 1);

						
					when NOUN_NET_CONNECTOR =>
						-- Propose net connectors in the vicinity of the given point:
						propose_connectors (
							module_cursor	=> active_module,
							catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
							count			=> count_total,
							log_threshold	=> log_threshold + 1);

					when others => null;						
				end case;
				
				
			when VERB_MOVE =>
				case noun is
					when NOUN_NET_LABEL =>
						
						-- Propose net labels in the vicinity of the given point:
						propose_labels (
							module_cursor	=> active_module,
							catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
							count			=> count_total,
							log_threshold	=> log_threshold + 1);


					when others => null;
				end case;
			when others => null;
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

						
					when VERB_DRAG =>
						set_first_selected_object_moving;


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

				reset_status_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				drag_object (
					module_cursor	=> active_module, 
					object			=> object, 
					point_of_attack	=> object_point_of_attack,
					destination		=> point,
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

				redraw_board; -- board is not always affected
				-- CS Redraw schematic ?
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			
			set_status (status_drag);
			-- CS clear status bar ?

			reset_editing_process; -- prepare for a new editing process
		end finalize;

		
	begin
		-- Initially the editing process is not running:
		if not edit_process_running then
			
			-- Set the tool being used:
			object_tool := tool;

			object_point_of_attack := point;
			
			
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
			finalize;
		end if;
	end drag_object;

	

	


	procedure move_object (
		tool	: in type_tool;
		point	: in type_vector_model)
	is 

		-- Moves the selected object:
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
					point_of_attack	=> object_point_of_attack,
					destination		=> point,
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

				redraw_board; -- board is not always affected
				-- CS redraw schematic ?
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			
			set_status (status_move);
			-- CS clear status bar

			reset_editing_process; -- prepare for a new editing process
		end finalize;

		
	begin
		-- Initially the editing process is not running:
		if not edit_process_running then
			
			-- Set the tool being used:
			object_tool := tool;

			object_point_of_attack := point;
			
			
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
			finalize;
		end if;
	end move_object;






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

				reset_status_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				delete_object (
					module_cursor	=> active_module, 
					object			=> object, 
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

				redraw_board; -- board is not always affected
				-- CS redraw schematic ?
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			
			set_status (status_delete);
			-- CS clear status bar

			reset_editing_process; -- prepare for a new editing process
		end finalize;

		
	begin
		if not clarification_pending then
			-- Locate all objects in the vicinity of the given point:
			find_objects (point);
			
			-- NOTE: If many objects have been found, then
			-- clarification is now pending.

			-- If find_objects has found only one object
			-- then the flag edit_process_running is set true.

			if edit_process_running then
				finalize;
			end if;
		else
			-- Here the clarification procedure ends.
			-- An object has been selected
			-- via procedure clarify_object.

			finalize;
		end if;
	end delete_object;







	procedure rename_object (
		point	: in type_vector_model)
	is begin
		if not rename_window_open then
			
			if not clarification_pending then
				-- Locate all objects in the vicinity of the given point:
				find_objects (point);
				
				-- NOTE: If many objects have been found, then
				-- clarification is now pending.

				-- If find_objects has found only one object
				-- then the flag edit_process_running is set true.

				if edit_process_running then
					show_rename_window;
				end if;
			else
				-- Here the clarification procedure ends.
				-- An object has been selected
				-- via procedure clarify_object.

				show_rename_window;
			end if;
		end if;
	end rename_object;

	


	
	


	procedure show_object (
		point	: in type_vector_model)
	is 

		-- Shows the selected object:
		procedure finalize is
			use et_modes.schematic;
			use et_undo_redo;
			use et_commit;

			object : constant type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize show", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then
				
				show_object (
					module_cursor	=> active_module, 
					object			=> object, 
					log_threshold	=> log_threshold + 1);

				if object.cat = CAT_NET then
					redraw_board;
				end if;

				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			
			reset_editing_process; -- prepare for a new editing process
		end finalize;

		
	begin
		if not clarification_pending then

			-- Locate all objects in the vicinity of the given point:
			find_objects (point);
			
			-- NOTE: If many objects have been found, then
			-- clarification is now pending.

			-- If find_objects has found only one object
			-- then the flag edit_process_running is set true.

			if edit_process_running then
				finalize;
			end if;
		else
			-- Here the clarification procedure ends.
			-- An object has been selected
			-- via procedure clarify_object.

			finalize;
		end if;
	end show_object;




	
	
	
-- NET LABLES


	

	
	
	function to_string (cat : in type_label_category) return string is 
		use ada.characters.handling;
	begin
		return to_lower (type_label_category'image (cat));
	end to_string;







	
	procedure place_net_label (
		tool	: in type_tool;
		point	: in type_vector_model)
	is 


		procedure finalize is
			use et_modes.schematic;
			use et_undo_redo;
			use et_commit;

			object : constant type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize place net label", level => log_threshold);
			log_indentation_up;

			
			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then
				
				reset_status_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				place_net_label (
					module_cursor	=> active_module, 
					segment			=> object.segment,
					position		=> point,
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			
			set_status (status_delete_label);
			-- CS clear status bar

			reset_editing_process; -- prepare for a new editing process
		end finalize;
		

	begin
		-- Initially the editing process is not running:
		if not edit_process_running then

			-- Set the tool being used:
			object_tool := tool;
			
			
			if not clarification_pending then
				-- Locate all net segments in the vicinity of the given point:
				find_objects (point);

				-- NOTE: If many segments have been found, then
				-- clarification is now pending.

				-- If find_objects has found only one segment,				
				-- then the flag edit_process_running is set true.

				if edit_process_running then
					finalize;
				end if;
				
			else
				-- Here the clarification procedure ends.
				-- A segment has been selected via procedure clarify_object.
				
				finalize;
			end if;

		else
			finalize;
		end if;
	end place_net_label;





	

	procedure place_net_connector (
		tool	: in type_tool;
		point	: in type_vector_model)
	is 


		procedure finalize is
			use et_modes.schematic;
			use et_undo_redo;
			use et_commit;

			object : constant type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalize place net connector", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then

				reset_status_objects (active_module, log_threshold + 1);
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);

				place_net_connector (
					module_cursor	=> active_module, 
					segment			=> object.segment,
					position		=> point,
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			
			set_status (status_delete_label);
			-- CS clear status bar

			reset_editing_process; -- prepare for a new editing process
		end finalize;
		

	begin
		-- Initially the editing process is not running:
		if not edit_process_running then

			-- Set the tool being used:
			object_tool := tool;
			
			
			if not clarification_pending then
				-- Locate all net segments in the vicinity of the given point:
				find_objects (point);

				-- NOTE: If many segments have been found, then
				-- clarification is now pending.

				-- If find_objects has found only one segment,				
				-- then the flag edit_process_running is set true.

				if edit_process_running then
					finalize;
				end if;
				
			else
				-- Here the clarification procedure ends.
				-- A segment has been selected via procedure clarify_object.
				
				finalize;
			end if;

		else
			finalize;
		end if;
	end place_net_connector;

	
	
	
-- 	procedure show_properties_of_selected_net is
-- 		ss	: constant type_selected_segment := element (selected_segment);
-- 		use et_net_class;
-- 		use et_netlists;
-- 	begin
-- 		reset_request_clarification;
-- 		
-- 		set_status ("Properties:"
-- 			& " name " & to_string (key (ss.net))
-- 			& ", class " & to_string (element (ss.net).class)
-- 			& ", scope " & to_string (element (ss.net).scope)
-- 			);
-- 		
-- 	end show_properties_of_selected_net;


	
end et_canvas_schematic_nets;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
