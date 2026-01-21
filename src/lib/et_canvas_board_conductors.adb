------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      CANVAS BOARD / CONDUCTORS                           --
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
-- DESCRIPTION:
-- 

with et_generic_modules;					use et_generic_modules;
with et_canvas_board;
with et_conductor_text.boards;
with et_conductor_segment.boards;			use et_conductor_segment.boards;
with et_board_ops;
with et_board_ops.conductors;				use et_board_ops.conductors;
with et_board_ops_signal_layers;			use et_board_ops_signal_layers;
with et_logging;							use et_logging;
with et_modes.board;
with et_display.board;
with et_undo_redo;
with et_commit;
with et_keywords;							use et_keywords;
with et_object_status;						use et_object_status;
with et_nets;
with et_canvas_board_preliminary_object;	use et_canvas_board_preliminary_object;
with et_pcb_placeholders.conductor;			use et_pcb_placeholders.conductor;


package body et_canvas_board_conductors is

	use et_canvas_board.pac_canvas;
	
	use pac_conductor_lines;
	use pac_conductor_arcs;
	use pac_conductor_circles;


	
	
	-- Outputs the selected line in the status bar:
	procedure show_selected_line_net (
		selected : in type_object_line_net)
	is 
		use et_nets;
		praeamble : constant string := "selected: ";
	begin
		set_status (praeamble
			& to_string (selected.line_cursor, true) -- start/end point/width/layer
			& " net " & get_net_name (selected.net_cursor)
			& ". " & status_next_object_clarification);
	end show_selected_line_net;


	

	-- Outputs the selected arc in the status bar:
	procedure show_selected_arc_net (
		selected : in type_object_arc_net)
	is 
		use et_nets;
		praeamble : constant string := "selected: ";
	begin
		set_status (praeamble
			& to_string (selected.arc_cursor, true) -- start/end point/width/layer
			& " net " & get_net_name (selected.net_cursor)
			& ". " & status_next_object_clarification);
	end show_selected_arc_net;

	
	

	-- Outputs the selected line in the status bar:
	procedure show_selected_line_floating (
		selected : in type_object_line_floating)
	is 
		praeamble : constant string := "selected: ";
	begin
		set_status (praeamble
			& to_string (selected.line_cursor, true) -- start/end point/width/layer
			& " floating. " & status_next_object_clarification);
	end show_selected_line_floating;


	

	-- Outputs the selected arc in the status bar:
	procedure show_selected_arc_floating (
		selected : in type_object_arc_floating)
	is 
		praeamble : constant string := "selected: ";
	begin
		set_status (praeamble
			& to_string (selected.arc_cursor, true) -- start/end point/width/layer
			& " floating. " & status_next_object_clarification);
	end show_selected_arc_floating;


	

	-- Outputs the selected segment in the status bar:
	procedure show_selected_segment_net (
		selected : in type_object_segment_net)
	is 
		praeamble : constant string := "selected: ";

		use et_nets;
		use et_board_geometry.pac_contours;
	begin
		set_status (praeamble & to_string (selected.segment)
			& " net " & get_net_name (selected.net)
			-- & " layer" & to_string (selected.laface) & ". " 
			-- CS read properties of zone to get the layer number
			& status_next_object_clarification);
	end show_selected_segment_net;

	


	-- Outputs the selected segment in the status bar:
	procedure show_selected_segment_floating (
		selected : in type_object_segment_floating)
	is 
		praeamble : constant string := "selected: ";

		use et_nets;
		use et_board_geometry.pac_contours;
	begin
		set_status (praeamble & to_string (selected.segment)
			-- & " layer" & to_string (selected.laface) & ". " 
			-- CS read properties of zone to get the layer number
			& status_next_object_clarification);
	end show_selected_segment_floating;

	

	
	-- Outputs the selected text in the status bar:
	procedure show_selected_text (
		selected : in type_object_text)
	is 
		praeamble : constant string := "selected: ";

		use et_conductor_text.boards;
	begin
		set_status (praeamble & to_string (selected.cursor)
			& status_next_object_clarification);
	end show_selected_text;




	-- Outputs the selected text placeholder in the status bar:
	procedure show_selected_placeholder (
		selected : in type_object_placeholder)
	is 
		praeamble : constant string := "selected: ";
	begin
		set_status (praeamble & to_string (selected.cursor)
			& status_next_object_clarification);
	end show_selected_placeholder;

	

	

	procedure show_selected_object (
		selected : in type_object)
	is begin
		case selected.cat is
			when CAT_LINE_NET =>
				show_selected_line_net (selected.line_net);

			when CAT_ARC_NET =>
				show_selected_arc_net (selected.arc_net);

			when CAT_LINE_FLOATING =>
				show_selected_line_floating (selected.line_floating);

			when CAT_ARC_FLOATING =>
				show_selected_arc_floating (selected.arc_floating);
				
			when CAT_ZONE_SEGMENT_NET =>
				show_selected_segment_net (selected.segment_net);

			when CAT_ZONE_SEGMENT_FLOATING =>
				show_selected_segment_floating (selected.segment_floating);
				
			when CAT_TEXT =>
				show_selected_text (selected.text);

			when CAT_PLACEHOLDER =>
				show_selected_placeholder (selected.placeholder);
				
			when CAT_VOID =>
				null; -- CS
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
		use et_board_ops;
		use et_modes.board;
		use et_display.board;

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

		
		-- This procedure proposes all objects in the vicinity of the given point.
		-- Only objects in enabled signal layers are adressed:
		procedure propose_objects is 
			catch_zone : type_catch_zone;
		begin
			for layer in 1 .. get_deepest_conductor_layer (active_module) loop
				if conductor_enabled (layer) then

					catch_zone := set_catch_zone (point, get_catch_zone (catch_zone_radius_default));

					-- Lines of nets:
					propose_lines (
						module_cursor	=> active_module, 
						layer			=> layer,
						catch_zone		=> catch_zone, 
						count			=> count_total, 
						freetracks		=> false,
						log_threshold	=> log_threshold + 2);

					-- Arcs of nets:
					propose_arcs (
						module_cursor	=> active_module, 
						layer			=> layer,
						catch_zone		=> catch_zone, 
						count			=> count_total, 
						freetracks		=> false,
						log_threshold	=> log_threshold + 2);
					
					-- Lines of freetracks (floating):
					propose_lines (
						module_cursor	=> active_module, 
						layer			=> layer,
						catch_zone		=> catch_zone, 
						count			=> count_total, 
						freetracks		=> true,
						log_threshold	=> log_threshold + 2);

					-- Arcs of freetracks (floating):
					propose_arcs (
						module_cursor	=> active_module, 
						layer			=> layer,
						catch_zone		=> catch_zone, 
						count			=> count_total, 
						freetracks		=> true,
						log_threshold	=> log_threshold + 2);
				
					-- CS circles

					-- Connected zones:
					propose_segments_net (
						module_cursor	=> active_module, 
						catch_zone		=> catch_zone, 
						layer			=> layer,
						count			=> count_total,
						log_threshold	=> log_threshold + 2);


					-- floating zones:
					propose_segments_floating (
						module_cursor	=> active_module, 
						catch_zone		=> catch_zone,
						layer			=> layer,
						count			=> count_total,
						log_threshold	=> log_threshold + 2);

					-- texts:
					propose_texts (
						module_cursor	=> active_module, 
						catch_zone		=> catch_zone,
						layer			=> layer,
						count			=> count_total,
						log_threshold	=> log_threshold + 2);

					-- placeholders:
					propose_placeholders (
						module_cursor	=> active_module, 
						catch_zone		=> catch_zone,
						layer			=> layer,
						count			=> count_total,
						log_threshold	=> log_threshold + 2);

					
				end if;
			end loop;
		end propose_objects;

				
	begin
		log (text => "proposing objects ...", level => log_threshold);
		log_indentation_up;

		propose_objects;		

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

				reset_proposed_objects (active_module, log_threshold + 1);
				
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

			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;	
			
			set_status (status_move_object);
			-- CS clear ?
			
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

				-- If find_objects has found only one object
				-- then the flag edit_process_running is set true.

			else
				-- Here the clarification procedure ends.
				-- An object has been selected via procedure clarify_object.
				-- By setting the status of the selected object
				-- as "moving", the selected object
				-- will be drawn according to object_point_of_attack and 
				-- the tool position.
				set_first_selected_object_moving;

				-- Furtheron, on the next call of this procedure
				-- the selected segment will be assigned its final position.
				
				set_edit_process_running;
				reset_request_clarification;
			end if;
			
		else
			finalize;
		end if;
	end move_object;

	

	
	
	
-- DELETE:
	
	procedure delete_object (
		point	: in type_vector_model)
	is
		
		-- Deletes the selected object.
		-- Resets variable preliminary_object:
		procedure finalize is 
			use et_modes.board;
			use et_undo_redo;
			use et_commit;

			object : constant type_object := get_first_object (
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
			
			set_status (status_delete_object);
			-- CS: clear ?

			reset_editing_process; -- prepare for a new editing process
		end finalize;

		
	begin
		if not clarification_pending then
			-- Locate all objects in the vicinity of the given point:
			find_objects (point);
			
			-- NOTE: If many segments have been found, then
			-- clarification is now pending.

			-- If find_objects has found only one object
			-- then the flag edit_process_running is set true.

			if edit_process_running then
				finalize;
			end if;
		else
			-- Here the clarification procedure ends.
			-- An object has been selected (indicated by selected_object)
			-- via procedure clarify_object.

			finalize;
		end if;
	end delete_object;


	
	
end et_canvas_board_conductors;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
