------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      CANVAS BOARD / CONDUCTORS                           --
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

with et_generic_module;					use et_generic_module;
with et_canvas_board_2;
with et_board_ops;
with et_board_ops.conductors;			use et_board_ops.conductors;
with et_logging;						use et_logging;
with et_modes.board;
with et_undo_redo;
with et_commit;
with et_keywords;						use et_keywords;
with et_object_status;

with et_canvas_board_preliminary_object;	use et_canvas_board_preliminary_object;


package body et_canvas_board_conductors is

	use et_canvas_board_2.pac_canvas;
	
	use pac_conductor_lines;
	use pac_conductor_arcs;
	use pac_conductor_circles;


	
	-- Outputs the selected line in the status bar:
	procedure show_selected_line (
		selected		: in type_object_line;
		clarification	: in boolean := false)
	is 
		praeamble : constant string := "selected: ";
	begin
		if clarification then
			set_status (praeamble
				& to_string (element (selected.line_cursor), true) -- start/end point/width/layer
				& ". " & status_next_object_clarification);
				-- CS net name ?
		else
			set_status (praeamble
				& to_string (element (selected.line_cursor), true)); -- start/end point/width/layer
			-- CS net name ?
		end if;		
	end show_selected_line;


	

	-- Outputs the selected segment in the status bar:
	procedure show_selected_segment (
		selected		: in type_object_segment;
		clarification	: in boolean := false)
	is 
		praeamble : constant string := "selected: ";
	begin
		if clarification then
			null;
			-- set_status (praeamble & to_string (selected.segment)
			-- & " layer" & to_string (selected.laface) & ". " 
			-- CS read properties of zone to get the layer number
				-- & status_next_object_clarification);
		else
			null;
			-- set_status (praeamble & to_string (selected.segment));
				-- & " face" & to_string (selected.face) & ". ");
		end if;		
	end show_selected_segment;

	



	procedure show_selected_object (
		selected		: in type_object;
		clarification	: in boolean := false)
	is begin
		case selected.cat is
			when CAT_LINE =>
				show_selected_line (selected.line);

			when CAT_ZONE_SEGMENT =>
				show_selected_segment (selected.segment);

			when CAT_TEXT =>
				null;
				-- show_selected_text (selected.text);
				
			when CAT_VOID =>
				null; -- CS
		end case;	
	end show_selected_object;


	

	
	
	procedure clarify_object is
		use et_object_status;
		use et_board_ops.conductors;
		selected_line : type_object_line;
	begin
		selected_line := get_first_line (
			module_cursor	=> active_module, 
			flag			=> SELECTED, 
			freetracks		=> true,
			log_threshold	=> log_threshold + 1);
		
		modify_status (
			module_cursor	=> active_module, 
			operation		=> (CLEAR, SELECTED),
			line_cursor		=> selected_line.line_cursor, 
			freetracks		=> true,
			log_threshold	=> log_threshold + 1);
		
		next_proposed_line (
			module_cursor	=> active_module, 
			line			=> selected_line, 
			freetracks		=> true,
			log_threshold	=> log_threshold + 1);
		
		modify_status (
			module_cursor	=> active_module, 
			operation		=> (SET, SELECTED),
			line_cursor		=> selected_line.line_cursor, 
			freetracks		=> true,
			log_threshold	=> log_threshold + 1);
		
		show_selected_line (selected_line, clarification => true);
		
	end clarify_object;


	

	-- This procedure searches for the first selected object
	-- and sets its status to "moving":
	procedure set_first_selected_object_moving is
		use et_board_ops.conductors;
		use et_object_status;

		-- use et_board_shapes_and_text.pac_contours;
		-- use pac_segments;
		-- selected_segment : pac_segments.cursor; -- of a contour

		selected_line : type_object_line;
		-- selected_arc : type_arc_segment;
	begin
		log (text => "set_first_selected_object_moving ...", level => log_threshold);

		selected_line := get_first_line (
			module_cursor	=> active_module,
			flag			=> SELECTED, 
			freetracks		=> true,
			log_threshold	=> log_threshold + 1);
		
		-- CS arcs, circles, zones
		
		modify_status (
			module_cursor	=> active_module, 
			line_cursor		=> selected_line.line_cursor, 
			operation		=> (SET, MOVING),
			freetracks		=> true,
			log_threshold	=> log_threshold + 1);

	end set_first_selected_object_moving;



	
	
	procedure find_objects (
	   point : in type_vector_model)
	is 
		use et_board_ops;
		use et_board_ops.conductors;

		count_total : natural := 0;
		
		
		procedure propose_lines (layer : in type_signal_layer) is 
			count : natural := 0;
		begin
			propose_lines (
				module_cursor	=> active_module, 
				point			=> point, 
				layer			=> layer, 
				zone			=> get_catch_zone (et_canvas_board_2.catch_zone),
				count			=> count,
				freetracks		=> true, 
				log_threshold	=> log_threshold + 1);
			
			count_total := count_total + count;
		end propose_lines;


		procedure select_first_proposed is 
			proposed_line : type_object_line;
			use et_object_status;
		begin
			proposed_line := get_first_line (
				module_cursor	=> active_module,
				flag			=> PROPOSED, 
				freetracks		=> true,
				log_threshold	=> log_threshold + 1);

			modify_status (
				module_cursor	=> active_module, 
				line_cursor		=> proposed_line.line_cursor, 
				operation		=> (SET, SELECTED),
				freetracks		=> true,
				log_threshold	=> log_threshold + 1);			
			
			-- If only one line found, then show it in the status bar:
			if count_total = 1 then
				show_selected_line (proposed_line);		
			end if;
		end select_first_proposed;
		

		use et_modes.board;
		
	begin
		log (text => "locating objects ...", level => log_threshold);
		log_indentation_up;

		-- Propose all objects in the vicinity of the given point.
		-- CS should depend on enabled signal layers.
		for l in 1 .. get_deepest_conductor_layer (active_module) loop
			propose_lines (l);
		end loop;
		
		-- CS arcs, circles, zones
		
		-- Evaluate the number of objects found here:
		case count_total is
			when 0 =>
				reset_request_clarification;
				reset_preliminary_object;

				reset_proposed_lines (
					module_cursor	=> active_module, 
					freetracks		=> true,
					log_threshold	=> log_threshold + 1);

				
			when 1 =>
				object_ready := true;
				select_first_proposed;

				if verb = VERB_MOVE then
					set_first_selected_object_moving;
				end if;
				
				reset_request_clarification;

				
			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;
				
				-- preselect the object
				-- selected_object := proposed_objects.first;
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
			use et_board_ops.conductors;

			selected_line : type_object_line;

			use et_object_status;

		begin
			log (text => "finalizing move ...", level => log_threshold);
			log_indentation_up;

			selected_line := get_first_line (
				module_cursor	=> active_module,
				flag			=> SELECTED, 
				freetracks		=> true,
				log_threshold	=> log_threshold + 1);

			
			if selected_line.line_cursor /= pac_conductor_lines.no_element then

				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);

				move_line_freetrack (
					module_cursor	=> active_module,
					line			=> element (selected_line.line_cursor),
					point_of_attack	=> object_point_of_attack,
					destination		=> point,
					log_threshold	=> log_threshold);
				
				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			set_status (status_move_object);
			
			reset_preliminary_object;

			reset_proposed_lines (
				module_cursor	=> active_module, 
				freetracks		=> true,
				log_threshold	=> log_threshold + 1);

		end finalize;
			
		
	begin
		-- Initially the preliminary_object is not ready.
		if not object_ready then

			-- Set the tool being used:
			object_tool := tool;

			object_point_of_attack := point;
			
			if not clarification_pending then
				-- Locate all objects in the vicinity of the given point:
				find_objects (point);
				
				-- NOTE: If many objects have been found, then
				-- clarification is now pending.

				-- If find_objects has found only one object
				-- then the flag object_ready is set true.

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
				
				object_ready := true;
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

			use et_board_ops.conductors;
			selected_line : type_object_line;

			use pac_conductor_lines;
			use et_object_status;

		begin
			log (text => "finalizing delete ...", level => log_threshold);
			log_indentation_up;

			selected_line := get_first_line (
				module_cursor	=> active_module,
				flag			=> SELECTED, 
				freetracks		=> true,
				log_threshold	=> log_threshold + 1);
			
			if selected_line.line_cursor /= pac_conductor_lines.no_element then

				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);

				delete_line_freetrack (
					module_cursor	=> active_module,
					line			=> element (selected_line.line_cursor),
					log_threshold	=> log_threshold);
				
				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);
			else
				log (text => "nothing to do", level => log_threshold);
			end if;

			-- CS arcs, circles
			
			log_indentation_down;			
			set_status (status_delete_object);
			
			reset_preliminary_object;
			
			reset_proposed_lines (
				module_cursor	=> active_module, 
				freetracks		=> true,
				log_threshold	=> log_threshold + 1);
			
		end finalize;

		
	begin
		if not clarification_pending then
			-- Locate all objects in the vicinity of the given point:
			find_objects (point);
			
			-- NOTE: If many segments have been found, then
			-- clarification is now pending.

			-- If find_objects has found only one object
			-- then the flag object_ready is set true.

			if object_ready then
				finalize;
			end if;
		else
			-- Here the clarification procedure ends.
			-- An object has been selected (indicated by selected_object)
			-- via procedure selected_object.

			finalize;
			reset_request_clarification;
		end if;
	end delete_object;


	
	
end et_canvas_board_conductors;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
