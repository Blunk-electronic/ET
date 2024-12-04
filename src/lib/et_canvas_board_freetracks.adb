------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      CANVAS BOARD / FREETRACKS                           --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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


package body et_canvas_board_freetracks is


	use et_canvas_board_2.pac_canvas;
	

	-- Clears the proposed_objects.
	-- Resets selected_object:
	procedure clear_proposed_objects is begin
		proposed_objects.clear;
		selected_object := pac_proposed_objects.no_element;
	end clear_proposed_objects;


	
	procedure reset_preliminary_object is begin
		preliminary_object.ready := false;
		preliminary_object.tool := MOUSE;
		clear_proposed_objects;
	end reset_preliminary_object;

	
	use pac_conductor_lines;
	use pac_conductor_arcs;

	use pac_conductor_circles;


	-- Outputs the selected line in the status bar:
	procedure show_selected_line (
		selected		: in et_board_ops.conductors.type_line_segment;
		clarification	: in boolean := false)
	is 
	begin
		if clarification then
			set_status (
				to_string (element (selected.line_cursor), true) -- start/end point/width/layer
				& ". " & status_next_object_clarification);
		else
			set_status (to_string (element (selected.line_cursor), true)); -- start/end point/width/layer
		end if;		
	end show_selected_line;


	

	-- Returns true if the given object matches the object indicated
	-- by cursor selected_object (see above):
	function is_selected (
		line_cursor	: in pac_conductor_lines.cursor)
		return boolean
	is begin
		if proposed_objects.is_empty then
			return false;
		else
			if selected_object /= pac_proposed_objects.no_element then
				declare
					candidate : type_conductor_line renames element (line_cursor);
					selected : type_proposed_object renames element (selected_object);
				begin
					-- CS test selected.shape
					if candidate = selected.line then
						return true;
					else
						return false;
					end if;
				end;
			else
				return false;
			end if;
		end if;
	end is_selected;


	

	function is_selected (
		arc_cursor	: in pac_conductor_arcs.cursor)
		return boolean
	is begin
		if proposed_objects.is_empty then
			return false;
		else
			if selected_object /= pac_proposed_objects.no_element then
				declare
					candidate : type_conductor_arc renames element (arc_cursor);
					selected : type_proposed_object renames element (selected_object);
				begin
					-- CS test selected.shape
					if candidate = selected.arc then
						return true;
					else
						return false;
					end if;
				end;
			else
				return false;
			end if;
		end if;
	end is_selected;


	

	function is_selected (
		circle_cursor	: in pac_conductor_circles.cursor)
		return boolean
	is begin
		if proposed_objects.is_empty then
			return false;
		else
			if selected_object /= pac_proposed_objects.no_element then
				declare
					candidate : type_conductor_circle renames element (circle_cursor);
					selected : type_proposed_object renames element (selected_object);
				begin
					-- CS test selected.shape
					if candidate = selected.circle then
						return true;
					else
						return false;
					end if;
				end;
			else
				return false;
			end if;
		end if;
	end is_selected;




	function get_position (
		object_cursor : in pac_proposed_objects.cursor)
		return string
	is
		object : type_proposed_object renames element (object_cursor);
		separator : constant string := ", ";
	begin
		case object.shape is
			when LINE =>
				return keyword_layer & to_string (object.line.layer) & separator
					& to_string (object.line);

			when ARC =>
				return keyword_layer & to_string (object.arc.layer) & separator
					& to_string (object.arc);
				
			when CIRCLE =>
				return keyword_layer & to_string (object.circle.layer) & separator
					& to_string (object.circle);
		end case;
	end get_position;
	

	
	
	procedure select_object is
	begin
		if next (selected_object) /= pac_proposed_objects.no_element then
			next (selected_object);
		else
			selected_object := proposed_objects.first;
		end if;

		-- show the position of the selected object in the status bar
		set_status ("selected object: " & get_position (selected_object)
			& ". " & status_next_object_clarification);

	end select_object;


	

	procedure find_objects (
	   point : in type_vector_model)
	is 
		use et_board_ops;
		use et_board_ops.conductors;

		count_total : natural := 0;
		
		
		procedure collect (layer : in type_signal_layer) is 
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
			
			-- CS arcs, circles
			count_total := count_total + count;
		end collect;


		procedure select_first_proposed is 
			proposed_line : type_line_segment;
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
				log_threshold	=> log_threshold + 1);
			
			
			-- If only one line found, then show it in the status bar:
			if count_total = 1 then
				show_selected_line (proposed_line);		
			end if;
		end select_first_proposed;
		
	
	begin
		log (text => "locating objects ...", level => log_threshold);
		log_indentation_up;

		-- Collect all objects in the vicinity of the given point
		-- and transfer them to the list proposed_objects:
		-- CS should depend on enabled signal layers.
		for l in 1 .. get_deepest_conductor_layer (active_module) loop
			collect (l);
		end loop;
		
		
		-- Evaluate the number of objects found here:
		case count_total is
			when 0 =>
				reset_request_clarification;
				reset_preliminary_object;
				
			when 1 =>
				preliminary_object.ready := true;
				select_first_proposed;
				reset_request_clarification;
				
			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;
				
				-- preselect the object
				selected_object := proposed_objects.first;
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
		begin
			log (text => "finalizing move ...", level => log_threshold);
			log_indentation_up;

			if selected_object /= pac_proposed_objects.no_element then

				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);

				declare
					object : type_proposed_object renames element (selected_object);
				begin
					case object.shape is
						when LINE =>
							null;
							-- move_line (
							-- 	module_cursor	=> active_module,
							-- 	face			=> object.line_face,
							-- 	line			=> object.line,
							-- 	point_of_attack	=> preliminary_object.point_of_attack,
							-- 	destination		=> point,
							-- 	log_threshold	=> log_threshold);

						when ARC =>
							null; -- CS

						when CIRCLE =>
							null; -- CS
					end case;
				end;

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			set_status (status_move_object);
			reset_preliminary_object;
		end finalize;
			
		
	begin
		-- Initially the preliminary_object is not ready.
		if not preliminary_object.ready then

			-- Set the tool being used:
			preliminary_object.tool := tool;

			preliminary_object.point_of_attack := point;
			
			if not clarification_pending then
				-- Locate all objects in the vicinity of the given point:
				find_objects (point);
				
				-- NOTE: If many objects have been found, then
				-- clarification is now pending.

				-- If find_objects has found only one object
				-- then the flag preliminary_object.ready is set true.

			else
				-- Here the clarification procedure ends.
				-- A object has been selected (indicated by selected_object)
				-- via procedure select_object.
				-- By setting preliminary_object.ready, the selected
				-- object will be drawn at the tool position
				-- when objects are drawn on the canvas.
				-- Furtheron, on the next call of this procedure
				-- the selected object will be assigned its final position.
				preliminary_object.ready := true;
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
		begin
			log (text => "finalizing delete ...", level => log_threshold);
			log_indentation_up;

			if selected_object /= pac_proposed_objects.no_element then

				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				declare
					object : type_proposed_object renames element (selected_object);
				begin
					case object.shape is
						when LINE =>
							-- CS
							null;
							-- delete (
							-- 	module_cursor	=> active_module,
							-- 	face			=> object.line_face,
							-- 	line			=> object.line,
							-- 	log_threshold	=> log_threshold);

						when ARC =>
							null; -- CS

						when CIRCLE =>
							null; -- CS
					end case;
				end;

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			set_status (status_delete_object);
			reset_preliminary_object;
		end finalize;


		
	begin
		if not clarification_pending then
			-- Locate all objects in the vicinity of the given point:
			find_objects (point);
			
			-- NOTE: If many segments have been found, then
			-- clarification is now pending.

			-- If find_objects has found only one object
			-- then the flag preliminary_object.ready is set true.

			if preliminary_object.ready then
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


	
	
end et_canvas_board_freetracks;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
