------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                CANVAS BOARD / ASSEMBLY DOCUMENTATION                     --
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

with ada.text_io;						use ada.text_io;

-- with et_project.modules;				use et_project.modules;
with et_generic_module;					use et_generic_module;
with et_canvas_board_2;

with et_board_shapes_and_text;

with et_board_ops.assy_doc;				use et_board_ops.assy_doc;

with et_logging;						use et_logging;
with et_modes.board;
with et_undo_redo;
with et_commit;
with et_object_status;

with et_canvas_board_preliminary_object;	use et_canvas_board_preliminary_object;


package body et_canvas_board_assy_doc is

	use et_canvas_board_2.pac_canvas;
	
	use pac_doc_lines;
	use pac_doc_arcs;
	use pac_doc_circles;

	use et_board_shapes_and_text.pac_contours;
	-- use pac_contours;

	
	-- Outputs the selected line in the status bar:
	procedure show_selected_line (
		selected		: in type_line_segment;
		clarification	: in boolean := false)
	is 
		praeamble : constant string := "selected: ";
	begin
		if clarification then
			set_status (praeamble & to_string (element (selected.cursor))
				& ". " & status_next_object_clarification);
			-- CS face
		else
			set_status (praeamble & to_string (element (selected.cursor)));
			-- CS face
		end if;		
	end show_selected_line;


	
	-- Outputs the selected segment in the status bar:
	procedure show_selected_segment (
		selected		: in type_zone_segment;
		clarification	: in boolean := false)
	is 
		praeamble : constant string := "selected: ";
	begin
		if clarification then
			set_status (praeamble & to_string (selected.segment)
				& ". " & status_next_object_clarification);
			-- CS face
		else
			set_status (praeamble & to_string (selected.segment));
			-- CS face
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

			when CAT_VOID =>
				null; -- CS
		end case;	
	end show_selected_object;

	
	
	
	procedure select_object is 
		use et_object_status;
		selected_line : type_line_segment;

		use pac_objects;

		-- Gather all proposed objects:
		proposed_objects : constant pac_objects.list := 
			get_objects (active_module, PROPOSED, log_threshold + 1);

		proposed_object_cursor : pac_objects.cursor;

		-- We start with the first object that is currently selected:
		selected_object : type_object := 
			get_first_object (active_module, SELECTED, log_threshold + 1);

		ct : count_type := proposed_objects.length;
	begin
		log (text => "select_object (object count) " & count_type'image (ct),
			 level => log_threshold + 1);


		-- Locate the selected object among the proposed objects:
		proposed_object_cursor := proposed_objects.find (selected_object);

		-- Deselect the the proposed object:
		modify_status (
			module_cursor	=> active_module, 
			operation		=> (CLEAR, SELECTED),
			object_cursor	=> proposed_object_cursor, 
			log_threshold	=> log_threshold + 1);

		-- Advance to the next proposed object:
		next (proposed_object_cursor);

		-- If end of list reached, then proceed at 
		-- the begin of the list:
		if proposed_object_cursor = pac_objects.no_element then
			proposed_object_cursor := proposed_objects.first;
		end if;
		
		-- Select the proposed object:
		modify_status (
			module_cursor	=> active_module, 
			operation		=> (SET, SELECTED),
			object_cursor	=> proposed_object_cursor, 
			log_threshold	=> log_threshold + 1);

		-- Display the object in the status bar:
		show_selected_object (element (proposed_object_cursor));

	end select_object;


	

	-- This procedure searches for the first selected object
	-- and sets its status to "moving":
	procedure set_first_selected_object_moving is
		use et_board_ops.assy_doc;
		use et_object_status;

		use pac_segments;
		selected_segment : type_zone_segment;

		selected_line : type_line_segment;
		-- selected_arc : type_arc_segment;
	begin
		-- log (text => "set_first_selected_object_moving ...", level => log_threshold);

		-- First we look for a selected line and set it as "moving":
		selected_line := get_first_line (active_module, SELECTED, log_threshold + 1);

		if selected_line.cursor /= pac_doc_lines.no_element then
			
			modify_status (
				module_cursor	=> active_module, 
				operation		=> (SET, MOVING),
				line_cursor		=> selected_line.cursor, 
				log_threshold	=> log_threshold + 1);
		
		else
		-- If no line found, then we look for a selected segment of a zone.
		-- If one has been found, then we set it as "moving":
			selected_segment := get_first_segment (active_module, SELECTED, log_threshold + 1);

			if selected_segment.segment /= pac_segments.no_element then
				
				modify_status (
					module_cursor	=> active_module, 
					operation		=> (SET, MOVING),
					segment			=> selected_segment, 
					log_threshold	=> log_threshold + 1);
				
			end if;
		end if;
		
		-- CS arcs, circles

	end set_first_selected_object_moving;



	
	
	procedure find_objects (
	   point : in type_vector_model)
	is 
		face : type_face := TOP;

		count : natural := 0;
		count_total : natural := 0;

		
		procedure select_first_proposed is
			use et_object_status;

			proposed_object : constant type_object := 
				get_first_object (active_module, PROPOSED, log_threshold + 1);
		begin
			case proposed_object.cat is
				when CAT_LINE =>
			
					-- If a proposed line has been found, then set it as "selected":
					modify_status (active_module, proposed_object.line.cursor,
						(SET, SELECTED), log_threshold + 1);

					-- If only one line found, then show it in the status bar:
					if count_total = 1 then
						show_selected_line (proposed_object.line);
					end if;

					
				when CAT_ZONE_SEGMENT =>

					-- If a proposed zone segment has been found, then set it as "selected":
					modify_status (active_module, proposed_object.segment,
						(SET, SELECTED), log_threshold + 1);
					
					-- If only one segment found, then show it in the status bar:
					if count_total = 1 then
						show_selected_segment (proposed_object.segment);
					end if;

				when CAT_VOID =>
					null; -- CS
			end case;						
		end select_first_proposed;


		use et_modes.board;
		
	begin
		log (text => "locating objects ...", level => log_threshold);
		log_indentation_up;

		-- Propose lines in the vicinity of the given point:
		-- CS should depend on enabled top/bottom side
		propose_lines (active_module, point, TOP, 
			get_catch_zone (et_canvas_board_2.catch_zone), 
			count, log_threshold + 1);
		
		count_total := count;
		
		propose_lines (active_module, point, BOTTOM, 
			get_catch_zone (et_canvas_board_2.catch_zone), 
			count, log_threshold + 1);
		
		count_total := count_total + count;
		
		-- CS arcs, circles

		propose_segments (
			module_cursor	=> active_module, 
			point			=> point, 
			zone			=> get_catch_zone (et_canvas_board_2.catch_zone),
			face			=> TOP,
			count			=> count,
			log_threshold	=> log_threshold + 1);
		
		count_total := count_total + count;
			

		propose_segments (
			module_cursor	=> active_module, 
			point			=> point, 
			zone			=> get_catch_zone (et_canvas_board_2.catch_zone),
			face			=> BOTTOM,
			count			=> count,
			log_threshold	=> log_threshold + 1);
		
		count_total := count_total + count;

		
		-- evaluate the number of objects found here:
		case count_total is
			when 0 =>
				reset_request_clarification;
				reset_preliminary_object;
				reset_proposed_lines (active_module, log_threshold + 1);
				reset_proposed_segments (active_module, log_threshold + 1);
				
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
			use et_board_ops.assy_doc;
			use et_object_status;

			selected_line : type_line_segment;

			use pac_segments;
			selected_segment : type_zone_segment;
		begin
			log (text => "finalizing move ...", level => log_threshold);
			log_indentation_up;

			selected_line := get_first_line (active_module, SELECTED, log_threshold + 1);
			selected_segment := get_first_segment (active_module, SELECTED, log_threshold + 1);

			
			if selected_line.cursor /= pac_doc_lines.no_element then

				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				-- case object.shape is
				-- 	when LINE =>
						move_line (
							module_cursor	=> active_module,
							face			=> selected_line.face,
							line			=> element (selected_line.cursor),
							point_of_attack	=> object_point_of_attack,
							-- coordinates		=> ABSOLUTE,
							destination		=> point,
							log_threshold	=> log_threshold);

				-- 	when ARC =>
				-- 		null; -- CS
				-- 
				-- 	when CIRCLE =>
				-- 		null; -- CS
				-- end case;


				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

				
			elsif selected_segment.segment /= pac_segments.no_element then

					-- Commit the current state of the design:
					commit (PRE, verb, noun, log_threshold + 1);
					
					move_segment (
						module_cursor	=> active_module,
						segment			=> selected_segment,
						point_of_attack	=> object_point_of_attack,
						-- coordinates		=> ABSOLUTE,
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
			reset_proposed_lines (active_module, log_threshold + 1);
			reset_proposed_segments (active_module, log_threshold + 1);
		end finalize;
			
		
	begin
		-- Initially the preliminary object is not ready.
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
				-- An object has been selected via procedure select_object.
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
			use et_board_ops.assy_doc;
			use et_modes.board;
			use et_undo_redo;
			use et_commit;
			use et_object_status;

			selected_line : type_line_segment;
		begin
			log (text => "finalizing delete ...", level => log_threshold);
			log_indentation_up;

			selected_line := get_first_line (active_module, SELECTED, log_threshold + 1);

			if selected_line.cursor /= pac_doc_lines.no_element then

				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);

					-- case object.shape is
						-- when LINE =>
							delete (
								module_cursor	=> active_module,
								face			=> selected_line.face,
								line			=> element (selected_line.cursor),
								log_threshold	=> log_threshold);

					-- 	when ARC =>
					-- 		null; -- CS
     -- 
					-- 	when CIRCLE =>
					-- 		null; -- CS
					-- end case;

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			set_status (status_delete_object);
			
			reset_preliminary_object;
			reset_proposed_lines (active_module, log_threshold + 1);
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
			-- An object has been selected
			-- via procedure select_object.

			finalize;
			reset_request_clarification;
		end if;
	end delete_object;

	
end et_canvas_board_assy_doc;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
