------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD OUTLINE                             --
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

with ada.text_io;					use ada.text_io;
with ada.strings;					use ada.strings;
with ada.strings.fixed; 			use ada.strings.fixed;

with glib.values;

with gdk.types;							use gdk.types;
with gdk.event;							use gdk.event;
with gdk.types.keysyms;					use gdk.types.keysyms;

with gtk.widget;						use gtk.widget;

with gtk.cell_renderer_text;		
with gtk.cell_layout;        		
with gtk.list_store;				
with gtk.tree_model;

with gtk.gentry;						use gtk.gentry;
with gtk.container;						use gtk.container;

with et_generic_module;					use et_generic_module;
with et_canvas_board_2;

with et_board_shapes_and_text;

with et_board_ops;						use et_board_ops;
with et_board_ops.board_contour;		use et_board_ops.board_contour;
with et_modes.board;

with et_display;						use et_display;
with et_display.board;					use et_display.board;

with et_logging;						use et_logging;
with et_string_processing;				use et_string_processing;
with et_exceptions;						use et_exceptions;

with et_undo_redo;
with et_commit;

with et_object_status;


with et_canvas_board_preliminary_object;	use et_canvas_board_preliminary_object;



package body et_canvas_board_outline is

	
	procedure make_path (
		tool	: in type_tool;
		point	: in type_vector_model)
	is
		PC : type_preliminary_object renames preliminary_object;
		line : type_line;

		
		procedure add_line is 
			use et_modes.board;
			use et_undo_redo;
			use et_commit;
			
			use et_board_shapes_and_text;
			use et_board_shapes_and_text.pac_contours;

			-- A temporary contour consisting of a single 
			-- segment:
			c : type_contour;
		begin
			-- Add the line to the temporary contour:
			append_segment (c, to_segment (line));
							
			-- Commit the current state of the design:
			commit (PRE, verb, noun, log_threshold + 1);

			-- Add the temporary contour to the board:
			draw_outline (
				module_cursor	=> active_module,
				outline			=> (c with null record),
				log_threshold	=> log_threshold);
			

			-- Commit the new state of the design:
			commit (POST, verb, noun, log_threshold + 1);			
		end add_line;

		
	begin -- make_path
		-- put_line ("make_path");
		
		-- Set the tool being used for this path so that procedure
		-- draw_path_live (for example in et_canvas_board_2-draw_assy_doc)
		-- knows where to get the end point from.
		PC.tool := tool;

		PC.category := LAYER_CAT_OUTLINE;
		

		-- Initally the preliminary_object is NOT ready. Nothing will be drawn.
		-- Upon the first calling of this procedure the start point of the
		-- path will be set.
		
		if not PC.ready then
			-- set start point:
			PC.path.start_point := point;

			-- Allow drawing of the path:
			preliminary_object.ready := true;

			set_status (status_start_point & to_string (PC.path.start_point) & ". " &
				status_press_space & status_set_end_point & status_hint_for_abort);

		else -- preliminary_object IS ready

			-- Start a new path only if the given point differs from 
			-- the start point of the current path:
			if point /= PC.path.start_point then

				-- Complete the path by setting its end point.
				-- The the current bend point (if there is one) into account:
				
				if PC.path.bended = NO then
					PC.path.end_point := point;

					-- insert a single line:
					line.start_point := PC.path.start_point;
					line.end_point   := PC.path.end_point;
					add_line;
					
				else
					-- The path is bended. The bend point has been computed
					-- interactively while moving the mouse or the cursor.
					-- See for example procedure draw_path in et_canvas_board_2-draw_assy_doc.

					-- insert first line of the path:
					line.start_point := PC.path.start_point;
					line.end_point   := PC.path.bend_point;
					add_line;

					
					-- insert second line of the path:
					PC.path.end_point := point;
					line.start_point := PC.path.bend_point;
					line.end_point   := PC.path.end_point;
					add_line;
				end if;

				-- Set start point of path so that a new
				-- path can be drawn:
				PC.path.start_point := point;
				
			else
				reset_preliminary_object;
			end if;
		end if;			
	end make_path;
		




	procedure show_selected_segment (
		selected		: in pac_contours.pac_segments.cursor;
		clarification	: in boolean := false)
	is 
		use pac_contours;
		use pac_segments;
		praeamble : constant string := "selected: ";
	begin
		
		if clarification then
			set_status (praeamble & to_string (selected)
				& ". " & status_next_object_clarification);
			-- CS face
		else
			set_status (praeamble & to_string (selected));
			-- CS face
		end if;		
	end show_selected_segment;


	
	

	procedure select_segment is 
		use pac_contours;
		use et_object_status;
		selected_object : pac_segments.cursor;
	begin
		selected_object := get_first_segment (active_module, SELECTED, log_threshold + 1);

		modify_status (
			module_cursor	=> active_module, 
			operation		=> (CLEAR, SELECTED),
			segment_cursor	=> selected_object, 
			log_threshold	=> log_threshold + 1);
		
		next_proposed_segment (active_module, selected_object, log_threshold + 1);
		
		modify_status (
			module_cursor	=> active_module, 
			operation		=> (SET, SELECTED),
			segment_cursor	=> selected_object, 
			log_threshold	=> log_threshold + 1);
		
		show_selected_segment (selected_object, clarification => true);
	end select_segment;




	-- This procedure searches for the first selected segment
	-- and sets its status to "moving":
	procedure set_first_selected_segment_moving is
		use et_board_ops.board_contour;
		use et_object_status;

		use pac_contours;
		use pac_segments;
		selected_segment : pac_segments.cursor;
	begin
		-- log (text => "set_first_selected_segment_moving ...", level => log_threshold);

		selected_segment := get_first_segment (active_module, SELECTED, log_threshold + 1);

		modify_status (
			module_cursor	=> active_module, 
			operation		=> (SET, MOVING),
			segment_cursor	=> selected_segment, 
			log_threshold	=> log_threshold + 1);

	end set_first_selected_segment_moving;



	

	procedure find_objects (
	   point : in type_vector_model)
	is 
		face : type_face := TOP;

		count : natural := 0;
		count_total : natural := 0;

		
		procedure select_first_proposed is
			use et_object_status;
			use pac_contours;
			proposed_object : pac_segments.cursor;
		begin
			proposed_object := get_first_segment (active_module, PROPOSED, log_threshold + 1);
  
			modify_status (active_module, proposed_object, (SET, SELECTED), log_threshold + 1);
  
			-- If only one line found, then show it in the status bar:
			if count = 1 then
				show_selected_segment (proposed_object);
			end if;
		end select_first_proposed;


		use et_modes.board;
	
	begin
		log (text => "locating objects ...", level => log_threshold);
		log_indentation_up;

		-- Propose lines in the vicinity of the given point:
		propose_segments (active_module, point,
			get_catch_zone (et_canvas_board_2.catch_zone), 
			count, log_threshold + 1);
		
		count_total := count;
		
		-- CS arcs, circles
		
		-- evaluate the number of objects found here:
		case count_total is
			when 0 =>
				reset_request_clarification;
				reset_preliminary_object;
				reset_proposed_segments (active_module, log_threshold + 1);
				
			when 1 =>
				preliminary_object.ready := true;
				select_first_proposed;

				if verb = VERB_MOVE then
					set_first_selected_segment_moving;
				end if;
				
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
			use et_modes.board;
			use et_undo_redo;
			use et_commit;
			use et_board_ops.board_contour;
			use et_object_status;

			use pac_contours;
			use pac_segments;
			selected_segment : pac_segments.cursor;
		begin
			log (text => "finalizing move ...", level => log_threshold);
			log_indentation_up;

			selected_segment := get_first_segment (active_module, SELECTED, log_threshold + 1);
			-- CS arcs
			
			if selected_segment /= pac_segments.no_element then

				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				move_segment (
					module_cursor	=> active_module,
					segment			=> selected_segment,
					point_of_attack	=> point_of_attack,
					-- coordinates		=> ABSOLUTE,
					destination		=> point,
					log_threshold	=> log_threshold);


				modify_status (
					module_cursor	=> active_module, 
					operation		=> (CLEAR, MOVING),
					segment_cursor	=> selected_segment, 
					log_threshold	=> log_threshold + 1);

				
				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			set_status (status_move_object);
			
			reset_preliminary_object;
			reset_proposed_segments (active_module, log_threshold + 1);
		end finalize;
			
			
		
		
	begin
		-- Initially the preliminary_object is not ready.
		if not preliminary_object.ready then

			-- Set the tool being used:
			preliminary_object.tool := tool;
			object_tool := tool;

			point_of_attack := point;
			
			if not clarification_pending then
				-- Locate all objects in the vicinity of the given point:
				find_objects (point);
				
				-- NOTE: If many objects have been found, then
				-- clarification is now pending.

				-- If find_objects has found only one object
				-- then the flag preliminary_object.ready is set true.

			else
				-- Here the clarification procedure ends.
				-- A segment has been selected via procedure select_segment.
				-- By setting the status of the selected segment 
				-- as "moving", the selected segment
				-- will be drawn according to point_of_attack and 
				-- the tool position.
				set_first_selected_segment_moving;
				
				-- Furtheron, on the next call of this procedure
				-- the selected segment will be assigned its final position.

				reset_request_clarification;
				preliminary_object.ready := true;
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
			use et_board_ops.board_contour;
			use et_modes.board;
			use et_undo_redo;
			use et_commit;
			use et_object_status;

			use pac_contours;
			use pac_segments;
			selected_segment : pac_segments.cursor;
		begin
			log (text => "finalizing delete ...", level => log_threshold);
			log_indentation_up;

			selected_segment := get_first_segment (active_module, SELECTED, log_threshold + 1);
			-- CS arcs

			if selected_segment /= pac_segments.no_element then

				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);

				delete_segment (
					module_cursor	=> active_module,
					segment			=> selected_segment,
					log_threshold	=> log_threshold);

				
				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			set_status (status_delete_object);
			
			reset_preliminary_object;
			reset_proposed_segments (active_module, log_threshold + 1);
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
			-- An object has been selected
			-- via procedure selected_object.

			finalize;
			reset_request_clarification;
		end if;
	end delete_object;

	
	
	
end et_canvas_board_outline;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
