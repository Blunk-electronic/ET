------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                CANVAS BOARD / ASSEMBLY DOCUMENTATION                     --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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
--                                                                          --
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

with et_project.modules;				use et_project.modules;

with et_canvas_board;
use et_canvas_board.pac_canvas;

with et_board_ops.assy_doc;

with et_logging;						use et_logging;



package body et_canvas_board_assy_doc is


	procedure reset_preliminary_object is begin
		preliminary_object.ready := false;
		preliminary_object.tool := MOUSE;
		clear_proposed_objects;
	end reset_preliminary_object;


	use pac_doc_lines;
	use pac_doc_arcs;
	use pac_doc_circles;

	
	-- Returns true if the given object matches the object indicated
	-- by cursor selected_object (see above):
	function is_selected (
		line_cursor	: in pac_doc_lines.cursor;
		face		: in type_face)
		return boolean
	is begin
		if proposed_objects.is_empty then
			return false;
		else
			if selected_object /= pac_proposed_objects.no_element then
				declare
					candidate : type_doc_line renames element (line_cursor);
					selected : type_proposed_object renames element (selected_object);
				begin
					if selected.line_face = face then
						if candidate = selected.line then
							return true;
						else
							return false;
						end if;
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
		arc_cursor	: in pac_doc_arcs.cursor;
		face		: in type_face)
		return boolean
	is begin
		if proposed_objects.is_empty then
			return false;
		else
			if selected_object /= pac_proposed_objects.no_element then
				declare
					candidate : type_doc_arc renames element (arc_cursor);
					selected : type_proposed_object renames element (selected_object);
				begin
					if selected.arc_face = face then
						if candidate = selected.arc then
							return true;
						else
							return false;
						end if;
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
		circle_cursor	: in pac_doc_circles.cursor;
		face			: in type_face)
		return boolean
	is begin
		if proposed_objects.is_empty then
			return false;
		else
			if selected_object /= pac_proposed_objects.no_element then
				declare
					candidate : type_doc_circle renames element (circle_cursor);
					selected : type_proposed_object renames element (selected_object);
				begin
					if selected.circle_face = face then
						if candidate = selected.circle then
							return true;
						else
							return false;
						end if;
					else
						return false;
					end if;
				end;
			else
				return false;
			end if;
		end if;
	end is_selected;

	
	
	-- Clears the proposed_objects.
	-- Resets selected_object:
	procedure clear_proposed_objects is begin
		proposed_objects.clear;
		selected_object := pac_proposed_objects.no_element;
	end clear_proposed_objects;


	function get_position (
		object_cursor : in pac_proposed_objects.cursor)
		return string
	is
		object : type_proposed_object renames element (object_cursor);
		separator : constant string := ", ";
	begin
		case object.shape is
			when LINE =>
				return keyword_face & to_string (object.line_face) & separator
					& to_string (object.line);

			when ARC =>
				return keyword_face & to_string (object.arc_face) & separator
					& to_string (object.arc);
				
			when CIRCLE =>
				return keyword_face & to_string (object.circle_face) & separator
					& to_string (object.circle);
		end case;
	end get_position;

	

	procedure select_object is begin
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
	   point : in type_point)
	is 
		face : type_face := TOP;
		
		lines : pac_doc_lines.list;
		arcs : pac_doc_arcs.list;
		circles : pac_doc_circles.list;

		procedure query_line (c : in pac_doc_lines.cursor) is begin
			proposed_objects.append ((
				shape		=> LINE,
				line_face	=> face,
				line		=> element (c)));
		end query_line;

		
		procedure collect is 
			use et_board_ops.assy_doc;
		begin
			lines := get_lines (current_active_module, face, point, catch_zone_default, log_threshold + 1);
			lines.iterate (query_line'access);

			-- CS arcs, circles
		end collect;
	
	begin
		log (text => "locating objects ...", level => log_threshold);
		log_indentation_up;

		-- Collect all objects in the vicinity of the given point
		-- and transfer them to the list proposed_texts:
		face := TOP;
		collect;
		face := BOTTOM;
		collect;
		
		-- evaluate the number of objects found here:
		case proposed_objects.length is
			when 0 =>
				reset_request_clarification;
				reset_preliminary_object;
				
			when 1 =>
				preliminary_object.ready := true;
				selected_object := proposed_objects.first;
				reset_request_clarification;
				
			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;

				-- preselect the object
				selected_object := proposed_objects.first;
		end case;
		
		log_indentation_down;
	end find_objects;

	
	
-- PLACING:

	
	

-- MOVE:


	
	procedure move_object (
		tool	: in type_tool;
		point	: in type_point)
	is

		-- Assigns the final position after the move to the selected text.
		-- Resets variable preliminary_text:
		procedure finalize is 
			use et_board_ops.assy_doc;
		begin
			log (text => "finalizing move ...", level => log_threshold);
			log_indentation_up;

			if selected_object /= pac_proposed_objects.no_element then
				declare
					object : type_proposed_object renames element (selected_object);
				begin
					case object.shape is
						when LINE =>
							move_line (
								module_cursor	=> current_active_module,
								face			=> object.line_face,
								line			=> object.line,
								coordinates		=> ABSOLUTE,
								point			=> point,
								log_threshold	=> log_threshold);

						when ARC =>
							null; -- CS

						when CIRCLE =>
							null; -- CS
					end case;
				end;
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
			
			if not clarification_pending then
				-- Locate all objects in the vicinity of the given point:
				find_objects (point);
				
				-- NOTE: If many objects have been found, then
				-- clarification is now pending.

				-- If find_objects has found only one object
				-- then the flag preliminary_object.read is set true.

			else
				-- Here the clarification procedure ends.
				-- A object has been selected (indicated by select_object)
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

	
	
end et_canvas_board_assy_doc;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16