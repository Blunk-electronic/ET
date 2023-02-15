------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                CANVAS BOARD / ASSEMBLY DOCUMENTATION                     --
--                                                                          --
--                               S p e c                                    --
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

with ada.containers;   			       	use ada.containers;
with ada.containers.indefinite_doubly_linked_lists;

with gtk.box;							use gtk.box;
-- with gtk.text_view;						--use gtk.text_view;

with et_canvas_general;					use et_canvas_general;

with et_geometry;						use et_geometry;
with et_pcb_coordinates;				use et_pcb_coordinates;
use et_pcb_coordinates.pac_geometry_2;

-- with et_text;							use et_text;

with et_board_shapes_and_text;			use et_board_shapes_and_text;
-- use et_board_shapes_and_text.pac_text_board;

-- with et_pcb_stack;						use et_pcb_stack;

-- with et_silkscreen;						use et_silkscreen;
with et_assy_doc;						use et_assy_doc;
-- with et_stop_mask;						use et_stop_mask;
-- with et_conductor_text.boards;			use et_conductor_text.boards;



package et_canvas_board_assy_doc is



	

	-- Before placing, moving, deleting or other operations we
	-- collect preliminary information using this type:
	type type_preliminary_object is record
		-- This flag indicates that the object has been
		-- clarified among the proposed lines:
		ready	: boolean := false;

		-- This tells the GUI whether the mouse or the
		-- cursor position is to be used when drawing the object:
		tool	: type_tool := MOUSE;
		
		face	: type_face := face_default;

		shape	: type_shape;

		line	: type_doc_line;
		arc		: type_doc_arc;
		circle	: type_doc_circle;

		-- CS zone
	end record;

	-- The place where preliminary information of
	-- a line is stored:
	preliminary_object : type_preliminary_object;


	-- Clears preliminary_object.ready.
	-- Clears the proposed objects.
	procedure reset_preliminary_object;

	
	-- When objects are proposed, we classify them by
	-- their shape and face:
	type type_proposed_object (shape : type_shape) is record
		case shape is
			when LINE =>
				line_face	: type_face;
				line		: type_doc_line; -- the line candidate itself
	
			when ARC =>
				arc_face	: type_face;
				arc			: type_doc_arc;  -- the arc candidate itself

			when CIRCLE =>
				circle_face	: type_face;
				circle		: type_doc_circle;  -- the circle candidate itself

		end case;
	end record;


	-- All the proposed objects are collected via a list:
	package pac_proposed_objects is new indefinite_doubly_linked_lists (type_proposed_object);
	use pac_proposed_objects;

	-- Here we store the proposed objects:
	proposed_objects : pac_proposed_objects.list;

	-- A selected object among the proposed objects is held here.
	-- After clarification (among the proposed objects),
	-- this cursor points to the selected object candidate:
	selected_object : pac_proposed_objects.cursor;
	
	
	
	-- Returns true if the given object matches the object indicated
	-- by cursor selected_object (see above):
	function is_selected (
		line_cursor	: in pac_doc_lines.cursor;
		face		: in type_face)
		return boolean;

	function is_selected (
		arc_cursor	: in pac_doc_arcs.cursor;
		face		: in type_face)
		return boolean;

	function is_selected (
		circle_cursor	: in pac_doc_circles.cursor;
		face			: in type_face)
		return boolean;
	
	
	-- Clears the proposed_objects.
	-- Resets selected_object:
	procedure clear_proposed_objects;


	-- Returns the positions of (start, end, center) of the given proposed 
	-- object as string:
	function get_position (
		object_cursor : in pac_proposed_objects.cursor)
		return string;

	

	-- Advances the cursors in variable selected_object 
	-- on each call of this procedure.
	procedure select_object;


	-- Locates objects in the vicinity of the given point.
	-- Depending on how many objects have been found, the behaviour is:
	-- - If only one object found, then it is selected and 
	--   the flag preliminary_object.ready will be set.
	--   This causes the selected object to be drawn at the tool position.
	-- - If more than one object found, then clarification is requested.
	--   No object will be moved.
	--   The next call of this procedure sets preliminary_object.ready
	--   so that the selected object will be drawn at the tool position.
	--   The next call of this procedure assigns the final position 
	--   to the selected_object:
	procedure find_objects (
		point : in type_point);

	
	
-- PLACING:

	-- CS
	

-- MOVE:

	status_move_object : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to move object." 
		& status_hint_for_abort;

	
	procedure move_object (
		tool	: in type_tool;
		point	: in type_point);				   

	
	
end et_canvas_board_assy_doc;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
