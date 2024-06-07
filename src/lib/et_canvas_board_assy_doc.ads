------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                CANVAS BOARD / ASSEMBLY DOCUMENTATION                     --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2023                                                --
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

with ada.containers;   			       	use ada.containers;
with ada.containers.indefinite_doubly_linked_lists;

with gtk.box;							use gtk.box;

with et_canvas_general;					use et_canvas_general;

with et_geometry;						use et_geometry;
with et_pcb_coordinates;				use et_pcb_coordinates;
use et_pcb_coordinates.pac_geometry_2;

with et_board_shapes_and_text;			use et_board_shapes_and_text;

with et_assy_doc;						use et_assy_doc;
with et_canvas_board_lines;				use et_canvas_board_lines;


package et_canvas_board_assy_doc is
	

	-- Before placing, moving, deleting or other operations we
	-- collect preliminary information using this type:
	type type_preliminary_object is record
		-- This flag indicates that the object has been
		-- clarified among the proposed objects:
		ready	: boolean := false;

		-- This tells the GUI whether the mouse or the
		-- cursor position is to be used when drawing the object:
		tool	: type_tool := MOUSE;

		point_of_attack : type_vector_model;
		
		face	: type_face := face_default;

		shape	: type_shape;

		line	: type_doc_line;
		arc		: type_doc_arc;
		circle	: type_doc_circle;

		-- CS zone
	end record;

	-- The place where preliminary information of
	-- an object is stored:
	preliminary_object : type_preliminary_object;


	-- Clears preliminary_object.ready.
	-- Clears the proposed objects.
	procedure reset_preliminary_object;

	

	-- Advances the cursors in variable selected_object 
	-- on each call of this procedure.
	procedure select_object;


	-- Locates objects in the vicinity of the given point
	-- and stores them in proposed_objects.
	-- Depending on how many objects have been found, the behaviour is:
	-- - If only one object found, then it is selected and 
	--   the flag preliminary_object.ready will be set.
	-- - If more than one object found, then clarification is requested.
	--   The first object of them is selected.
	procedure find_objects (
		point : in type_vector_model);

	
	
-- PLACING:

	-- see package et_canvas_board_lines
	

	

-- MOVE:

	status_move_object : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to move object in assy doc." 
		& status_hint_for_abort;

	
	procedure move_object (
		tool	: in type_tool;
		point	: in type_vector_model);				   



-- DELETE:

	status_delete_object : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to delete object in assy doc." 
		& status_hint_for_abort;

	
	procedure delete_object (
		point	: in type_vector_model);				   

	
	
end et_canvas_board_assy_doc;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
