------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD TEXTS                               --
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
with gtk.text_view;						--use gtk.text_view;

with et_canvas;
with et_canvas_tool;					use et_canvas_tool;
with et_canvas_messages;				use et_canvas_messages;


with et_geometry;						use et_geometry;
with et_pcb_coordinates_2;				use et_pcb_coordinates_2;
use et_pcb_coordinates_2.pac_geometry_2;

with et_text;							use et_text;

with et_board_shapes_and_text;			use et_board_shapes_and_text;
use et_board_shapes_and_text.pac_text_board;

with et_pcb_stack;						use et_pcb_stack;

with et_silkscreen;						use et_silkscreen;
with et_assy_doc;						use et_assy_doc;
with et_stop_mask;						use et_stop_mask;
with et_conductor_text.boards;			use et_conductor_text.boards;



package et_canvas_board_texts is


	-- The text properties bar:
	type type_box_properties is record
		box_main	: gtk_hbox;
		
		-- This flag indicates that the
		-- box is being displayed. 
		-- The purpose of this flag is
		-- to prevent the box from being drawn
		-- multiple times:
		displayed	: boolean := false;
	end record;

	box_properties : type_box_properties;


	

	-- Before placing, moving, deleting or other operations we
	-- collect preliminary information using this type:
	type type_preliminary_text is record
		-- This flag indicates that the text has been
		-- clarified among the proposed texts:
		ready		: boolean := false;

		-- This tells the GUI whether the mouse or the
		-- cursor position is to be used when drawing the text:
		tool		: type_tool := MOUSE;
		
		category		: type_text_layer := type_text_layer'first;
		signal_layer	: type_signal_layer := signal_layer_default;
		face			: type_face := face_default;

		text			: type_text_fab_with_content := (
					size		=> 10.0,
					line_width	=> 1.0,
					position	=> origin_zero_rotation,
					alignment	=> text_alignment_default,
					content		=> empty_text_content);
		
		-- NOTE: The content will be extracted from selector entry_content.
		entry_content	: gtk.text_view.gtk_text_view;
	end record;

	-- The place where preliminary information of
	-- a text is stored:
	preliminary_text : type_preliminary_text;


	-- Clears preliminary_text.ready and box_properties.displayed.
	-- Removes the text properties bar.
	-- Clears the proposed texts.
	procedure reset_preliminary_text;

	
	-- Calls reset_preliminary_text if the verb is not VERB_PLACE.
	procedure remove_text_properties;
	

	-- Builds the box for the text properties and
	-- inserts it below the console.
	-- If the box is already on display, nothing happens.
	procedure show_text_properties;


	

	-- When texts are proposed, we classify them by
	-- their layer category and face:
	type type_proposed_text (cat : type_text_layer) is record
		case cat is
			when LAYER_CAT_ASSY =>
				doc_face	: type_face;
				doc_text	: type_doc_text; -- the text candidate itself
	
			when LAYER_CAT_SILKSCREEN =>
				silk_face	: type_face;
				silk_text	: type_silk_text;  -- the text candidate itself

			when LAYER_CAT_STOP =>
				stop_face	: type_face;
				stop_text	: type_stop_text;  -- the text candidate itself

			when LAYER_CAT_CONDUCTOR =>
				conductor_text	: type_conductor_text;  -- the text candidate itself
		end case;
	end record;

	-- All the proposed texts are collected via a list:
	package pac_proposed_texts is new indefinite_doubly_linked_lists (type_proposed_text);
	use pac_proposed_texts;

	-- Here we store the proposed texts:
	proposed_texts	: pac_proposed_texts.list;

	-- A selected text among the proposed texts is held here.
	-- After clarification (among the proposed texts),
	-- this cursor points to the selected text candidate:
	selected_text	: pac_proposed_texts.cursor;
	
	
	
	-- Returns true if the given text matches the text indicated
	-- by cursor selected_text (see above):
	function is_selected (
		text_cursor	: in pac_doc_texts.cursor;
		face		: in type_face)
		return boolean;

	function is_selected (
		text_cursor	: in pac_silk_texts.cursor;
		face		: in type_face)
		return boolean;

	function is_selected (
		text_cursor	: in pac_stop_texts.cursor;
		face		: in type_face)
		return boolean;

	function is_selected (
		text_cursor	: in pac_conductor_texts.cursor)
		return boolean;

	
	
	-- Clears the proposed_texts.
	-- Resets selected_text:
	procedure clear_proposed_texts;


	-- Returns the position of the given proposed text as string:
	function get_position (
		text_cursor : in pac_proposed_texts.cursor)
		return string;

	

	-- Advances the cursors in variable selected_text 
	-- on each call of this procedure.
	procedure select_text;


	-- Locates texts in the vicinity of the given point.
	-- Depending on how many texts have been found, the behaviour is:
	-- - If only one text found, then it is selected and 
	--   the flag preliminary_text.ready will be set.
	--   This causes the selected text to be drawn at the tool position.
	-- - If more than one text found, then clarification is requested.
	--   No text will be moved.
	--   The next call of this procedure sets preliminary_text.ready
	--   so that the selected text will be drawn at the tool position.
	--   The next call of this procedure assigns the final position 
	--   to the selected_text:
	procedure find_texts (
		point : in type_vector_model);

	
	
-- PLACING:

	-- to be output in the status bar:
	status_place_text : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to place text." 
		& status_hint_for_abort;


	-- Builds the final text-to-be-placed from the information
	-- provided by preliminary_text.
	-- Places the text at the given point:
	procedure place_text (
		point : in type_vector_model);

	

-- MOVE:

	status_move_text : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to move text." 
		& status_hint_for_abort;

	
	procedure move_text (
		tool	: in type_tool;
		point	: in type_vector_model);				   

	
	
end et_canvas_board_texts;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
