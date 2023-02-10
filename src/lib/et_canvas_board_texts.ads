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

with et_canvas_general;					use et_canvas_general;

with et_geometry;						use et_geometry;
with et_pcb_coordinates;				use et_pcb_coordinates;
use et_pcb_coordinates.pac_geometry_2;

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


	

	-- -- When texts in non-conductor layers are proposed,
	-- -- we store them in lists according to layer category and face:
	-- type type_proposed_assy_doc is record
	-- 	top, bottom : pac_doc_texts.list;
	-- end record;
 -- 
	-- type type_proposed_silkscreen is record
	-- 	top, bottom : pac_silk_texts.list;
	-- end record;
 -- 
	-- type type_proposed_stop_mask is record
	-- 	top, bottom : pac_stop_texts.list;
	-- end record;
 -- 
	-- -- All proposed texts are stored via this record type in sub-lists:
	-- type type_proposed_texts is record
	-- 	assy_doc	: type_proposed_assy_doc;
	-- 	silkscreen	: type_proposed_silkscreen;
	-- 	stop_mask	: type_proposed_stop_mask;
	-- 	conductors	: pac_conductor_texts.list;
	-- end record;
 -- 
	-- -- The proposed texts are finally stored here:
	-- proposed_texts : type_proposed_texts;


	type type_proposed_text (cat : type_text_layer) is record
		case cat is
			when LAYER_CAT_ASSY =>
				doc_face	: type_face;
				doc_text	: pac_doc_texts.cursor;
	
			when LAYER_CAT_SILKSCREEN =>
				silk_face	: type_face;
				silk_text	: pac_silk_texts.cursor;

			when LAYER_CAT_STOP =>
				stop_face	: type_face;
				stop_text	: pac_stop_texts.cursor;

			when LAYER_CAT_CONDUCTOR =>
				conductor_text	: pac_conductor_texts.cursor;
		end case;
	end record;

	package pac_proposed_texts is new indefinite_doubly_linked_lists (type_proposed_text);
	use pac_proposed_texts;

	proposed_texts	: pac_proposed_texts.list;
	selected_text	: pac_proposed_texts.cursor;
	

-- 	-- After clarification (among the proposed texts),
-- 	-- a cursor points to the selected text.
-- 	-- We use a cursor according to layer category and face:
-- 	type type_selected_assy_doc is record
-- 		top, bottom : pac_doc_texts.cursor;
-- 	end record;
-- 	
-- 	type type_selected_silkscreen is record
-- 		top, bottom : pac_silk_texts.cursor;
-- 	end record;
-- 
-- 	type type_selected_stop_mask is record
-- 		top, bottom : pac_stop_texts.cursor;
-- 	end record;


	-- When a certain text has been selected, then one of
	-- these cursors points to the actual text.
	-- Only two cases exist: 
	-- 1. Nothing is selected. Means all cursors point to no_element.
	-- 2. Only one text is selected. Means ONE of all the cursors points
	--    to a text:
	-- type type_selected_text is record
	-- 	assy_doc	: type_selected_assy_doc;
	-- 	silkscreen	: type_selected_silkscreen;
	-- 	stop_mask	: type_selected_stop_mask;
	-- 	conductors	: pac_conductor_texts.cursor;
	-- end record;
 -- 
	-- selected_text : type_selected_text;


-- 	type type_selected_query_result (
-- 		empty	: boolean := true;								
-- 		cat 	: type_text_layer := LAYER_CAT_ASSY) -- relevant if count is 1
-- 	is record
-- 		case empty is
-- 			when TRUE  => null; -- nothing selected
-- 			when FALSE =>
-- 				
-- 				case cat is
-- 					when LAYER_CAT_CONDUCTOR => 
-- 						conductor : type_conductor_text;
-- 						
-- 					when LAYER_CAT_SILKSCREEN => 
-- 						silkscreen_face	: type_face;
-- 						silkscreen		: type_silk_text;
-- 						
-- 					when LAYER_CAT_ASSY => 
-- 						assy_doc_face	: type_face;
-- 						assy_doc		: type_doc_text;
-- 						
-- 					when LAYER_CAT_STOP => 
-- 						stop_mask_face	: type_face;
-- 						stop_mask		: type_stop_text;
-- 				end case;
-- 
-- 		end case;
-- 	end record;
-- 
	
	-- Queries the components of variable selected_text
	-- and returns information about the actual selected text candidate:
	-- function get_selected return type_selected_query_result;
	
	
	-- Returns true if the given text matches the text indicated
	-- by selected_text:
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


	-- Advances the cursors in variable selected_text 
	-- on each call of this procedure.
	procedure select_text;

	-- function get_number_of_proposed_texts
	-- 	return count_type; -- CS subtype ?
 -- 
	-- function get_first_proposed
	-- 	return type_selected_text;
	

	procedure find_texts (
		point : in type_point);

	
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
		point : in type_point);

	

-- MOVE:

	status_move_text : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to move text." 
		& status_hint_for_abort;

	
	procedure move_text (
		tool	: in type_tool;
		point	: in type_point);				   

	
	
end et_canvas_board_texts;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
