------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         CONDUCTOR TEXT                                  --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   to do:

with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;

with et_pcb_coordinates;		use et_pcb_coordinates;
with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_design_rules;			use et_design_rules;
with et_text;					use et_text;
with et_pcb_stack;				use et_pcb_stack;
with et_logging;				use et_logging;


package et_conductor_text is

	--use pac_geometry_2;

	
	use pac_geometry_brd;
	use pac_polygons;

	
	use et_board_shapes_and_text.pac_text_fab;


	function face_to_mirror (
		f : in type_face)
		return type_vector_text_mirrored;


	type type_conductor_text 
		is new type_text_fab_with_content with
	record
		vectors	: type_vector_text;
	end record;

	
	function to_polygons (
		text : in type_conductor_text)
		return pac_polygon_list.list;

	
	
	--package pac_conductor_line_segments is new
		--doubly_linked_lists (type_conductor_line_segment);


	--type type_conductor_text_package is new pac_text_fab.type_text_fab_with_content with null record;


	
	--package pac_conductor_texts_package is new 
		--doubly_linked_lists (type_conductor_text_package);

		

	
	--type type_conductor_text_board is new type_conductor_text_package with record
		--layer	: type_signal_layer := type_signal_layer'first;
	--end record;

	--function to_string (text : in type_conductor_text)
		--return string;
	
	--package pac_conductor_texts_board is new 
		--doubly_linked_lists (type_conductor_text_board);

	--use pac_conductor_texts_board;
		

	---- Logs the properties of the given text.
	--procedure text_conductor_properties (
		--cursor			: in pac_conductor_texts_board.cursor;
		--log_threshold 	: in type_log_level);

	
end et_conductor_text;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
