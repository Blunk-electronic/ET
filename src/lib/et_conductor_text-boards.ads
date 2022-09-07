------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        CONDUCTOR TEXT BOARDS                             --
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



package et_conductor_text.boards is

	--use pac_geometry_brd;
	--use pac_polygons;
	
	--use pac_geometry_2;

	
	--package pac_conductor_line_segments is new
		--doubly_linked_lists (type_conductor_line_segment);

		
	-- Iterates the segments. Aborts the process when the proceed-flag goes false:
	--procedure iterate (
		--segments	: in pac_conductor_line_segments.list;
		--process		: not null access procedure (position : in pac_conductor_line_segments.cursor);
		--proceed		: not null access boolean);

	
	--function make_segments (
		--v_text	: in type_vector_text;
		--width	: in type_distance_positive)
		--return pac_conductor_line_segments.list;
	
	
	--type type_conductor_text 
		--is new type_text_fab_with_content with
	--record
		--layer	: type_signal_layer := type_signal_layer'first;
		--vectors	: type_vector_text;
		----segments: pac_conductor_line_segments.list;
	--end record;


	type type_conductor_text 
		is new et_conductor_text.type_conductor_text with
	record
		layer : type_signal_layer := type_signal_layer'first;
	end record;
	
	
	package pac_conductor_texts is new doubly_linked_lists (type_conductor_text);
	use pac_conductor_texts;

	
	-- Iterates the texts. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		texts	: in pac_conductor_texts.list;
		process	: not null access procedure (position : in pac_conductor_texts.cursor);
		proceed	: not null access boolean);


	
	-- Logs the properties of the given text.
	procedure text_conductor_properties (
		cursor			: in pac_conductor_texts.cursor;
		log_threshold 	: in type_log_level);

	
end et_conductor_text.boards;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
