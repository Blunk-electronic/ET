------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        COMPONENT LIBRARIES                               --
--                                                                          --
--                             S p e c                                      --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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
with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;
--with ada.containers.vectors;

with et_coordinates;			use et_coordinates;
with et_geometry;
with et_string_processing;
with et_general;
with et_text;
-- with et_symbols;				use et_symbols;
-- with et_packages;				use et_packages;
with et_devices;				use et_devices;

package et_libraries is

	path_length_max : constant natural := 500; -- CS: increase if necessary


	
	
	-- The name of the person who has drawn, checked or approved something may have 100 characters which seems sufficient for now.
 	person_name_length : constant natural := 100;
	package type_person_name is new generic_bounded_length (person_name_length);

	function to_string (person : in type_person_name.bounded_string) return string;
	-- Returns the given person name as string.








	
-- DRAWING FRAME

	-- $ET_FRAMES/drawing_frame_version_1.frm
	frame_template_name_length_max : constant positive := 300;
	package type_frame_template_name is new generic_bounded_length (frame_template_name_length_max);

	frame_template_name_dummy : constant type_frame_template_name.bounded_string := 
		type_frame_template_name.to_bounded_string ("dummy_frame");
	
	function to_string (name : in type_frame_template_name.bounded_string) return string;
	function to_template_name (name : in string) return type_frame_template_name.bounded_string;
	
    type type_title_block_line is null record; -- CS
-- 		coordinates_start : type_point;
-- 		coordinates_end   : type_point;
--     end record;

	package type_title_block_lines is new doubly_linked_lists (type_title_block_line);
    
 	title_block_text_length_max : constant natural := 200;
	package type_title_block_text_content is new generic_bounded_length (title_block_text_length_max);

	type type_title_block_text_meaning is ( 
		PROJECT, TITLE, 
        DRAWN_BY, CHECKED_BY, APPROVED_BY, 
        DRAWN_DATE, CHECKED_DATE, APPROVED_DATE,
        COMPANY,
		REVISION, MISC);
	
	type type_title_block_text is record
		meaning			: type_title_block_text_meaning;
-- CS 		coordinates		: type_point; 
		text			: type_title_block_text_content.bounded_string; -- CS: rename to content
 		size			: natural; -- CS pac_text.type_text_size;
 		rotation		: et_coordinates.type_rotation;
		-- CS: font, ...
 	end record;

	package type_title_block_texts is new doubly_linked_lists (type_title_block_text);

    -- the final title block
    type type_title_block is record
--  CS       coordinates     : type_point; -- CS rename to position
        lines           : type_title_block_lines.list;
        texts           : type_title_block_texts.list;
    end record;

    -- A drawing frame consists of straight lines and texts.
    -- The text is a character at the x/y border that helps to locate objects.
    type type_frame_line is null record; -- CS
-- 		coordinates_start : type_point;
--         coordinates_end   : type_point;
-- 	end record;
	
	package type_frame_lines is new doubly_linked_lists (type_frame_line);

	type type_frame_text is record
-- CS		coordinates		: type_point; -- CS rename to position
		text			: character_set := et_string_processing.general_characters; -- CS rename to content
		size			: natural; -- CS pac_text.type_text_size;
		rotation		: et_coordinates.type_rotation;
		-- CS: font, ...
	end record;
	
	package type_frame_texts is new doubly_linked_lists (type_frame_text);

	-- the final drawing frame
	-- NOTE: The native drawing frame has its lower left corner at position x/y 0/0. always.
    type type_frame is tagged record
        paper_size      : et_general.type_paper_size; -- the size of the paper
        size_x, size_y  : et_coordinates.type_distance; -- the dimensions of the frame (should fit into paper_size) 
        lines           : type_frame_lines.list;
		texts           : type_frame_texts.list;
		title_block		: type_title_block;
    end record;

--     -- There are lots of drawing frames in a schematic. We store them in a vector:
-- 	package type_frames is new vectors (
-- 		index_type		=> et_coordinates.type_submodule_sheet_number,
-- 		element_type	=> type_frame);

	
		
end et_libraries;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
