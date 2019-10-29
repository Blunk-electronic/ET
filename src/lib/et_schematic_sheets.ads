------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         SCHEMATIC SHEEETS                                --
--                                                                          --
--                               S p e c                                    --
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
--   ToDo: 

with ada.text_io;				use ada.text_io;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

-- with et_general;				use et_general;

with et_geometry;
with et_coordinates;		use et_coordinates;
with et_symbols;
with et_text;				use et_text;
with et_frames;				use et_frames;

package et_schematic_sheets is

	use et_coordinates.geometry;
	use type_text_content;

	package pac_shapes is new et_geometry.shapes_2d (
		geometry	=> et_coordinates.geometry);
	
	package pac_frames is new et_frames.frames (
		shapes	=> pac_shapes,
		text	=> et_symbols.pac_text);
	
	
	procedure dummy;

-- FRAMES
	use pac_frames;

	-- The title block of a schematic sheet requires a placeholder for the 
	-- description of the sheet.
	type type_title_block is new pac_frames.type_title_block with record
		description	: type_text_placeholder;
	end record;

	-- This is the drawing frame used in a schematic:
	type type_frame is new pac_frames.type_frame with record
		title_block : type_title_block;
	end record;


	
-- DESCRIPTIONS INDIVIDUAL SHEETS
	package pac_descriptions is new ordered_maps (
		key_type		=> et_coordinates.type_sheet,
		element_type	=> et_text.type_text_content.bounded_string);


		
end et_schematic_sheets;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
