------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           SCHEMATIC SHEETS                               --
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

with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;

with et_geometry;
with et_coordinates;		use et_coordinates;
with et_symbols;
with et_text;				use et_text;
with et_frames;				use et_frames;

package et_schematic_sheets is

	-- $ET_FRAMES/drawing_frame_version_1.frs
	template_file_extension : constant string := "frs";
	
	package pac_template_name is new generic_bounded_length (template_file_name_length_max);

	frame_template_name_dummy : constant pac_template_name.bounded_string := 
		pac_template_name.to_bounded_string (template_file_name_dummy);
	
	function to_string (name : in pac_template_name.bounded_string) return string;
	function to_template_name (name : in string) return pac_template_name.bounded_string;

	
	use et_coordinates.geometry;
	use type_text_content;

	package pac_shapes is new et_geometry.shapes_2d (
		geometry	=> et_coordinates.geometry);
	
	package pac_frames is new et_frames.frames (
		shapes	=> pac_shapes,
		text	=> et_symbols.pac_text);
	
	

-- FRAMES
	use pac_frames;

	-- The title block of a schematic sheet has placeholders:
	type type_text_placeholders is record
		sheet_number	: type_text_placeholder;
		description		: type_text_placeholder;
		category		: type_text_placeholder; -- development, routing, product
	end record;
	
	type type_title_block is new pac_frames.type_title_block with record
		additional_placeholders : type_text_placeholders;		
	end record;

	-- This is the drawing frame used in a schematic:
	type type_frame is new pac_frames.type_frame with record
		title_block : type_title_block;
	end record;

	type type_sheet_category is (
		DEVELOPMENT,
		ROUTING,
		PRODUCT
		);

	sheet_category_default : constant type_sheet_category := DEVELOPMENT;
		
	type type_description is record
		content			: et_text.type_text_content.bounded_string;
		sheet_category	: type_sheet_category := sheet_category_default;
	end record;
		
	-- For each sheet a description is required. The descriptions 
	-- are ordered by the sheet numbers:
	package pac_descriptions is new ordered_maps (
		key_type		=> et_coordinates.type_sheet,
		element_type	=> type_description);


	-- The final drawing frames:
	type type_frames is record
		template		: pac_template_name.bounded_string := frame_template_name_dummy;
			-- like $ET_FRAMES/drawing_frame_A4_landscape.frs

		frame			: type_frame; -- lines, title block
		
		descriptions	: pac_descriptions.map;
	end record;

		
end et_schematic_sheets;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
