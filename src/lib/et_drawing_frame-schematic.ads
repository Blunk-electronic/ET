------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        DRAWING FRAME SCHEMATIC                           --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                -- 
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


with ada.directories;				use ada.directories;
with ada.containers.ordered_maps;

with et_sheets;						use et_sheets;



package et_drawing_frame.schematic is

	-- file extension:
	template_schematic_extension : constant string := "frs"; -- $ET_FRAMES/drawing_frame_version_1.frs


	-- default file names:
	template_schematic_default : constant pac_template_name.bounded_string := 
		pac_template_name.to_bounded_string (
			compose (
				name		=> template_file_name_dummy,
				extension	=> template_schematic_extension)
				);



	
	
-- TEXT PLACEHOLDERS AND TITLE BLOCK

	

	-- The set of basic placeholders is extended by other things which are
	-- required in the schematic:
	type type_placeholders_schematic is new type_placeholders_basic with record
		sheet_number		: type_placeholder;
		sheet_description	: type_placeholder;
		sheet_category		: type_placeholder; -- development, routing, product
	end record;
	
	type type_title_block_schematic is new type_title_block with record
		placeholders_additional : type_placeholders_schematic;
	end record;


	





	type type_frame_schematic is new type_frame_general with record
		title_block_schematic : type_title_block_schematic;
	end record;

	


	
	-- Applies defaults to given frame:
	procedure apply_defaults_schematic (frame : in out type_frame_schematic);


	function make_default_frame_schematic
		return type_frame_schematic;
	
	

	


-- THE FINAL FRAME:

	-- Prefixes before enumeration types prevent clashes with gnat keywords
	-- and package names:
	category_prefix : constant string := ("CAT_");
	-- CS apply prefix !
	
	type type_schematic_sheet_category is (
		DEVELOPMENT,
		ROUTING,
		PRODUCT
		);

	
	schematic_sheet_category_default : constant type_schematic_sheet_category := DEVELOPMENT;

	
	function to_string (cat : in type_schematic_sheet_category) return string;

	function to_category (cat : in string) return type_schematic_sheet_category;

	
	type type_schematic_description is record
		content		: pac_text_content.bounded_string := to_content ("no description");
		category	: type_schematic_sheet_category := schematic_sheet_category_default;
	end record;


	
	-- For each sheet of a schematic a description is required. 
	-- The descriptions are ordered by the sheet numbers:
	package pac_schematic_descriptions is new ordered_maps (
		key_type		=> type_sheet,
		element_type	=> type_schematic_description);


	
	-- The final drawing frames:
	type type_frames_schematic is record
		template		: pac_template_name.bounded_string := template_schematic_default;
			-- like $ET_FRAMES/drawing_frame_A4_landscape.frs

		--frame			: type_frame (DOMAIN_SCHEMATIC) := make_default_frame (DOMAIN_SCHEMATIC);
		frame			: type_frame_schematic := make_default_frame_schematic;
		
		descriptions	: pac_schematic_descriptions.map;
	end record;

	
	
end et_drawing_frame.schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
