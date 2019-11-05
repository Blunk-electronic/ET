------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              FRAME_RW                                    --
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

with ada.strings.bounded; 		use ada.strings.bounded;
with ada.strings.maps;			use ada.strings.maps;
with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;

with et_string_processing;
with et_coordinates;			use et_coordinates;
with et_general;				use et_general;
with et_text;
with et_frames;					use et_frames;

package frame_rw is

	keyword_domain			: constant string := "domain";				
	keyword_paper_size		: constant string := "paper_size";
	keyword_orientation		: constant string := "orientation";
	keyword_border_width	: constant string := "border_width";
	keyword_size			: constant string := "size";
	keyword_x				: constant string := "x";
	keyword_y				: constant string := "y";	

	keyword_sectors			: constant string := "sectors";
	keyword_rows			: constant string := "rows";
	keyword_columns			: constant string := "columns";	

	
	section_title_block		: constant string := "[TITLE_BLOCK";
	
	type type_section is (
		SEC_INIT,
		SEC_TITLE_BLOCK
		);
	
	procedure create_frame (
	-- Creates and saves a frame in given file_name.
		file_name		: in pac_template_name.bounded_string;
		domain			: in type_domain;							   
		log_threshold	: in et_string_processing.type_log_level);
	
	procedure save_frame (
	-- Saves the given frame in file_name.
		frame			: in type_frame;
		file_name		: in pac_template_name.bounded_string;							 
		log_threshold	: in et_string_processing.type_log_level);

	function read_frame (
	-- Reads a frame from given file_name and returns a parameterized type_frame.
		file_name		: in pac_template_name.bounded_string;
		domain			: in type_domain;
		log_threshold	: in et_string_processing.type_log_level)
		return type_frame;
	
end frame_rw;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
