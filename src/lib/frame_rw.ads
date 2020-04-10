------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              FRAME_RW                                    --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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
with et_coordinates;
with et_general;				use et_general;
with et_text;
with et_frames;					use et_frames;

package frame_rw is

	keyword_domain			: constant string := "domain";				
	keyword_paper_size		: constant string := "paper_size";
	keyword_orientation		: constant string := "orientation";
	keyword_border_width	: constant string := "border_width";
	keyword_size			: constant string := "size";

	keyword_sectors			: constant string := "sectors";
	keyword_rows			: constant string := "rows";
	keyword_columns			: constant string := "columns";	


	
	-- general sections:
	section_title_block		: constant string := "[TITLE_BLOCK";
	section_lines			: constant string := "[LINES";

	section_placeholders			: constant string := "[PLACEHOLDERS";

	section_project_name			: constant string := "[PROJECT_NAME";
	section_module_file_name		: constant string := "[MODULE_FILE_NAME";		
	section_active_assembly_variant	: constant string := "[ACTIVE_ASSEMBLY_VARIANT";

	section_company					: constant string := "[COMPANY";
	section_customer				: constant string := "[CUSTOMER";
	section_partcode				: constant string := "[PARTCODE";
	section_drawing_number			: constant string := "[DRAWING_NUMBER";
	section_revision				: constant string := "[REVISION";

	section_drawn_by				: constant string := "[DRAWN_BY";
	section_checked_by				: constant string := "[CHECKED_BY";
	section_approved_by				: constant string := "[APPROVED_BY";

	section_drawn_date				: constant string := "[DRAWN_DATE";
	section_checked_date			: constant string := "[CHECKED_DATE";
	section_approved_date			: constant string := "[APPROVED_DATE";

	
	-- schematic related sections:	
	section_sheet_number			: constant string := "[SHEET_NUMBER";
	section_sheet_description		: constant string := "[SHEET_DESCRIPTION";
	section_sheet_category			: constant string := "[SHEET_CATEGORY";
	
	
	-- pcb related sections:
	section_cam_markers		: constant string := "[CAM_MARKERS";
	section_face			: constant string := "[FACE";
	section_silk_screen		: constant string := "[SILK_SCREEN";
	section_assy_doc		: constant string := "[ASSY_DOC";
	section_keepout			: constant string := "[KEEPOUT";
	section_plated_millings	: constant string := "[PLATED_MILLINGS";
	section_pcb_outline		: constant string := "[PCB_OUTLINE";
	section_route_restrict	: constant string := "[ROUTE_RESTRICT";
	section_via_restrict	: constant string := "[VIA_RESTRICT";
	section_signal_layer	: constant string := "[SIGNAL_LAYER";


	
	type type_section is (
		SEC_ACTIVE_ASSEMBLY_VARIANT,
		SEC_ASSY_DOC,
		SEC_APPROVED_BY,
		SEC_APPROVED_DATE,
		SEC_CAM_MARKERS,
		SEC_CHECKED_BY,
		SEC_CHECKED_DATE,
		SEC_COMPANY,
		SEC_CUSTOMER,
		SEC_DRAWING_NUMBER,
		SEC_DRAWN_BY,
		SEC_DRAWN_DATE,
		SEC_FACE,
		SEC_INIT,
		SEC_KEEPOUT,
		SEC_LINE,
		SEC_LINES,
		SEC_MODULE_FILE_NAME,
		SEC_PARTCODE,
		SEC_PCB_OUTLINE,
		SEC_PLACEHOLDERS,
		SEC_PLATED_MILLINGS,
		SEC_PROJECT_NAME,
		SEC_REVISION,
		SEC_ROUTE_RESTRICT,
		SEC_SHEET_CATEGORY,
		SEC_SHEET_DESCRIPTION,
		SEC_SHEET_NUMBER,
		SEC_SIGNAL_LAYER,
		SEC_SILK_SCREEN,
		SEC_TEXT,
		SEC_TEXTS,
		SEC_TITLE_BLOCK,
		SEC_VIA_RESTRICT
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
