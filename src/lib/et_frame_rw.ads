------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                     DRAWING FRAME READ AND WRITE                         --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2023                                                -- 
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

with ada.strings.bounded; 		use ada.strings.bounded;
with ada.strings.maps;			use ada.strings.maps;
with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;

with et_string_processing;
with et_logging;				use et_logging;
with et_coordinates_2;
with et_text;
with et_frames;					use et_frames;


package et_frame_rw is
	
-- GENERAL SECTIONS:
	
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
	section_stencil			: constant string := "[STENCIL";
	section_stop_mask		: constant string := "[STOP_MASK";

	
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
		SEC_STENCIL,
		SEC_STOP_MASK,
		SEC_TEXT,
		SEC_TEXTS,
		SEC_TITLE_BLOCK,
		SEC_VIA_RESTRICT
		);

	
	-- Creates and saves a frame in given file_name.
	procedure create_frame (
		file_name		: in pac_template_name.bounded_string;
		domain			: in et_frames.type_domain;							   
		log_threshold	: in type_log_level);

	
	-- Saves the given frame in file_name.
	procedure save_frame (
		frame			: in type_frame;
		file_name		: in pac_template_name.bounded_string;							 
		log_threshold	: in type_log_level);

	
	-- Reads a frame from given file_name and returns a parameterized type_frame.
	function read_frame (
		file_name		: in pac_template_name.bounded_string;
		domain			: in et_frames.type_domain;
		log_threshold	: in type_log_level)
		return type_frame;



	function to_position (
		line : in et_string_processing.type_fields_of_line; -- position x -100 y -150
		from : in et_string_processing.type_field_count_positive)
		return type_position;

	
end et_frame_rw;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
