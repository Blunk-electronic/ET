------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               FRAMES                                     --
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
with ada.directories;			use ada.directories;

with et_coordinates;			use et_coordinates;
with et_general;				use et_general;
with et_text;

with glib;
with cairo;

package et_frames is

-- PAPER SIZES
    type type_paper_size is (A3, A4); -- CS: others ?
    paper_size_default : constant type_paper_size := A4;

	function to_paper_size (paper_size : in string) return type_paper_size;
	function to_string (paper_size : in type_paper_size) return string;

	type type_orientation is (PORTRAIT, LANDSCAPE);
	orientation_default : constant type_orientation := LANDSCAPE;

	function to_string (orientation : in type_orientation) return string;
	function to_orientation (orientation : in string) return type_orientation;

	
	template_file_name_length_max : constant positive := 300;
	template_file_name_dummy : constant string := "dummy_frame";

	line_width_thin : constant glib.gdouble := 0.3;
	line_width_thick : constant glib.gdouble := 1.0;
	
	-- A drawing frame is divided in columns and rows. The columns run from 1 to maximal 26.
	-- The rows run from A to Z.
	type type_rows is new positive range 1..26;
	rows_default : constant type_rows := 7;

	function to_string (rows : in type_rows) return string;
	function to_rows (rows : in string) return type_rows;
	
	type type_columns is new positive range 1..26;
	columns_default : constant type_columns := 10;

	function to_string (columns : in type_columns) return string;
	function to_columns (columns : in string) return type_columns;
	
	row_characters : constant character_set := to_set (span => ('A','Z')); -- CS currently not used
	-- CS row numbers must be mapped to row characters 
	
	type type_sectors is record
		rows	: type_rows := rows_default; 
		columns	: type_columns := columns_default;
	end record;

	-- The unit for all kinds of distances in a drawing frame is millimeters.
	type type_distance is new natural range 0 .. 10000;

	function to_string (distance : in type_distance) return string;
	function to_distance (distance : in string) return type_distance;
	
	type type_size is record -- CS rename to type_frame_size
		x	: type_distance := 280;
		y	: type_distance := 200;
	end record;

	subtype type_border_width is type_distance range 4 .. 10;
	border_width_default : constant type_border_width := 7;

	paper_size_A3_x : constant type_distance := 420;
	paper_size_A3_y : constant type_distance := 297;
	
	paper_size_A4_x : constant type_distance := 297;
	paper_size_A4_y : constant type_distance := 210;

	function paper_dimension (
	-- Returns for the given paper size, orientation and axis the corresponding size in mm.
		paper_size	: in type_paper_size;
		orientation	: in type_orientation := LANDSCAPE;
		axis		: in type_axis_2d)
		return type_distance;

	
	type type_position is record
		x, y : type_distance := border_width_default;
	end record;

	position_default : constant type_position := (1,1);


	
	type type_line is record
		start_point	: type_position := position_default;
		end_point	: type_position := position_default;
		-- CS line width ?
	end record;

	package pac_lines is new doubly_linked_lists (type_line);



	
	subtype type_text_size is type_distance range 1 .. 50;
	text_size_default : constant type_text_size := 3;


	
	type type_placeholder is tagged record
		size			: type_text_size := text_size_default;
		position		: type_position := position_default;
	end record;

	-- These placeholders are common in both schematic and pcb title blocks:
	type type_placeholders_common is record
		project_name			: type_placeholder; -- name of project directory
		module_file_name		: type_placeholder; -- name of module file
		active_assembly_variant	: type_placeholder; -- the active assembly variant
	end record;

	-- Basic placeholders are separately available for schematic and pcb.
	-- For example the revision in schematic is not necessarily the same as in the layout.
	-- Another example: The person who has drawn the schematic is not necessarily the
	-- same as the one who did the layout.
	type type_placeholders_basic is tagged record
		company			: type_placeholder;
		customer		: type_placeholder;
		partcode		: type_placeholder;
		drawing_number	: type_placeholder;
		revision		: type_placeholder;
		
		drawn_by		: type_placeholder;
		checked_by		: type_placeholder;
		approved_by		: type_placeholder;

		drawn_date		: type_placeholder;
		checked_date	: type_placeholder;
		approved_date	: type_placeholder;

-- 		created_date	: type_placeholder;
-- 		edited_date		: type_placeholder;
	end record;


	
	type type_text is new type_placeholder with record
		content			: et_text.type_text_content.bounded_string;
	end record;

	package pac_texts is new doubly_linked_lists (type_text);


	-- The basic title block:
	type type_title_block is tagged record
		position		: type_position := position_default;

		-- The positions of title block objects are relative
		-- to the position of the title block:
		lines			: pac_lines.list;
		placeholders	: type_placeholders_common;
		texts			: pac_texts.list;
	end record;

	
	-- GUI relevant only: The font of placeholders:
	font_placeholders : constant et_text.type_font := (
		family	=> et_text.to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_NORMAL,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);	

	-- GUI relevant only: The font of other texts:
	font_texts : constant et_text.type_font := (
		family	=> et_text.to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_NORMAL,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);	

	-- GUI relevant only: The font of column and row indexes:
	font_indexes : constant et_text.type_font := (
		family	=> et_text.to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_NORMAL,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);	

	-- GUI relevant only: The font size of column and row indexes:
	font_indexes_size : constant type_distance := 3;
	


-- FILE NAMES

	-- extensions:
	template_schematic_extension	: constant string := "frs"; -- $ET_FRAMES/drawing_frame_version_1.frs
	template_pcb_extension			: constant string := "frb"; -- $ET_FRAMES/drawing_frame_version_1.frb

	package pac_template_name is new generic_bounded_length (template_file_name_length_max);

	-- default file names:
	template_schematic_default : constant pac_template_name.bounded_string := 
		pac_template_name.to_bounded_string (
			compose (
				name		=> template_file_name_dummy,
				extension	=> template_schematic_extension)
				);

	template_pcb_default : constant pac_template_name.bounded_string := 
		pac_template_name.to_bounded_string (
			compose (
				name		=> template_file_name_dummy,
				extension	=> template_pcb_extension)
				);
	
	function to_string (name : in pac_template_name.bounded_string) return string;
	function to_template_name (name : in string) return pac_template_name.bounded_string;


	
-- TEXT PLACEHOLDERS AND TITLE BLOCKS
	
	-- schematic:
	-- The set of basic placeholders is extended by other things which are
	-- required in the schematic:
	type type_placeholders_schematic is new type_placeholders_basic with record
		sheet_number	: type_placeholder;
		description		: type_placeholder;
		category		: type_placeholder; -- development, routing, product
	end record;
	
	type type_title_block_schematic is new type_title_block with record
		additional_placeholders : type_placeholders_schematic;
	end record;

	-- pcb:
	
	-- CAM markers are required for CAM output and visualization.
	-- They are texts in the title block that indicate what it is about.
	-- Depending on the structures being displayed or exported, they are displayed or not.
	-- Their content may be specified by the operator in the respective sections in the frame file (*.frb).
	-- The content specified in the frame file overrides the default content. 
	-- If they are not specified by the frame file, default position, size and content is used (see below).
	type type_cam_marker is new type_text with null record;

	type type_cam_markers is record
		face			: type_cam_marker := (content => et_text.to_content ("FACE:"), others => <>);
		silk_screen		: type_cam_marker := (content => et_text.to_content ("SILKSCREEN"), others => <>);
		assy_doc		: type_cam_marker := (content => et_text.to_content ("ASSEMBLY"), others => <>);
		keepout			: type_cam_marker := (content => et_text.to_content ("KEEPOUT"), others => <>);
		plated_millings	: type_cam_marker := (content => et_text.to_content ("PLTD_MILLGS"), others => <>); 
		pcb_outline 	: type_cam_marker := (content => et_text.to_content ("OUTLINE"), others => <>);
		route_restrict	: type_cam_marker := (content => et_text.to_content ("ROUTE_RESTR:"), others => <>);
		via_restrict	: type_cam_marker := (content => et_text.to_content ("VIA_RESTR:"), others => <>);		
		signal_layer	: type_cam_marker := (content => et_text.to_content ("SGNL_LYR:"), others => <>);
		stencil			: type_cam_marker := (content => et_text.to_content ("STENCIL"), others => <>);		
		stop_mask		: type_cam_marker := (content => et_text.to_content ("STOP_MASK"), others => <>);
	end record;
	
	type type_placeholders_pcb is new type_placeholders_basic with record
		face			: type_placeholder; -- to be filled with the word "TOP" or "BOTTOM"
		signal_layer	: type_placeholder; -- to be filled with the signal layer id like 1,2,3, 8..16
	end record;
	
	type type_title_block_pcb is new type_title_block with record
		additional_placeholders	: type_placeholders_pcb;
		cam_markers				: type_cam_markers;
	end record;



	
-- THE GENERAL PARAMETERIZED FRAME

	-- The frame may be used in a schematic drawing or a layout drawing:
	type type_domain is (SCHEMATIC, PCB);

	function to_string (domain : in type_domain) return string;
	function to_domain (domain : in string) return type_domain;

	-- The used title block depends on the domain.
	type type_frame (domain : type_domain) is record
		paper			: type_paper_size := paper_size_default;
		orientation		: type_orientation := orientation_default;
		border_width	: type_border_width := border_width_default;
		size			: type_size;
		sectors			: type_sectors;
	
		case domain is
			when SCHEMATIC =>
				title_block_schematic : type_title_block_schematic;

			when PCB =>
				title_block_pcb : type_title_block_pcb;
		end case;
	end record;


	-- Applies defaults to given frame:
	procedure apply_defaults (frame : in out type_frame);

	
-- THE FINAL FRAME IN A PCB DRAWING
	
	-- This is the drawing frame used in a pcb layout:
	type type_frame_pcb is record
		template	: pac_template_name.bounded_string := template_pcb_default;
			-- like $ET_FRAMES/drawing_frame_A3_landscape.frm

		frame		: type_frame (PCB);
	end record;

	


-- THE FINAL FRAME IN A SCHEMATIC
	
	type type_schematic_sheet_category is (
		DEVELOPMENT,
		ROUTING,
		PRODUCT
		);

	schematic_sheet_category_default : constant type_schematic_sheet_category := DEVELOPMENT;

	function to_string (cat : in type_schematic_sheet_category) return string;
	function to_category (cat : in string) return type_schematic_sheet_category;
	
	type type_schematic_description is record
		content		: et_text.type_text_content.bounded_string := et_text.to_content ("no description");
		category	: type_schematic_sheet_category := schematic_sheet_category_default;
	end record;
		
	-- For each sheet of a schematic a description is required. 
	-- The descriptions are ordered by the sheet numbers:
	package pac_schematic_descriptions is new ordered_maps (
		key_type		=> et_coordinates.type_sheet,
		element_type	=> type_schematic_description);


	-- The final drawing frames:
	type type_frames_schematic is record
		template		: pac_template_name.bounded_string := template_schematic_default;
			-- like $ET_FRAMES/drawing_frame_A4_landscape.frs

		frame			: type_frame (SCHEMATIC);
		
		descriptions	: pac_schematic_descriptions.map;
	end record;
	
end et_frames;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
