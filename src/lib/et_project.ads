------------------------------------------------------------------------------
--                                                                          --
--                          SYSTEM ET PROJECT                               --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
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

--   The two letters "CS" indicate a "construction side" where things are not
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
with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_general;
with et_coordinates;			use et_coordinates;
with et_libraries;
with et_string_processing;
with et_schematic;

package et_project is

	-- after importing a foreign project, native ET projects are created here:
	directory_import		: constant string (1..9)	:= "et_import";

	-- native ET projects live in a parent folder
	directory_projects		: constant string (1..8)	:= "projects";

	-- native library directories
	directory_libraries 				: constant string (1..9) 	:= "libraries";	
	directory_libraries_devices			: constant string (1..7) 	:= "devices";
	directory_libraries_symbols			: constant string (1..7) 	:= "symbols";
	directory_libraries_packages		: constant string (1..8) 	:= "packages";	

	directory_libraries_schemlets		: constant string (1..9) 	:= "schemlets";
	
	-- supplementary stuff of a project
	directory_dru			: constant string (1..12)	:= "design_rules";
	directory_cam			: constant string (1..3)	:= "CAM";
	directory_net_classes	: constant string (1..11)	:= "net_classes";	
	directory_settings		: constant string (1..8)	:= "settings";
	directory_reports		: constant string (1..7)	:= "reports";
	directory_documentation	: constant string (1..13)	:= "documentation";
	directory_miscellaneous	: constant string (1..13)	:= "miscellaneous";		

	subtype type_file_name_text_size is type_distance range 1.0 .. 5.0; -- unit is mm
	file_name_text_size_default : constant type_file_name_text_size := 1.3;

	function to_file_name_text_size (size : in string) return type_file_name_text_size;

	
	-- This is the root directory where all projects live:
	projects_root_dir_length : constant natural := 100;
	package type_projects_root_dir is new generic_bounded_length (projects_root_dir_length);
	projects_root_dir : type_projects_root_dir.bounded_string;
	
	
	project_name_max : constant natural := 100;
	package type_project_name is new generic_bounded_length (project_name_max);
	project_name : type_project_name.bounded_string; -- the name of the current native project (or the rig)
	
	function to_string (project_name : in type_project_name.bounded_string) return string;

	function to_project_name (name : in string) return type_project_name.bounded_string;
	
	project_path_max : constant natural := 200;
	package type_et_project_path is new generic_bounded_length (project_path_max);

	function to_string (path : in type_et_project_path.bounded_string) return string;
	
	-- The current project file name is stored here:
	package type_project_file_name is new generic_bounded_length (project_path_max + project_name_max + 1); -- incl. directory separator
	project_file_name : type_project_file_name.bounded_string; -- et_projects/led_matrix


	
	project_file_name_extension : constant string (1..2) := "et";
	
	project_file_handle : ada.text_io.file_type;

    -- A sheet title may have 100 characters which seems sufficient for now.
 	sheet_title_length : constant natural := 100;    
	package type_sheet_title is new generic_bounded_length (sheet_title_length);

	subtype type_sheet_name_text_size is type_distance range 1.0 .. 5.0; -- unit is mm
	sheet_name_text_size_default : constant type_sheet_name_text_size := 1.3;

	function to_sheet_name_text_size (size : in string) return type_sheet_name_text_size;
	-- Converts a string to type_sheet_name_text_size.

	
	-- The current directory where libraries live is stored here:
-- 	package type_libraries_directory is new generic_bounded_length (project_path_max + directory_libraries_components'length + 1); -- incl. directory separator
-- 	component_libraries_directory_name : type_libraries_directory.bounded_string; -- ET_projects/lbr

	
-- 	procedure create_libraries_directory_components (
-- 	-- Creates a directory where component libraries will live.
-- 	-- An already existing directory will be overwritten.
-- 		project_path	: in type_et_project_path.bounded_string;
-- 		log_threshold	: in et_string_processing.type_log_level);

	
	procedure create_project_directory (
	-- Creates given project directory in the given project_path.
	-- Already existing projects in given project_path are overwritten.
		project_name	: in type_project_name.bounded_string;
		project_path	: in type_et_project_path.bounded_string;
		log_threshold	: in et_string_processing.type_log_level);

	procedure save_project (log_threshold : in et_string_processing.type_log_level);
	-- Saves the schematic and layout data in project file (project_file_handle).

	procedure open_project (log_threshold : in et_string_processing.type_log_level);
	-- Opens and reads the schematic and layout data present in project file (project_file_handle).
	

-- 	procedure write_component_libraries (log_threshold : in et_string_processing.type_log_level);
-- 	-- Writes the ET native component libraries in libraries_directory_name.

	-- PROJECT FILE SECTIONS AND KEYWORDS
	section_module				: constant string := "[MODULE";
	
	section_begin				: constant string := "BEGIN]";	
	section_end					: constant string := "END]";
	
	keyword_generic_name			: constant string := "generic_name";
	keyword_instance_name			: constant string := "instance";
	keyword_name					: constant string := "name";
	keyword_description				: constant string := "description";
	keyword_clearance				: constant string := "clearance";		
	keyword_track_width_min			: constant string := "track_width_min";
	keyword_via_drill_min			: constant string := "via_drill_min";
	keyword_via_restring_min		: constant string := "via_restring_min";	
	keyword_micro_via_drill_min		: constant string := "micro_via_drill_min";
	keyword_micro_via_restring_min	: constant string := "micro_via_restring_min";	
	keyword_class					: constant string := "class";
	keyword_scope					: constant string := "scope";	
	keyword_position				: constant string := "position";
	keyword_start					: constant string := "start";
	keyword_end						: constant string := "end";
	keyword_rotation				: constant string := "rotation";
	keyword_size					: constant string := "size";
	keyword_style					: constant string := "style";
	keyword_line_width				: constant string := "line_width";
	keyword_appearance				: constant string := "appearance";
	keyword_direction				: constant string := "direction";			
	keyword_junction				: constant string := "junction";
	keyword_pos_x					: constant string := "x";
	keyword_pos_y					: constant string := "y";		
	keyword_device					: constant string := "device";
	keyword_port					: constant string := "port";
	keyword_module					: constant string := "module";
	keyword_layer					: constant string := "layer";
	keyword_width					: constant string := "width";
	keyword_center					: constant string := "center";
	keyword_diameter				: constant string := "diameter";
	keyword_restring_outer_layers	: constant string := "restring_outer_layers";
	keyword_restring_inner_layers	: constant string := "restring_inner_layers";		
	keyword_layer_start				: constant string := "layer_start";
	keyword_layer_end				: constant string := "layer_end";		
	keyword_priority				: constant string := "priority";
	keyword_isolation				: constant string := "isolation";
	keyword_corner_easing			: constant string := "corner_easing";
	keyword_easing_radius			: constant string := "easing_radius";	
	keyword_fill_style				: constant string := "fill_style";	
	keyword_hatching_line_width		: constant string := "hatching_line_width";
	keyword_hatching_line_spacing	: constant string := "hatching_line_spacing";		
	keyword_pad_connection			: constant string := "pad_connection";
	keyword_min_width				: constant string := "min_width";
	keyword_pad_technology			: constant string := "pad_technology";		
	keyword_thermal_width			: constant string := "thermal_width";		
	keyword_thermal_gap				: constant string := "thermal_gap";
	keyword_template				: constant string := "template";
	keyword_model					: constant string := "model";				
	keyword_value					: constant string := "value";
	keyword_bom						: constant string := "bom";		
	keyword_partcode				: constant string := "partcode";	
	keyword_purpose					: constant string := "purpose";	
	keyword_variant					: constant string := "variant";
	keyword_mirrored				: constant string := "mirrored";
	keyword_face					: constant string := "face";
	keyword_meaning					: constant string := "meaning";
	keyword_alignment				: constant string := "alignment";
	keyword_horizontal				: constant string := "horizontal";
	keyword_vertical				: constant string := "vertical";		
	keyword_hidden					: constant string := "hidden";		
	keyword_sheet					: constant string := "sheet";
	keyword_path					: constant string := "path";	
	keyword_position_in_board		: constant string := "position_in_board";
	keyword_view_mode				: constant string := "view_mode";
	keyword_reference_offset		: constant string := "reference_offset";	
	keyword_content					: constant string := "content";
	keyword_radius					: constant string := "radius";	
	
	section_net_classes			: constant string := "[NET_CLASSES";
	section_net_class			: constant string := "[NET_CLASS";

	section_nets				: constant string := "[NETS";
	section_net					: constant string := "[NET";

	section_strands				: constant string := "[STRANDS";
	section_strand				: constant string := "[STRAND";

	section_segments			: constant string := "[SEGMENTS";
	section_segment				: constant string := "[SEGMENT";

	section_junctions			: constant string := "[JUNCTIONS";
	
	section_labels				: constant string := "[LABELS";
	section_label				: constant string := "[LABEL";

	section_submodule_ports		: constant string := "[SUBMODULE_PORTS";
	section_port				: constant string := "[PORT";
	
	section_route				: constant string := "[ROUTE";

	section_lines				: constant string := "[LINES";
	section_line				: constant string := "[LINE";

	section_arc					: constant string := "[ARC";
	section_circle				: constant string := "[CIRCLE";
	
	section_polygon				: constant string := "[POLYGON";
	section_corners				: constant string := "[CORNERS";
	
	section_via					: constant string := "[VIA";
	
	section_submodules			: constant string := "[SUBMODULES";

	section_submodule			: constant string := "[SUBMODULE";

	section_drawing_frames		: constant string := "[DRAWING_FRAMES";

	section_schematic			: constant string	:= "[SCHEMATIC";

	section_board				: constant string	:= "[BOARD";

	section_devices				: constant string	:= "[DEVICES";
	section_device				: constant string	:= "[DEVICE";

	section_units				: constant string	:= "[UNITS";
	section_unit				: constant string	:= "[UNIT";

	section_texts				: constant string	:= "[TEXTS";
	section_text				: constant string	:= "[TEXT";

	section_placeholders		: constant string	:= "[PLACEHOLDERS";	
	section_placeholder			: constant string	:= "[PLACEHOLDER";
	section_package				: constant string	:= "[PACKAGE";

	section_silk_screen			: constant string	:= "[SILK_SCREEN";
	section_assembly_doc		: constant string	:= "[ASSEMBLY_DOCUMENTATION";
	section_stencil				: constant string	:= "[STENCIL";
	section_stop_mask			: constant string	:= "[STOP_MASK";
	section_keepout				: constant string	:= "[KEEPOUT";
	section_route_restrict		: constant string	:= "[ROUTE_RESTRICT";
	section_via_restrict		: constant string	:= "[VIA_RESTRICT";
	section_copper				: constant string	:= "[COPPER";
	section_pcb_contours		: constant string	:= "[PCB_CONTOURS_NON_PLATED";
	section_pcb_contours_plated	: constant string	:= "[PCB_CONTOURS_PLATED";
	section_pac_3d_contours		: constant string	:= "[PACKAGE_3D_CONTOURS";
	
	section_top					: constant string	:= "[TOP";
	section_bottom				: constant string	:= "[BOTTOM";

	section_variants			: constant string	:= "[VARIANTS";
	section_variant				: constant string	:= "[VARIANT";

	section_terminal_port_map	: constant string	:= "[TERMINAL_PORT_MAP";
	section_units_internal		: constant string	:= "[UNITS_INTERNAL";
	section_units_external		: constant string	:= "[UNITS_EXTERNAL";
	
	section_symbol				: constant string	:= "[SYMBOL";
	section_shapes				: constant string	:= "[SHAPES";
	section_polyline			: constant string	:= "[POLYLINE";
	section_rectangle			: constant string	:= "[RECTANGLE";

	section_ports				: constant string	:= "[PORTS";
	section_port_begin			: constant string	:= "[PORT";

	section_terminals			: constant string	:= "[TERMINALS";
	section_terminal			: constant string	:= "[TERMINAL";
	
	section_title_block			: constant string	:= "[TITLE_BLOCK";

	type type_section_name_project is (
		SEC_MODULE,
		SEC_NET_CLASSES,
		SEC_NET_CLASS,
		SEC_NETS,
		SEC_NET,
		SEC_STRANDS,
		SEC_STRAND,
		SEC_SEGMENTS,
		SEC_SEGMENT,
		SEC_LABELS,
		SEC_LABEL,
		SEC_SUBMODULE_PORTS,
		SEC_ROUTE,
		SEC_LINE,
		SEC_ARC,
		SEC_POLYGON,
		SEC_VIA,
		SEC_SUBMODULES,
		SEC_SUBMODULE,
		SEC_FRAMES,
		SEC_SCHEMATIC,
		SEC_BOARD,
		SEC_DEVICES,
		SEC_DEVICE,
		SEC_UNITS,
		SEC_UNIT,
		SEC_PACKAGE,
		SEC_PLACEHOLDER,
		SEC_SILK_SCREEN,
		SEC_ASSEMBLY_DOCUMENTATION,
		SEC_KEEPOUT,
		SEC_ROUTE_RESTRICT,
		SEC_VIA_RESTRICT,
		SEC_STOP_MASK,
		SEC_STENCIL,
		SEC_COPPER,
		SEC_PCB_CONTOURS_NON_PLATED,
		-- CS SEC_PCB_CONTOURS_PLATED
		SEC_TOP,
		SEC_BOTTOM
		);

	type type_section_name_device is (
		SEC_VARIANTS,
		SEC_VARIANT,
		SEC_TERMINAL_PORT_MAP,
		SEC_UNITS_INTERNAL,
		SEC_UNIT,
		SEC_SYMBOL,
		SEC_SHAPES,
		SEC_LINE,
		SEC_ARC,
		SEC_CIRCLE,
		SEC_POLYLINE,
		SEC_RECTANGLE,
		SEC_TEXTS,
		SEC_TEXT,
		SEC_PLACEHOLDERS,
		SEC_PORTS,
		SEC_PORT,
		SEC_UNITS_EXTERNAL
		);

	
end et_project;

-- Soli Deo Gloria
