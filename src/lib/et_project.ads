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

	-- PROJECT FILE SECTIONS
	section_module_begin			: constant string (1 .. 14) := "[MODULE BEGIN]";
	section_module_end				: constant string (1 .. 12) := "[MODULE END]";	

	section_net_classes_begin		: constant string (1 .. 19) := "[NET_CLASSES BEGIN]";
	section_net_classes_end			: constant string (1 .. 17) := "[NET_CLASSES END]";	

	section_net_class_begin			: constant string (1 .. 17) := "[NET_CLASS BEGIN]";
	section_net_class_end			: constant string (1 .. 15) := "[NET_CLASS END]";	

	section_nets_begin				: constant string (1 .. 12) := "[NETS BEGIN]";
	section_nets_end				: constant string (1 .. 10) := "[NETS END]";	

	section_net_begin				: constant string (1 .. 11) := "[NET BEGIN]";
	section_net_end					: constant string (1 ..  9) := "[NET END]";	

	section_strands_begin			: constant string (1 .. 15) := "[STRANDS BEGIN]";
	section_strands_end				: constant string (1 .. 13) := "[STRANDS END]";

	section_strand_begin			: constant string (1 .. 14) := "[STRAND BEGIN]";
	section_strand_end				: constant string (1 .. 12) := "[STRAND END]";

	section_segments_begin			: constant string (1 .. 16) := "[SEGMENTS BEGIN]";
	section_segments_end			: constant string (1 .. 14) := "[SEGMENTS END]";	

	section_segment_begin			: constant string (1 .. 15) := "[SEGMENT BEGIN]";
	section_segment_end				: constant string (1 .. 13) := "[SEGMENT END]";	

	section_labels_begin			: constant string (1 .. 14) := "[LABELS BEGIN]";
	section_labels_end				: constant string (1 .. 12) := "[LABELS END]";

	section_label_begin				: constant string (1 .. 13) := "[LABEL BEGIN]";
	section_label_end				: constant string (1 .. 11) := "[LABEL END]";	

	section_submodule_ports_begin	: constant string (1 .. 23) := "[SUBMODULE_PORTS BEGIN]";
	section_submodule_ports_end		: constant string (1 .. 21) := "[SUBMODULE_PORTS END]";

	section_route_begin				: constant string (1 .. 13) := "[ROUTE BEGIN]";
	section_route_end				: constant string (1 .. 11) := "[ROUTE END]";	

	section_lines_begin				: constant string (1 .. 13) := "[LINES BEGIN]";
	section_lines_end				: constant string (1 .. 11) := "[LINES END]";
	
	section_line_begin				: constant string (1 .. 12) := "[LINE BEGIN]";
	section_line_end				: constant string (1 .. 10) := "[LINE END]";

	section_arc_begin				: constant string (1 .. 11) := "[ARC BEGIN]";
	section_arc_end					: constant string (1 ..  9) := "[ARC END]";

	section_circle_begin			: constant string (1 .. 14)	:= "[CIRCLE BEGIN]";
	section_circl_end				: constant string (1 .. 12)	:= "[CIRCLE END]";	
	
	section_polygon_begin			: constant string (1 .. 15) := "[POLYGON BEGIN]";
	section_polygon_end				: constant string (1 .. 13) := "[POLYGON END]";

	section_corners_begin			: constant string (1 .. 15) := "[CORNERS BEGIN]";
	section_corners_end				: constant string (1 .. 13) := "[CORNERS END]";
	
	section_via_begin				: constant string (1 .. 11) := "[VIA BEGIN]";
	section_via_end					: constant string (1 ..  9) := "[VIA END]";
	
	section_submodules_begin		: constant string (1 .. 18)	:= "[SUBMODULES BEGIN]";
	section_submodules_end			: constant string (1 .. 16)	:= "[SUBMODULES END]";	

	section_submodule_begin			: constant string (1 .. 17)	:= "[SUBMODULE BEGIN]";
	section_submodule_end			: constant string (1 .. 15)	:= "[SUBMODULE END]";	

	section_drawing_frames_begin	: constant string (1 .. 22)	:= "[DRAWING_FRAMES BEGIN]";
	section_drawing_frames_end		: constant string (1 .. 20)	:= "[DRAWING_FRAMES END]";

	section_schematic_begin			: constant string (1 .. 17)	:= "[SCHEMATIC BEGIN]";
	section_schematic_end			: constant string (1 .. 15)	:= "[SCHEMATIC END]";		

	section_board_begin				: constant string (1 .. 13)	:= "[BOARD BEGIN]";
	section_board_end				: constant string (1 .. 11)	:= "[BOARD END]";		

	section_devices_begin			: constant string (1 .. 15)	:= "[DEVICES BEGIN]";
	section_devices_end				: constant string (1 .. 13)	:= "[DEVICES END]";	

	section_device_begin			: constant string (1 .. 14)	:= "[DEVICE BEGIN]";
	section_device_end				: constant string (1 .. 12)	:= "[DEVICE END]";	

	section_units_begin				: constant string (1 .. 13)	:= "[UNITS BEGIN]";
	section_units_end				: constant string (1 .. 11)	:= "[UNITS END]";
	
	section_unit_begin				: constant string (1 .. 12)	:= "[UNIT BEGIN]";
	section_unit_end				: constant string (1 .. 10)	:= "[UNIT END]";	

	section_texts_begin				: constant string (1 .. 13)	:= "[TEXTS BEGIN]";
	section_texts_end				: constant string (1 .. 11)	:= "[TEXTS END]";	
	
	section_text_begin				: constant string (1 .. 12)	:= "[TEXT BEGIN]";
	section_text_end				: constant string (1 .. 10)	:= "[TEXT END]";	

	section_placeholder_begin		: constant string (1 .. 19)	:= "[PLACEHOLDER BEGIN]";
	section_placeholder_end			: constant string (1 .. 17)	:= "[PLACEHOLDER END]";	

	section_package_begin			: constant string (1 .. 15)	:= "[PACKAGE BEGIN]";
	section_package_end				: constant string (1 .. 13)	:= "[PACKAGE END]";

	section_silk_screen_begin		: constant string (1 .. 19)	:= "[SILK_SCREEN BEGIN]";
	section_silk_screen_end			: constant string (1 .. 17)	:= "[SILK_SCREEN END]";

	section_assembly_doc_begin		: constant string (1 .. 30)	:= "[ASSEMBLY_DOCUMENTATION BEGIN]";
	section_assembly_doc_end		: constant string (1 .. 28)	:= "[ASSEMBLY_DOCUMENTATION END]";

	section_stencil_begin			: constant string (1 .. 15)	:= "[STENCIL BEGIN]";
	section_stencil_end				: constant string (1 .. 13)	:= "[STENCIL END]";

	section_stop_mask_begin			: constant string (1 .. 17)	:= "[STOP_MASK BEGIN]";
	section_stop_mask_end			: constant string (1 .. 15)	:= "[STOP_MASK END]";
	
	section_keepout_begin			: constant string (1 .. 15)	:= "[KEEPOUT BEGIN]";
	section_keepout_end				: constant string (1 .. 13)	:= "[KEEPOUT END]";

	section_route_restrict_begin	: constant string (1 .. 22)	:= "[ROUTE_RESTRICT BEGIN]";
	section_route_restrict_end		: constant string (1 .. 20)	:= "[ROUTE_RESTRICT END]";

	section_via_restrict_begin		: constant string (1 .. 20)	:= "[VIA_RESTRICT BEGIN]";
	section_via_restrict_end		: constant string (1 .. 18)	:= "[VIA_RESTRICT END]";

	section_copper_begin			: constant string (1 .. 14)	:= "[COPPER BEGIN]";
	section_copper_end				: constant string (1 .. 12)	:= "[COPPER END]";

	section_pcb_contours_begin		: constant string (1 .. 31)	:= "[PCB_CONTOURS_NON_PLATED BEGIN]";
	section_pcb_contours_end		: constant string (1 .. 29)	:= "[PCB_CONTOURS_NON_PLATED END]";

	section_pcb_contours_plated		: constant string (1 .. 27)	:= "[PCB_CONTOURS_PLATED BEGIN]";
	section_pcb_contours_plated_end	: constant string (1 .. 25)	:= "[PCB_CONTOURS_PLATED END]";

	section_pac_3d_contours_begin	: constant string (1 .. 27)	:= "[PACKAGE_3D_CONTOURS BEGIN]";
	section_pac_3d_contours_end		: constant string (1 .. 25)	:= "[PACKAGE_3D_CONTOURS END]";
	
	section_top_begin				: constant string (1 .. 11)	:= "[TOP BEGIN]";
	section_top_end					: constant string (1 ..  9)	:= "[TOP END]";	

	section_bottom_begin			: constant string (1 .. 14)	:= "[BOTTOM BEGIN]";
	section_bottom_end				: constant string (1 .. 12)	:= "[BOTTOM END]";	

	section_variants_begin			: constant string (1 .. 16)	:= "[VARIANTS BEGIN]";
	section_variants_end			: constant string (1 .. 14)	:= "[VARIANTS END]";	

	section_variant_begin			: constant string (1 .. 15)	:= "[VARIANT BEGIN]";
	section_variant_end				: constant string (1 .. 13)	:= "[VARIANT END]";	

	section_terminal_port_map_begin	: constant string (1 .. 25)	:= "[TERMINAL_PORT_MAP BEGIN]";
	section_terminal_port_map_end	: constant string (1 .. 23)	:= "[TERMINAL_PORT_MAP END]";

	section_units_internal_begin	: constant string (1 .. 22)	:= "[UNITS_INTERNAL BEGIN]";
	section_units_internal_end		: constant string (1 .. 20)	:= "[UNITS_INTERNAL END]";

	section_units_external_begin	: constant string (1 .. 22)	:= "[UNITS_EXTERNAL BEGIN]";
	section_units_external_end		: constant string (1 .. 20)	:= "[UNITS_EXTERNAL END]";
	
	section_symbol_begin			: constant string (1 .. 14)	:= "[SYMBOL BEGIN]";
	section_symbol_end				: constant string (1 .. 12)	:= "[SYMBOL END]";
	
	section_shapes_begin			: constant string (1 .. 14)	:= "[SHAPES BEGIN]";
	section_shapes_end				: constant string (1 .. 12)	:= "[SHAPES END]";

	section_polyline_begin			: constant string (1 .. 16)	:= "[POLYLINE BEGIN]";
	section_polyline_end			: constant string (1 .. 14)	:= "[POLYLINE END]";	

	section_rectangle_begin			: constant string (1 .. 17)	:= "[RECTANGLE BEGIN]";
	section_rectangle_end			: constant string (1 .. 15)	:= "[RECTANGLE END]";	

	section_ports_begin				: constant string (1 .. 13)	:= "[PORTS BEGIN]";
	section_ports_end				: constant string (1 .. 11)	:= "[PORTS END]";	

	section_port_begin				: constant string (1 .. 12)	:= "[PORT BEGIN]";
	section_port_end				: constant string (1 .. 10)	:= "[PORT END]";	

	section_terminals_begin			: constant string (1 .. 17)	:= "[TERMINALS BEGIN]";
	section_terminals_end			: constant string (1 .. 15)	:= "[TERMINALS END]";

	section_terminal_begin			: constant string (1 .. 16)	:= "[TERMINAL BEGIN]";
	section_terminal_end			: constant string (1 .. 14)	:= "[TERMINAL END]";
	
	section_title_block_begin		: constant string (1 .. 19)	:= "[TITLE_BLOCK BEGIN]";
	section_title_block_end			: constant string (1 .. 17)	:= "[TITLE_BLOCK END]";	

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
