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

	procedure write_project_footer;
	-- writes a nice footer in the project file and closes it.
	

-- 	procedure write_component_libraries (log_threshold : in et_string_processing.type_log_level);
-- 	-- Writes the ET native component libraries in libraries_directory_name.

	-- PROJECT FILE SECTIONS
	project_module_begin		: constant string (1 .. 14) := "[MODULE BEGIN]";
	project_module_end			: constant string (1 .. 12) := "[MODULE END]";	

	project_net_classes_begin	: constant string (1 .. 19) := "[NET_CLASSES BEGIN]";
	project_net_classes_end		: constant string (1 .. 17) := "[NET_CLASSES END]";	
	project_net_class_begin		: constant string (1 .. 17) := "[NET_CLASS BEGIN]";
	project_net_class_end		: constant string (1 .. 15) := "[NET_CLASS END]";	

	project_nets_begin			: constant string (1 .. 12) := "[NETS BEGIN]";
	project_nets_end			: constant string (1 .. 10) := "[NETS END]";	
	project_net_begin			: constant string (1 .. 11) := "[NET BEGIN]";
	project_net_end				: constant string (1 ..  9) := "[NET END]";	

	project_strands_begin		: constant string (1 .. 15) := "[STRANDS BEGIN]";
	project_strands_end			: constant string (1 .. 13) := "[STRANDS END]";
	project_strand_begin		: constant string (1 .. 14) := "[STRAND BEGIN]";
	project_strand_end			: constant string (1 .. 12) := "[STRAND END]";

	project_segments_begin		: constant string (1 .. 16) := "[SEGMENTS BEGIN]";
	project_segments_end		: constant string (1 .. 14) := "[SEGMENTS END]";	
	project_segment_begin		: constant string (1 .. 15) := "[SEGMENT BEGIN]";
	project_segment_end			: constant string (1 .. 13) := "[SEGMENT END]";	

	project_labels_begin		: constant string (1 .. 14) := "[LABELS BEGIN]";
	project_labels_end			: constant string (1 .. 12) := "[LABELS END]";
	project_label_begin			: constant string (1 .. 13) := "[LABEL BEGIN]";
	project_label_end			: constant string (1 .. 11) := "[LABEL END]";	

	project_submodule_ports_begin	: constant string (1 .. 23) := "[SUBMODULE_PORTS BEGIN]";
	project_submodule_ports_end		: constant string (1 .. 21) := "[SUBMODULE_PORTS END]";
	project_port_begin				: constant string (1 .. 12) := "[PORT BEGIN]";
	project_port_end				: constant string (1 .. 10) := "[PORT END]";

	project_route_begin				: constant string (1 .. 13) := "[ROUTE BEGIN]";
	project_route_end				: constant string (1 .. 11) := "[ROUTE END]";	

	project_line_begin				: constant string (1 .. 12) := "[LINE BEGIN]";
	project_line_end				: constant string (1 .. 10) := "[LINE END]";

	project_arc_begin				: constant string (1 .. 11) := "[ARC BEGIN]";
	project_arc_end					: constant string (1 ..  9) := "[ARC END]";

	project_polygon_begin			: constant string (1 .. 15) := "[POLYGON BEGIN]";
	project_polygon_end				: constant string (1 .. 13) := "[POLYGON END]";

	project_via_begin				: constant string (1 .. 11) := "[VIA BEGIN]";
	project_via_end					: constant string (1 ..  9) := "[VIA END]";
	
	project_submodules_begin		: constant string (1 .. 18)	:= "[SUBMODULES BEGIN]";
	project_submodules_end			: constant string (1 .. 16)	:= "[SUBMODULES END]";	
	project_submodule_begin			: constant string (1 .. 17)	:= "[SUBMODULE BEGIN]";
	project_submodule_end			: constant string (1 .. 15)	:= "[SUBMODULE END]";	

	project_drawing_frames_begin	: constant string (1 .. 22)	:= "[DRAWING_FRAMES BEGIN]";
	project_drawing_frames_end		: constant string (1 .. 20)	:= "[DRAWING_FRAMES END]";
	project_schematic_begin			: constant string (1 .. 17)	:= "[SCHEMATIC BEGIN]";
	project_schematic_end			: constant string (1 .. 15)	:= "[SCHEMATIC END]";		
	project_board_begin				: constant string (1 .. 13)	:= "[BOARD BEGIN]";
	project_board_end				: constant string (1 .. 11)	:= "[BOARD END]";		

	project_devices_begin			: constant string (1 .. 15)	:= "[DEVICES BEGIN]";
	project_devices_end				: constant string (1 .. 13)	:= "[DEVICES END]";	
	project_device_begin			: constant string (1 .. 14)	:= "[DEVICE BEGIN]";
	project_device_end				: constant string (1 .. 12)	:= "[DEVICE END]";	
	project_unit_begin				: constant string (1 .. 12)	:= "[UNIT BEGIN]";
	project_unit_end				: constant string (1 .. 10)	:= "[UNIT END]";	
	project_placeholder_begin		: constant string (1 .. 19)	:= "[PLACEHOLDER BEGIN]";
	project_placeholder_end			: constant string (1 .. 17)	:= "[PLACEHOLDER END]";	
	project_package_begin			: constant string (1 .. 15)	:= "[PACKAGE BEGIN]";
	project_package_end				: constant string (1 .. 13)	:= "[PACKAGE END]";




end et_project;

-- Soli Deo Gloria
