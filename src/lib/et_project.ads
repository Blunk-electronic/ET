------------------------------------------------------------------------------
--                                                                          --
--                          SYSTEM ET PROJECT                               --
--                                                                          --
--                                 ET                                       --
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
with ada.characters.latin_1;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_general;				use et_general;
with et_coordinates;			use et_coordinates;
with et_libraries;
with et_string_processing;
with et_schematic;
with submodules;
with assembly_variants;
with et_pcb;

package et_project is
	comment_mark : constant string := ("--");
	
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
	directory_dru			: constant string := "design_rules";
	directory_cam			: constant string := "CAM";
	directory_net_classes	: constant string := "net_classes";
	directory_templates		: constant string := "templates";		
	directory_settings		: constant string := "settings";
	directory_reports		: constant string := "reports";
	directory_documentation	: constant string := "documentation";
	directory_miscellaneous	: constant string := "miscellaneous";		

	function expand (
	-- Translates a file name like $HOME/libraries/devices/7400.dev to
	-- /home/user/libraries/devices/7400.dev
	-- CS: works on unix/linux only
		name_in			: in string) -- $HOME/libraries/devices/7400.dev
		--log_threshold	: et_string_processing.type_log_level)
		return string;
	
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
	function to_project_path (path : in string) return type_et_project_path.bounded_string;
	

	-- The module file name: -- CS move to et_general
	module_file_name_length_max : constant positive := 100;
	package type_module_file_name is new generic_bounded_length (module_file_name_length_max);

	function to_module_file_name (name : in string) return type_module_file_name.bounded_string;
	function to_string (name : in type_module_file_name.bounded_string) return string;
	



	-- The rig configuration is modelled here:
	rig_configuration_file_length_max : constant positive := 100;
	package type_rig_configuration_file_name is new generic_bounded_length (rig_configuration_file_length_max);
	use type_rig_configuration_file_name;
	
	rig_configuration_file_extension : constant string := "conf";
	rig_configuration_file_extension_asterisk : constant string := "*." & rig_configuration_file_extension;

	type type_module_instance is record
		generic_name		: type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		assembly_variant	: assembly_variants.type_variant_name.bounded_string; -- low_cost
		-- CS other properties ?
	end record;

	-- Lots of module instances are a map from the instance name to the type_module_instance.
	package type_module_instances is new ordered_maps (
		key_type		=> et_general.type_module_instance_name.bounded_string, -- LMX_1
		"<"				=> et_general.type_module_instance_name."<",
		element_type	=> type_module_instance);

	-- module connection (or board-to-board connector). NOTE: This could be a cable as well.
	type type_connector is record
		instance_A	: et_general.type_module_instance_name.bounded_string; -- LMX_2
		purpose_A	: et_libraries.type_device_purpose.bounded_string; -- pwr_in
		instance_B	: et_general.type_module_instance_name.bounded_string; -- PWR
		purpose_B	: et_libraries.type_device_purpose.bounded_string; -- pwr_out

		-- CS
		-- net_comparator : on/off 
		-- warn_only : on/off 
		-- cable moodel ?
	end record;

	function compare_connectors (left, right : in type_connector) return boolean;
	-- Returns true if left connector comes before right connector.

	package type_module_connectors is new ordered_sets (
		element_type	=> type_connector,
		"<"				=> compare_connectors);
	
	-- A rig consists of a list of module instances
	-- and a list of module-to-module connectors (or board-to-board connectors).
	-- It is modelled in a rig configuration file:
	type type_rig is record
		module_instances	: type_module_instances.map;
		connections			: type_module_connectors.set;
		-- CS description, docs, links, images ... ?
	end record;

	-- Lots of rigs are stored in a map:
	package type_rigs is new ordered_maps (
		key_type		=> type_rig_configuration_file_name.bounded_string, -- CS dedicated type_rig_name ?
		element_type	=> type_rig);

	-- The collection of rig configurations:
	rigs : type_rigs.map;

	-- Generic modules and submodules (which contain schematic and layout stuff)
	-- are collected here.
	-- Module names are things like "motor_driver" or "temperature_controller".
	-- Submodule names are things like "templates/clock_generator" or
	-- "$TEMPLATES/clock_generator" or "/home/user/templates/clock_generator":
	package type_modules is new ordered_maps (
		key_type		=> type_module_name.bounded_string, -- motor_driver (without extension *.mod)
		"<"				=> type_module_name."<",
		element_type	=> et_schematic.type_module,
		"="				=> et_schematic."=");

	modules : type_modules.map;

	function exists (module : in type_module_name.bounded_string) return boolean;
	-- Returns true if the module with the given name exists in container modules.

	function locate_module (name : in type_module_name.bounded_string) -- motor_driver (without extension *.mod)
	-- Locates the given module in the global container "modules".
		return type_modules.cursor;

	function port_connected (
	-- Returns true if given port of netchanger is connected with any net.
		module	: in type_modules.cursor;	
		port	: in et_schematic.type_port_netchanger)
		return boolean;
	
	function netchanger_as_port_available (
	-- Returns true if the given net provides a netchanger that may serve as port
	-- to a parent module.
		module	: in type_modules.cursor;
		net		: in et_schematic.type_nets.cursor) 
		return boolean;
	
	type type_section_name_rig_configuration is (
		SEC_INIT,
		SEC_MODULE_INSTANCES,
		SEC_MODULE,
		SEC_MODULE_CONNECTIONS,
		SEC_CONNECTOR
		);

	function to_string (section : in type_section_name_rig_configuration) return string;
	-- Converts a section like SEC_MODULE_INSTANCES to a string "module_instances".

	
	
    -- A sheet title may have 100 characters which seems sufficient for now.
 	sheet_title_length : constant natural := 100;    
	package type_sheet_title is new generic_bounded_length (sheet_title_length);

	subtype type_sheet_name_text_size is type_distance range 1.0 .. 5.0; -- unit is mm
	sheet_name_text_size_default : constant type_sheet_name_text_size := 1.3;

	function to_sheet_name_text_size (size : in string) return type_sheet_name_text_size;
	-- Converts a string to type_sheet_name_text_size.

	
	procedure create_project_directory (
	-- Creates given project directory in the given project_path.
	-- Creates a default rig configuration file.										   
	-- Already existing projects in given project_path are overwritten.
		project_name	: in type_project_name.bounded_string;		-- blood_sample_analyzer
		project_path	: in type_et_project_path.bounded_string;	-- /home/user/et_projects
		log_threshold	: in et_string_processing.type_log_level);

	procedure create_project_directory_bare (
	-- Creates a bare project (without a rig configuration file).
	-- Already existing projects in given path are overwritten.
	-- Sets the global project file name so that subsequent write and read operations
	-- know the right project file.
		project_name	: in type_project_name.bounded_string;		-- blood_sample_analyzer
		project_path	: in type_et_project_path.bounded_string; 	-- /home/user/et_projects
		log_threshold	: in et_string_processing.type_log_level);

	
	subtype type_tab_depth is natural range natural'first .. 9;
	tab_depth : type_tab_depth := type_tab_depth'first;
	
	tab : character renames et_string_processing.tabulator;
	space : character renames ada.characters.latin_1.space;

	
	type type_section_mark is (HEADER, FOOTER);

	procedure save_rig_configuration (
		project_name	: in type_project_name.bounded_string;		-- blood_sample_analyzer
		rig_conf_name	: in type_rig_configuration_file_name.bounded_string; -- demo, low_cost, fully_equipped
		rig				: in type_rig; -- the actual rig configuration		
		project_path	: in type_et_project_path.bounded_string; 	-- /home/user/et_projects
		log_threshold 	: in et_string_processing.type_log_level);
	-- Saves the rig configuration in the file with the given name rig_conf_file.	
	
	procedure save_module (
		module			: in et_schematic.type_module;				-- the module
		project_name	: in type_project_name.bounded_string;		-- blood_sample_analyzer
		module_name		: in type_module_name.bounded_string := to_module_name ("");	-- motor_driver
		project_path	: in type_et_project_path.bounded_string; 	-- /home/user/et_projects
		log_threshold 	: in et_string_processing.type_log_level);
	-- Saves the given module (incl. schematic and layout data) in the module file
	-- of the given project.
	-- If module_name not provided, the module will be named after the given project_name.


	procedure save_device (
		name			: in string; -- libraries/devices/resistor.dev
		device			: in et_libraries.type_device; -- the actual device model
		log_threshold	: in et_string_processing.type_log_level);

	procedure read_symbol (
	-- Opens the symbol file and stores the symbol in container et_libraries.symbols.
		file_name 		: in et_libraries.type_symbol_model_file.bounded_string; -- libraries/symbols/nand.sym
		log_threshold	: in et_string_processing.type_log_level);
    
	procedure read_device_file (
	-- Opens the device and stores it in container et_libraries.devices.
		file_name 		: in et_libraries.type_device_model_file.bounded_string; -- ../lbr/logic_ttl/7400.dev
		log_threshold	: in et_string_processing.type_log_level);
	
	procedure save_symbol (
	-- Saves the given symbol model in a file specified by name.
		name			: in string; -- libraries/symbols/resistor.sym
		symbol			: in et_libraries.type_symbol; -- the actual symbol model
		log_threshold	: in et_string_processing.type_log_level);
	
	procedure save_package (
		name			: in string; -- libraries/devices/resistor.pac
		packge			: in et_pcb.type_package; -- the actual device model
		log_threshold	: in et_string_processing.type_log_level);

	

-- FILE SECTIONS AND KEYWORDS
	
	keyword_generic_name			: constant string := "generic_name";
	keyword_instance_name			: constant string := "instance_name";
	keyword_assembly_variant		: constant string := "assembly_variant";
	keyword_instance_A				: constant string := "instance_A";
	keyword_instance_B				: constant string := "instance_B";		
	keyword_purpose_A				: constant string := "purpose_A";
	keyword_purpose_B				: constant string := "purpose_B";	
	keyword_net_comparator			: constant string := "net_comparator";
	keyword_net_comparator_warn_only: constant string := "warn_only";
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
	keyword_rotation_in_schematic	: constant string := "rotation_in_schematic";
	keyword_size					: constant string := "size";
	keyword_style					: constant string := "style"; -- CS remove ?
	keyword_line_width				: constant string := "line_width";
	keyword_appearance				: constant string := "appearance";
	keyword_direction				: constant string := "direction";			
	keyword_junction				: constant string := "junction";
	keyword_pos_x					: constant string := "x";
	keyword_pos_y					: constant string := "y";		
	keyword_device					: constant string := "device";
	keyword_port					: constant string := "port";
	keyword_submodule				: constant string := "submodule";
	keyword_netchanger				: constant string := "netchanger";		
	keyword_layer					: constant string := "layer";
	keyword_width					: constant string := "width";
	keyword_height					: constant string := "height";	
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
	--keyword_path					: constant string := "path";	
	keyword_position_in_board		: constant string := "position_in_board";
	keyword_position_in_schematic	: constant string := "position_in_schematic";	
	keyword_view_mode				: constant string := "view_mode";
	keyword_device_names_offset		: constant string := "device_names_offset";	
	keyword_content					: constant string := "content";
	keyword_radius					: constant string := "radius";
	keyword_filled					: constant string := "filled";		
	keyword_layers					: constant string := "layers";
	keyword_locked					: constant string := "locked";
	keyword_prefix					: constant string := "prefix";	
	keyword_commissioned			: constant string := "commissioned";
	keyword_updated					: constant string := "updated";
	keyword_author					: constant string := "author";
	keyword_file					: constant string := "file";
	keyword_package_model			: constant string := "package_model";
	keyword_terminal				: constant string := "terminal";
	keyword_unit					: constant string := "unit";		
	keyword_swap_level				: constant string := "swap_level";
	keyword_add_level				: constant string := "add_level";
	keyword_length					: constant string := "length";	
	keyword_port_name_visible		: constant string := "port_name_visible";
	keyword_terminal_name_visible	: constant string := "terminal_name_visible";
	keyword_port_name_size			: constant string := "port_name_size";
	keyword_terminal_name_size		: constant string := "terminal_name_size";
	keyword_sensitivity_edge		: constant string := "sensitivity_edge";
	keyword_sensitivity_level		: constant string := "sensitivity_level";
	keyword_inverted				: constant string := "inverted";
	keyword_weakness				: constant string := "weakness";
	keyword_tristate				: constant string := "tristate";	
	keyword_output_inverted			: constant string := "output_inverted";
	keyword_output_weakness			: constant string := "output_weakness";
	keyword_output_tristate			: constant string := "output_tristate";
	keyword_input_sensitivity_edge	: constant string := "input_sensitivity_edge";
	keyword_input_sensitivity_level	: constant string := "input_sensitivity_level";
	keyword_level					: constant string := "level";
	keyword_assembly_technology		: constant string := "technology";				
	keyword_pad_shape				: constant string := "pad_shape";
	keyword_tht_hole				: constant string := "hole";
	keyword_width_inner_layers		: constant string := "width_inner_layers";
	keyword_stop_mask				: constant string := "stop_mask";
	keyword_solder_paste			: constant string := "solder_paste";
	keyword_drill_size				: constant string := "drill_size";
	keyword_not_mounted				: constant string := "not_mounted";

	section_begin				: constant string := "BEGIN]";	
	section_end					: constant string := "END]";

	section_module_instances	: constant string := "[MODULE_INSTANCES";
	section_module_connections	: constant string := "[MODULE_CONNECTIONS";
	section_connector			: constant string := "[CONNECTOR";
	
	section_module				: constant string := "[MODULE";
	
	section_net_classes			: constant string := "[NET_CLASSES";
	section_net_class			: constant string := "[NET_CLASS";

	section_nets				: constant string := "[NETS";
	section_net					: constant string := "[NET";

	section_strands				: constant string := "[STRANDS";
	section_strand				: constant string := "[STRAND";

	section_segments			: constant string := "[SEGMENTS";
	section_segment				: constant string := "[SEGMENT";

	section_labels				: constant string := "[LABELS";
	section_label				: constant string := "[LABEL";

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

	section_assembly_variants	: constant string	:= "[ASSEMBLY_VARIANTS";
	section_assembly_variant	: constant string	:= "[VARIANT";	
	
	section_netchangers			: constant string 	:= "[NETCHANGERS";
	section_netchanger			: constant string 	:= "[NETCHANGER";
	
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
	--section_pcb_contours_plated	: constant string	:= "[PCB_CONTOURS_PLATED"; 
	section_pac_3d_contours		: constant string	:= "[PACKAGE_3D_CONTOURS";
	
	section_top					: constant string	:= "[TOP";
	section_bottom				: constant string	:= "[BOTTOM";

	section_variants			: constant string	:= "[VARIANTS";
	section_variant				: constant string	:= "[VARIANT";

	section_terminal_port_map	: constant string	:= "[TERMINAL_PORT_MAP";
	section_units_internal		: constant string	:= "[UNITS_INTERNAL";
	section_units_external		: constant string	:= "[UNITS_EXTERNAL";
	
	section_symbol				: constant string	:= "[SYMBOL";
	section_draw				: constant string	:= "[DRAW";
	
	section_ports				: constant string	:= "[PORTS";
	section_port_begin			: constant string	:= "[PORT";

	section_terminals			: constant string	:= "[TERMINALS";
	section_terminal			: constant string	:= "[TERMINAL";
	
	section_pad_contours_smt	: constant string	:= "[PAD_CONTOURS_SMT";
	section_pad_contours_tht	: constant string	:= "[PAD_CONTOURS_THT";	
	section_pad_millings		: constant string	:= "[MILLINGS";
	
	section_title_block			: constant string	:= "[TITLE_BLOCK";
	
	type type_section_name_module is (
		SEC_INIT,
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
		SEC_PORTS,
		SEC_PORT,
		SEC_ROUTE,
		SEC_LINE,
		SEC_ARC,
		SEC_POLYGON,
		SEC_CORNERS,
		SEC_VIA,
		SEC_SUBMODULES,
		SEC_SUBMODULE,
		SEC_DRAWING_FRAMES,
		SEC_SCHEMATIC,
		SEC_BOARD,
		SEC_DEVICES,
		SEC_DEVICE,
		SEC_ASSEMBLY_VARIANTS,
		SEC_ASSEMBLY_VARIANT,
		SEC_NETCHANGERS,
		SEC_NETCHANGER,
		SEC_TEXTS,
		SEC_TEXT,
		SEC_UNITS,
		SEC_UNIT,
		SEC_PACKAGE,
		SEC_PLACEHOLDER,
		SEC_PLACEHOLDERS,		
		SEC_SILK_SCREEN,
		SEC_CIRCLE,
		SEC_ASSEMBLY_DOCUMENTATION,
		SEC_KEEPOUT,
		SEC_ROUTE_RESTRICT,
		SEC_VIA_RESTRICT,
		SEC_STOP_MASK,
		SEC_STENCIL,
		SEC_COPPER,
		SEC_PCB_CONTOURS_NON_PLATED,
		-- CS SEC_PCB_CONTOUR_PLATED
		SEC_TOP,
		SEC_BOTTOM
		);

	function to_string (section : in type_section_name_module) return string;
	-- Converts a section like SEC_NET to a string "net".
	
	procedure read_module_file (
		file_name 		: in string; -- motor_driver.mod, templates/clock_generator.mod
		log_threshold	: in et_string_processing.type_log_level);
	
	procedure open_project (log_threshold : in et_string_processing.type_log_level);
	-- Enters the project directory specified by project_name.
	-- Searches for rig configuration files (*.conf), reads them and stores configurations in et_project.rigs.
	-- Searches for module files (*.mod), reads them and stores modules in et_project.modules.

	procedure save_project (
		destination		: in type_project_name.bounded_string; -- blood_sample_analyzer
		log_threshold 	: in et_string_processing.type_log_level);
	
	type type_section_name_device is (
		SEC_INIT,
		SEC_VARIANTS,
		SEC_VARIANT,
		SEC_TERMINAL_PORT_MAP,
		SEC_UNITS_INTERNAL,
		SEC_UNIT,
		SEC_SYMBOL,
		SEC_DRAW,
		SEC_LINE,
		SEC_ARC,
		SEC_CIRCLE,
		SEC_TEXTS,
		SEC_TEXT,
		SEC_PLACEHOLDER,		
		SEC_PLACEHOLDERS,
		SEC_PORTS,
		SEC_PORT,
		SEC_UNITS_EXTERNAL
		);

	type type_section_name_symbol is (
		SEC_INIT,
		SEC_DRAW,
		SEC_LINE,
		SEC_ARC,
		SEC_CIRCLE,
		SEC_TEXTS,
		SEC_TEXT,
		SEC_PLACEHOLDER,		
		SEC_PLACEHOLDERS,
		SEC_PORTS,
		SEC_PORT
		);

	type type_section_name_package is (
		SEC_INIT,
		SEC_TOP,
		SEC_BOTTOM,
		SEC_LINE,
		SEC_ARC,
		SEC_CIRCLE,
		SEC_SILK_SCREEN,
		SEC_ASSEMBLY_DOCUMENTATION,
		SEC_KEEPOUT,
		SEC_COPPER,
		SEC_STOP_MASK,
		SEC_STENCIL,
		SEC_ROUTE_RESTRICT,
		SEC_VIA_RESTRICT,
		SEC_PCB_CONTOURS_NON_PLATED,
		SEC_TERMINALS,
		SEC_TERMINAL,
		SEC_PAD_CONTOURS_SMT,
		SEC_PAD_CONTOURS_THT,		
		SEC_MILLINGS,
		SEC_TEXT,
		SEC_PLACEHOLDER,
		SEC_POLYGON,
		SEC_CORNERS,
		SEC_PACKAGE_3D_CONTOURS
		);
	

	function exists (
	-- Returns true if the given module provides the given port.
	-- The module being searched in must be in the rig already.						
		module			: in submodules.type_submodules.cursor;
		port			: in et_general.type_net_name.bounded_string)
		return boolean;

	function exists (
	-- Returns true if the given module provides the given device.
	-- The module being searched in must be in the rig already.						
		module	: in type_modules.cursor;
		device	: in et_libraries.type_device_name)
		return boolean;
	
	function exists (
	-- Returns true if the given module provides the given submodule instance.
	-- The module being searched in must be in the rig already.						
		module		: in type_modules.cursor; -- the parent module that contains the submodule instance
		instance	: in et_general.type_module_instance_name.bounded_string) -- OSC1
		return boolean;

	function exists (
	-- Returns true if the given submodule instance provides the
	-- given assembly variant. The submodule instance is searched for
	-- in the parent module indicated by cursor "module".
	-- The module being searched in must be in the rig already.						
		module		: in type_modules.cursor; -- the parent module that contains the submodule instance
		instance	: in et_general.type_module_instance_name.bounded_string; -- OSC1
		variant		: in assembly_variants.type_variant_name.bounded_string) -- low_cost				
		return boolean;

	function exists (
	-- Returns true if the given module provides the given assembly variant.
	-- If the variant is an empty string then it is about the default variant
	-- which is always provided. The return is true in that case.
		module		: in type_modules.cursor;
		variant		: in assembly_variants.type_variant_name.bounded_string) -- low_cost
		return boolean;	

	function exists (
	-- Returns true if the given module and variant provides the given device.
	-- Assumptions: 
	-- - The module being searched in must be in the rig already.
	-- - The assembly variant must exist in the module.
	-- - The device must exist in the module.
		module	: in type_modules.cursor; -- the module like motor_driver
		variant	: in assembly_variants.type_variant_name.bounded_string; -- low_cost				
		device	: in et_libraries.type_device_name)
		return boolean;

	function alternative_device (
	-- Returns a cursor to the alternative device in the given module
	-- and given assembly variant.
	-- Assumptions: 
	-- - The module being searched in must be in the rig already.
	-- - The assembly variant must exist in the module.
	-- - The device must exist in the module.
	-- - The device must have an entry in the given assembly variant,
	--   otherwise the return is no_element.
		module	: in type_modules.cursor; -- the module like motor_driver
		variant	: in assembly_variants.type_variant_name.bounded_string; -- low_cost				
		device	: in et_libraries.type_device_name)
		return assembly_variants.type_devices.cursor;

	function alternative_submodule (
	-- Returns a cursor to the alternative submodule variant in the given module
	-- and given assembly variant.
	-- Assumptions: 
	-- - The module being searched in must be in the rig already.
	-- - The assembly variant must exist in the module.
	-- - The suubmodule must have been instantiated in the module.
	-- - The submodule must have an entry in the given assembly variant,
	--   otherwise the return is no_element.
	-- If the given variant is an emtpy string (means default variant) the return
	-- is no_element.
		module	: in type_modules.cursor; -- the module like motor_driver
		variant	: in assembly_variants.type_variant_name.bounded_string; -- low_cost
		submod	: in et_general.type_module_instance_name.bounded_string) -- OSC1
		return assembly_variants.type_submodules.cursor;

	
-- GENERICS
	
	generic
		max : positive;
		type item is private;
	package stack_lifo is
		procedure push (x : in item);
		procedure pop;
		function pop return item;
		function depth return natural;
		procedure init;
		function empty return boolean;
		function current return item;
		function parent (degree : in natural := 1) return item;
		
	end stack_lifo;
	
end et_project;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
