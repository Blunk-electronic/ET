------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               PROJECT                                    --
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
with et_string_processing;
with et_schematic;
with submodules;
with netlists;
with assembly_variants;
with et_packages;
with et_pcb;
with et_pcb_stack;
with et_devices;				use et_devices;
with et_frames;

package et_project is
	comment_mark : constant string := ("--");
	
	-- after importing a foreign project, native ET projects are created here:
	directory_import		: constant string := "et_import";

	-- native ET projects live in a parent folder
	directory_projects		: constant string := "projects";

	-- native library directories
	directory_libraries 				: constant string := "libraries";	
	directory_libraries_devices			: constant string := "devices";
	directory_libraries_symbols			: constant string := "symbols";
	directory_libraries_packages		: constant string := "packages";	

	directory_libraries_schemlets		: constant string := "schemlets";
	
	-- supplementary stuff of a project
-- 	directory_dru			: constant string := "design_rules";
-- 	directory_cam			: constant string := "CAM";
-- 	directory_net_classes	: constant string := "net_classes";
	directory_templates		: constant string := "templates";		
-- 	directory_settings		: constant string := "settings";
	directory_reports		: constant string := "reports";
	directory_documentation	: constant string := "documentation";
	directory_miscellaneous	: constant string := "miscellaneous";		

	subtype type_file_name_text_size is type_distance range 1.0 .. 5.0; -- unit is mm
	file_name_text_size_default : constant type_file_name_text_size := 1.3;

	function to_file_name_text_size (size : in string) return type_file_name_text_size;

	
	-- This is the root directory where all projects live:
	projects_root_dir_length : constant natural := 100;
	package type_projects_root_dir is new generic_bounded_length (projects_root_dir_length);
	projects_root_dir : type_projects_root_dir.bounded_string;
	
	
	project_name_max : constant natural := 100;
	package type_project_name is new generic_bounded_length (project_name_max);
	
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
		assembly_variant	: et_general.type_variant_name.bounded_string; -- low_cost
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
		purpose_A	: et_devices.type_purpose.bounded_string; -- pwr_in
		instance_B	: et_general.type_module_instance_name.bounded_string; -- PWR
		purpose_B	: et_devices.type_purpose.bounded_string; -- pwr_out

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

	-- Returns the description of a sheet of a generic module:
	function sheet_description (
		module	: in type_modules.cursor;
		sheet	: in type_sheet)
		return et_frames.type_schematic_description;
								   
	procedure port_not_at_edge (name : in et_general.type_net_name.bounded_string);
	
	function port_connected (
	-- Returns true if given port of netchanger is connected with any net.
		module	: in type_modules.cursor;	
		port	: in netlists.type_port_netchanger)
		return boolean;

	function locate_net (
	-- Returns a cursor to the given net in the given module.
		module_cursor	: in type_modules.cursor;
		net_name		: in type_net_name.bounded_string)
		return et_schematic.type_nets.cursor;
	
	function netchanger_as_port_available (
	-- Returns true if the given net provides a netchanger that may serve as port
	-- to a parent module.
		module		: in type_modules.cursor;
		net			: in et_schematic.type_nets.cursor;
		direction	: in submodules.type_netchanger_port_name) -- master/slave 		
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
--  	sheet_title_length : constant natural := 100;    -- CS no longer required
-- 	package type_sheet_title is new generic_bounded_length (sheet_title_length); -- CS no longer required

	subtype type_sheet_name_text_size is type_distance range 1.0 .. 5.0; -- unit is mm -- CS no longer required ?
	sheet_name_text_size_default : constant type_sheet_name_text_size := 1.3; -- CS no longer required ?

	function to_sheet_name_text_size (size : in string) return type_sheet_name_text_size; -- CS no longer required ?
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
	
	procedure save_rig_configuration (
		project_name	: in type_project_name.bounded_string;		-- blood_sample_analyzer
		rig_conf_name	: in type_rig_configuration_file_name.bounded_string; -- demo, low_cost, fully_equipped
		rig				: in type_rig; -- the actual rig configuration		
		project_path	: in type_et_project_path.bounded_string; 	-- /home/user/et_projects
		log_threshold 	: in et_string_processing.type_log_level);
	-- Saves the rig configuration in the file with the given name rig_conf_file.	

	procedure save_module (
	-- Saves the given generic module in the given file.
		module_cursor		: in type_modules.cursor;
		module_file_name	: in type_module_file_name.bounded_string; -- led_matrix.mod
		log_threshold		: in et_string_processing.type_log_level);
	
	procedure save_module (
		module_cursor	: in type_modules.cursor;				-- the module
		project_name	: in type_project_name.bounded_string;		-- blood_sample_analyzer
		module_name		: in type_module_name.bounded_string := to_module_name ("");	-- motor_driver
		project_path	: in type_et_project_path.bounded_string; 	-- /home/user/et_projects
		log_threshold 	: in et_string_processing.type_log_level);
	-- Saves the given module in the module file of the given project.
	-- If module_name not provided, the module will be named after the given project_name.

	
	

	

-- KEYWORDS

	keyword_default					: constant string := "default";
	keyword_generic_name			: constant string := "generic_name";
	keyword_instance_name			: constant string := "instance_name";
	keyword_assembly_variant		: constant string := "assembly_variant";
	keyword_not_mounted				: constant string := "not_mounted";

	keyword_instance_A				: constant string := "instance_A";
	keyword_instance_B				: constant string := "instance_B";		
	keyword_purpose_A				: constant string := "purpose_A";
	keyword_purpose_B				: constant string := "purpose_B";	
	keyword_net_comparator			: constant string := "net_comparator";
	keyword_net_comparator_warn_only: constant string := "warn_only";

	keyword_class					: constant string := "class";

	keyword_scope					: constant string := "scope";	
	keyword_flipped					: constant string := "flipped";
	keyword_rotation_in_schematic	: constant string := "rotation_in_schematic";

	keyword_junction				: constant string := "junction";
	keyword_submodule				: constant string := "submodule";
	keyword_netchanger				: constant string := "netchanger";		
	keyword_height					: constant string := "height";	
	keyword_template				: constant string := "template";
	keyword_origin					: constant string := "origin";
	keyword_model					: constant string := "model";				
	keyword_variant					: constant string := "variant";
	keyword_mirrored				: constant string := "mirrored";

	keyword_sheet_number			: constant string := "number";
	keyword_sheet_category			: constant string := "category";
	keyword_sheet_description		: constant string := "text";

	
-- SECTION NAMES
	
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
	
	section_submodules			: constant string := "[SUBMODULES";
	section_submodule			: constant string := "[SUBMODULE";

	section_drawing_frames		: constant string := "[DRAWING_FRAMES";
	section_sheet_descriptions	: constant string := "[SHEET_DESCRIPTIONS";
	section_sheet				: constant string := "[SHEET";
	
	section_schematic			: constant string := "[SCHEMATIC";

	section_devices				: constant string := "[DEVICES";
	section_device				: constant string := "[DEVICE";

	section_assembly_variants	: constant string := "[ASSEMBLY_VARIANTS";
	section_assembly_variant	: constant string := "[VARIANT";	
	
	section_netchangers			: constant string := "[NETCHANGERS";
	section_netchanger			: constant string := "[NETCHANGER";

	section_meta				: constant string := "[META";
	
	section_units				: constant string := "[UNITS";

	section_port_begin			: constant string := "[PORT";
	
	
	type type_section is ( -- CS: sort aphabetically
		SEC_BOARD_LAYER_STACK,
		SEC_CONTOURS, -- of fill and cutout zones
		SEC_CUTOUT_ZONE,
		SEC_DRAWING_GRID,
		SEC_FILL_ZONE,
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
		SEC_VIA,
		SEC_SUBMODULES,
		SEC_SUBMODULE,
		SEC_SHEET_DESCRIPTIONS,
		SEC_SHEET,
		SEC_DRAWING_FRAMES,
		SEC_SCHEMATIC,
		SEC_BOARD,
		SEC_DEVICES,
		SEC_DEVICE,
		SEC_ASSEMBLY_VARIANTS,
		SEC_ASSEMBLY_VARIANT,
		SEC_NETCHANGERS,
		SEC_NETCHANGER,
		SEC_META,
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

	function to_string (section : in type_section) return string;
	-- Converts a section like SEC_NET to a string "net".
	
	procedure read_module_file (
	-- Reads a module file and stores its content as generic module in container modules.
	-- The file name may contain environment variables.
	-- The file must exist, must be visible from the current working directory.
		file_name 		: in string; -- motor_driver.mod, templates/clock_generator.mod
		log_threshold	: in et_string_processing.type_log_level);

	procedure create_module (
	-- Creates an empty generic module in container modules.								   
		module_name		: in type_module_name.bounded_string; -- motor_driver, templates/clock_generator
		log_threshold	: in et_string_processing.type_log_level);

	procedure save_module (
	-- Saves a generic module (from container modules) in a file inside the current project directory. 
		module_name		: in type_module_name.bounded_string; -- motor_driver, templates/clock_generator
		log_threshold	: in et_string_processing.type_log_level);
	
	procedure delete_module (
	-- Deletes a generic module in container modules.
	-- Deletes the module file of the generic module.								
		module_name		: in type_module_name.bounded_string; -- motor_driver, templates/clock_generator
		log_threshold	: in et_string_processing.type_log_level);
	
	procedure open_project (
		project_name 	: in type_project_name.bounded_string; -- blood_sample_analyzer
		log_threshold 	: in et_string_processing.type_log_level);
	-- Enters the project directory specified by project_name.
	-- Searches for rig configuration files (*.conf), reads them and stores configurations in et_project.rigs.
	-- Searches for module files (*.mod), reads them and stores modules in et_project.modules.

	procedure save_project (
		destination		: in type_project_name.bounded_string; -- blood_sample_analyzer
		log_threshold 	: in et_string_processing.type_log_level);
	


	

	function exists (
	-- Returns true if the given module provides the given port.
	-- The module being searched in must be in the rig already.						
		module			: in submodules.type_submodules.cursor;
		port			: in et_general.type_net_name.bounded_string;
		direction		: in submodules.type_netchanger_port_name) -- master/slave		
		return boolean;

	function exists (
	-- Returns true if the given module provides the given device.
	-- The module being searched in must be in the rig already.						
		module	: in type_modules.cursor;
		device	: in type_name)
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
		variant		: in et_general.type_variant_name.bounded_string) -- low_cost				
		return boolean;

	function exists (
	-- Returns true if the given module provides the given assembly variant.
	-- If the variant is an empty string then it is about the default variant
	-- which is always provided. The return is true in that case.
		module		: in type_modules.cursor;
		variant		: in et_general.type_variant_name.bounded_string) -- low_cost
		return boolean;	

	function exists (
	-- Returns true if the given module and variant provides the given device.
	-- Assumptions: 
	-- - The module being searched in must be in the rig already.
	-- - The assembly variant must exist in the module.
	-- - The device must exist in the module.
		module	: in type_modules.cursor; -- the module like motor_driver
		variant	: in et_general.type_variant_name.bounded_string; -- low_cost				
		device	: in type_name)
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
		variant	: in et_general.type_variant_name.bounded_string; -- low_cost				
		device	: in type_name)
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
		variant	: in et_general.type_variant_name.bounded_string; -- low_cost
		submod	: in et_general.type_module_instance_name.bounded_string) -- OSC1
		return assembly_variants.type_submodules.cursor;

-- 	function package_model (
-- 		module	: in type_modules.cursor; -- the module like motor_driver
-- 		device	: in et_libraries.type_name; -- IC40
-- 		return et_libraries.type_package_model_file.bounded_string; -- libraries/packages/smd/SOT23.pac
							   
end et_project;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
