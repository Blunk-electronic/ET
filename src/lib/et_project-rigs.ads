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

with ada.containers;            	use ada.containers;
with ada.containers.ordered_maps;
with ada.containers.ordered_sets;
with et_devices;
with et_conventions;


package et_project.rigs is

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
	-- Conventions apply for the whole rig.
	
	-- CS: Discussion required whether to apply conventions to the whole project,
	-- means to all rigs. This would require a project configuration file.
	
	-- A single rig is modelled by this type and stored in a 
	-- similar structured rig configuration file:
	type type_rig is record
		module_instances	: type_module_instances.map;
		connections			: type_module_connectors.set;
		conventions			: et_conventions.pac_file_name.bounded_string; -- ../conventions.txt
		-- CS description, docs, links, images ... ?
	end record;

	-- Lots of rigs are stored in a map:
	package pac_rigs is new ordered_maps (
		key_type		=> type_rig_configuration_file_name.bounded_string, -- CS dedicated type_rig_name ?
		element_type	=> type_rig);

	-- The collection of rig configurations:
	rigs : pac_rigs.map;

	
	type type_section_name is (
		SEC_INIT,
		SEC_MODULE_INSTANCES,
		SEC_MODULE,
		SEC_MODULE_CONNECTIONS,
		SEC_CONNECTOR
		);

	function to_string (section : in type_section_name) return string;
	-- Converts a section like SEC_MODULE_INSTANCES to a string "module_instances".


	procedure write_rig_configuration_header;
	procedure write_rig_configuration_footer;
	
	procedure save_rig_configuration (
		project_name	: in type_project_name.bounded_string;		-- blood_sample_analyzer
		rig_conf_name	: in type_rig_configuration_file_name.bounded_string; -- demo, low_cost, fully_equipped
		rig				: in type_rig; -- the actual rig configuration		
		project_path	: in type_et_project_path.bounded_string; 	-- /home/user/et_projects
		log_threshold 	: in et_string_processing.type_log_level);
	-- Saves the rig configuration in the file with the given name rig_conf_file.	

	

-- KEYWORDS

	keyword_generic_name			: constant string := "generic_name";
	keyword_instance_name			: constant string := "instance_name";
	keyword_instance_A				: constant string := "instance_A";
	keyword_instance_B				: constant string := "instance_B";		
	keyword_purpose_A				: constant string := "purpose_A";
	keyword_purpose_B				: constant string := "purpose_B";	
	keyword_net_comparator			: constant string := "net_comparator";
	keyword_net_comparator_warn_only: constant string := "warn_only";

	
-- SECTION NAMES
	
	section_module_instances	: constant string := "[MODULE_INSTANCES";
	section_module_connections	: constant string := "[MODULE_CONNECTIONS";
	section_connector			: constant string := "[CONNECTOR";
	section_module				: constant string := "[MODULE";

	
	
	-- Enters the project directory specified by project_name.
	-- Searches for rig configuration files (*.conf), reads them and stores configurations in et_project.rigs.rigs.
	-- Searches for module files (*.mod), reads them and stores modules in et_project.modules.generic_modules.
	procedure read_rigs (
		project_name 	: in type_project_name.bounded_string; -- blood_sample_analyzer
		log_threshold 	: in et_string_processing.type_log_level);
	
end et_project.rigs;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
