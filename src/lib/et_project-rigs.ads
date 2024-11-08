------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            PROJECT RIGS                                  --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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
--                                                                          --
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
with et_device_purpose;				use et_device_purpose;
with et_conventions;
with et_assembly_variants;			use et_assembly_variants;
with et_module_names;				use et_module_names;
with et_rig_name;					use et_rig_name;
with et_module_instance;			use et_module_instance;


package et_project.rigs is

	
	-- module connection (or board-to-board connector). NOTE: This could be a cable as well.
	type type_connector is record
		instance_A	: pac_module_instance_name.bounded_string; -- LMX_2
		purpose_A	: pac_device_purpose.bounded_string; -- pwr_in
		instance_B	: pac_module_instance_name.bounded_string; -- PWR
		purpose_B	: pac_device_purpose.bounded_string; -- pwr_out

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
	
	-- A single rig is modelled by this type and stored in a 
	-- similar structured rig configuration file:
	type type_rig is record
		module_instances	: type_module_instances.map;
		connections			: type_module_connectors.set;
		conventions			: et_conventions.pac_file_name.bounded_string; -- ../conventions.txt
		-- CS description, docs, links, images ... ?
	end record;


	use et_rig_name.pac_file_name;
	
	-- Lots of rigs are stored in a map:
	package pac_rigs is new ordered_maps (
		key_type		=> et_rig_name.pac_file_name.bounded_string, -- CS dedicated type_rig_name ?
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
	
	-- Saves the given rig in the current working directory
	-- in a *.rig file.
	procedure save_rig_configuration ( -- CS rename to save_rig
		rig_cursor		: in pac_rigs.cursor;
		log_threshold 	: in type_log_level);

	

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

	-- Assumes the current working directory is a project.
	-- Searches for rig configuration files (*.rig), reads them and 
	-- stores them in et_project.rigs.rigs.
	-- Searches for module files (*.mod) in the current directory,
	-- reads them and stores then
	-- in et_project.modules.generic_modules.
	-- Use this procedure when opening a project.
	procedure read_rigs (
		log_threshold 	: in type_log_level);
	
end et_project.rigs;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
