------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               PROJECT                                    --
--                                                                          --
--                               B o d y                                    --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	--use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.text_io;				use ada.text_io;

with ada.tags;

with ada.exceptions;
with ada.directories;
with gnat.directory_operations;

with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;

with et_general;				use et_general;
with et_coordinates;
with et_string_processing;
with et_export;
with et_import;
with et_schematic;
with et_packages;
with et_pcb;
with et_pcb_stack;
with et_pcb_coordinates;
with et_material;
with et_text;					use et_text;
with et_geometry;				use et_geometry;
with general_rw;				use general_rw;
with pcb_rw;					use pcb_rw;
with pcb_rw.device_packages;	use pcb_rw.device_packages;
with schematic_rw;				use schematic_rw;
with symbol_rw;					use symbol_rw;
with device_rw;					use device_rw;
with et_symbols;
with frame_rw;
with et_meta;
with et_design_rules;
with et_project.modules;
	
package body et_project.rigs is

	use et_general.type_net_name;

	function to_string (section : in type_section_name_rig_configuration) return string is
	-- Converts a section like SEC_MODULE_INSTANCES to a string "module_instances".
		len : positive := type_section_name_rig_configuration'image (section)'length;
	begin
		return to_lower (type_section_name_rig_configuration'image (section) (5..len));
	end to_string;

	function compare_connectors (left, right : in type_connector) return boolean is
	-- Returns true if left connector comes before right connector.
	-- Returns false if connectors are equal.
		use et_devices.type_purpose;
		use type_module_instance_name;
		r : boolean := false; -- to be returned
	begin
		-- First we compare instance_A
		if left.instance_A > right.instance_A then
			r := true;
		elsif left.instance_A < right.instance_A then
			r := false;
		else -- left instance_A equals right instance_A

			-- compare instance_B
			if left.instance_B > right.instance_B then
				r := true;
			elsif left.instance_B < right.instance_B then
				r := false;
			else -- left instance_B equals right instance_B

				-- compare purpose_A
				if left.purpose_A > right.purpose_A then
					r := true;
				elsif left.purpose_A < right.purpose_A then
					r := false;
				else -- left purpose_A equals right purpose_A

					-- compare purpose_B
					if left.purpose_B > right.purpose_B then
						r := true;
					elsif left.purpose_B < right.purpose_B then
						r := false;
					else 
						-- left purpose_B equals right purpose_B
						-- means: connectors are equal
						r := false;
					end if;
				end if;
			end if;
		end if;
		
		return r;
	end compare_connectors;
	
	procedure write_rig_configuration_header is 
		use et_general;
		use et_string_processing;
	begin
		-- write a nice header
		put_line (comment_mark & " " & system_name & " rig configuration file");
		put_line (comment_mark & " " & date);
		put_line (comment_mark & " " & row_separator_double);
		new_line;
	end;

	procedure write_rig_configuration_footer is
		use et_string_processing;
	begin
		-- write a nice footer
		new_line;
		put_line (comment_mark & " " & row_separator_double);
		put_line (comment_mark & " " & date);
		put_line (comment_mark & " rig configuration file end");
		new_line;
	end;

	
	-- Saves the rig configuration in the file with the given name rig_conf_file.
	procedure save_rig_configuration (
		project_name	: in type_project_name.bounded_string;		-- blood_sample_analyzer
		rig_conf_name	: in type_rig_configuration_file_name.bounded_string; -- demo, low_cost, fully_equipped
		rig				: in type_rig; -- the actual rig configuration				
		project_path	: in type_et_project_path.bounded_string; 	-- /home/user/et_projects
		log_threshold 	: in et_string_processing.type_log_level) 
		is separate;

	-- Enters the project directory specified by project_name.
	-- Searches for rig configuration files (*.conf), reads them and stores configurations in et_project.rigs.
	-- Searches for module files (*.mod), reads them and stores modules in et_project.modules.
	procedure open_project (
		project_name 	: in type_project_name.bounded_string; -- blood_sample_analyzer
		log_threshold 	: in et_string_processing.type_log_level)
		is separate;
		
end et_project.rigs;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
