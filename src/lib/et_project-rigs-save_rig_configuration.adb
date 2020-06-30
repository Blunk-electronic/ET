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

separate (et_project.rigs)

procedure save_rig_configuration (
	project_name	: in type_project_name.bounded_string;		-- blood_sample_analyzer
	rig_conf_name	: in type_rig_configuration_file_name.bounded_string; -- demo, low_cost, fully_equipped
	rig				: in type_rig; -- the actual rig configuration				
	project_path	: in type_et_project_path.bounded_string; 	-- /home/user/et_projects
	log_threshold 	: in et_string_processing.type_log_level) is

	use ada.directories;
	use et_string_processing;
	use type_module_instances;
	use type_module_connectors;
	
	file_name : type_rig_configuration_file_name.bounded_string; -- the final full file name
	file_handle : ada.text_io.file_type;

	package type_path is new generic_bounded_length (project_name_max + project_path_max + 1); -- incl. directory separator
	use type_path;
	path : type_path.bounded_string := to_bounded_string (compose (to_string (project_path), to_string (project_name)));

	procedure query_instance (instance_cursor : in type_module_instances.cursor) is
	begin
		section_mark (section_module, HEADER);			
		write (keyword => keyword_generic_name, parameters => to_string (element (instance_cursor).generic_name));
		write (keyword => keyword_instance_name, parameters => et_general.to_string (key (instance_cursor)));
		section_mark (section_module, FOOTER);
	end;

	procedure query_connections (connection_cursor : in type_module_connectors.cursor) is
		con : type_connector := element (connection_cursor);
	begin
		section_mark (section_connector, HEADER);
		write (keyword => keyword_instance_A, parameters => to_string (con.instance_A));
		write (keyword => keyword_purpose_A, wrap => true, parameters => et_devices.to_string (con.purpose_A));
		new_line;
		write (keyword => keyword_instance_B, parameters => to_string (con.instance_B));
		write (keyword => keyword_purpose_B, wrap => true, parameters => et_devices.to_string (con.purpose_B));

		-- CS: net comparator, warnings
		
		section_mark (section_connector, FOOTER);
	end;
	
begin -- save_rig_configuration
	log (text => "saving rig configuration ...", level => log_threshold);
	reset_tab_depth;
	log_indentation_up;

	-- compose the full file name
	file_name := type_rig_configuration_file_name.to_bounded_string (compose (
		containing_directory	=> to_string (path),
		name 					=> to_string (rig_conf_name),
		extension 				=> rig_configuration_file_extension));

	-- create the file
	create (
		file => file_handle,
		mode => out_file, 
		name => to_string (file_name));
	
	set_output (file_handle);
	write_rig_configuration_header;		

	-- section module instances
	section_mark (section_module_instances, HEADER);
	iterate (rig.module_instances, query_instance'access);
	-- CS In the future, write other things here that characterize the instance.
	section_mark (section_module_instances, FOOTER);

	-- section connectors
	new_line;
	section_mark (section_module_connections, HEADER);
	iterate (rig.connections, query_connections'access);
	-- CS In the future, write other things here that characterize the board to board connection
	section_mark (section_module_connections, FOOTER);

	-- close the file
	write_rig_configuration_footer;
	set_output (standard_output);
	close (file_handle);
	
	log_indentation_down;

	exception when event:
		others => 
			log (text => ada.exceptions.exception_message (event), console => true);
			close (file_handle);
			raise;

end save_rig_configuration;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
